{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Api.Handlers
  ( healthCheck
  , validatePaymentHandler
  , submitPaymentHandler
  , getAccountHandler
  , getTransactionsHandler
  ) where

import           Control.Exception            (try, SomeException)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Aeson                   (object, (.=))
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Data.UUID                    (fromText)
import           Data.UUID.V4                 (nextRandom)
import           Network.HTTP.Types.Status
import           Web.Scotty

import           Types
import           Database.Connection          (ConnectionPool, withPool)
import qualified Database.Account             as DbAcct
import qualified Database.Transaction         as DbTx
import           Validation.Pipeline
import           Validation.Fraud             (FraudConfig(..))
import           Database.PostgreSQL.Simple   (Connection)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

parseAccountId :: TL.Text -> Maybe AccountId
parseAccountId t = AccountId <$> fromText (TL.toStrict t)

withAccount :: ConnectionPool -> TL.Text -> (Account -> ActionM ()) -> ActionM ()
withAccount pool rawId action = do
  case parseAccountId rawId of
    Nothing -> do
      status badRequest400
      json $ object ["error" .= ("Invalid account ID format" :: T.Text)]
    Just aid -> do
      mAcct <- liftIO $ withPool pool $ \conn -> DbAcct.getAccountById conn aid
      case mAcct of
        Nothing -> do
          status notFound404
          json $ object ["error" .= ("Account not found" :: T.Text)]
        Just acct -> action acct

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | GET /health
healthCheck :: ActionM ()
healthCheck = json $ object ["status" .= ("ok" :: T.Text)]

-- | POST /api/validate -- dry-run validation, no side effects
validatePaymentHandler :: ConnectionPool -> AppConfig -> ActionM ()
validatePaymentHandler pool cfg = do
  req <- jsonData :: ActionM PaymentRequest
  result <- liftIO $ withPool pool $ \conn -> do
    mAcct <- DbAcct.getAccountById conn (fromAccount req)
    case mAcct of
      Nothing -> pure $ validationFailure (AccountNotActive Closed)
      Just acct -> do
        ctx <- buildContext conn acct cfg
        pure $ validatePaymentWithContext ctx acct req
  if isValid result
    then json $ object ["valid" .= True, "errors" .= ([] :: [ValidationError])]
    else do
      status badRequest400
      json result

-- | POST /api/payments -- validate and execute
submitPaymentHandler :: ConnectionPool -> AppConfig -> ActionM ()
submitPaymentHandler pool cfg = do
  req <- jsonData :: ActionM PaymentRequest
  result <- liftIO $ withPool pool $ \conn -> do
    mAcct <- DbAcct.getAccountById conn (fromAccount req)
    case mAcct of
      Nothing -> pure $ Left "Sender account not found"
      Just acct -> do
        ctx <- buildContext conn acct cfg
        let vResult = validatePaymentWithContext ctx acct req
        if not (isValid vResult)
          then pure $ Left $ T.pack $ "Validation failed: " ++ show (getErrors vResult)
          else do
            txId <- TransactionId <$> nextRandom
            -- Execute the transfer atomically-ish
            eTx <- try $ do
              _ <- DbAcct.updateBalance conn (fromAccount req) (negate (payAmount req))
              _ <- DbAcct.updateBalance conn (toAccount req) (payAmount req)
              mTx <- DbTx.insertTransaction conn txId
                (fromAccount req) (toAccount req)
                (payAmount req) (payCurrency req)
                Completed (T.unpack (payDescription req))
              case mTx of
                Just tx -> pure tx
                Nothing -> error "Failed to insert transaction"
            case eTx of
              Left (e :: SomeException) -> pure $ Left (T.pack $ show e)
              Right tx -> pure $ Right tx

  case result of
    Left err -> do
      status badRequest400
      json $ object ["error" .= err]
    Right tx -> do
      status created201
      json tx

-- | GET /api/accounts/:id
getAccountHandler :: ConnectionPool -> ActionM ()
getAccountHandler pool = do
  rawId <- captureParam "id"
  withAccount pool rawId $ \acct -> json acct

-- | GET /api/accounts/:id/transactions
getTransactionsHandler :: ConnectionPool -> ActionM ()
getTransactionsHandler pool = do
  rawId <- captureParam "id"
  withAccount pool rawId $ \acct -> do
    txs <- liftIO $ withPool pool $ \conn ->
      DbTx.getTransactionsByAccount conn (accountId acct)
    json txs

--------------------------------------------------------------------------------
-- Build validation context from DB
--------------------------------------------------------------------------------

buildContext :: Connection -> Account -> AppConfig -> IO ValidationContext
buildContext conn acct cfg = do
  daily    <- DbTx.getDailyTotal conn (accountId acct)
  monthly  <- DbTx.getMonthlyTotal conn (accountId acct)
  recent   <- DbTx.getRecentTransactionCount conn (accountId acct) (velocityWindow cfg)
  avgAmt   <- DbTx.getAverageTransactionAmount conn (accountId acct)
  let fc = FraudConfig
        { fcBlockedCountries  = blockedCountries cfg
        , fcVelocityWindow    = velocityWindow cfg
        , fcVelocityMax       = velocityMax cfg
        , fcAnomalyMultiplier = anomalyMultiplier cfg
        }
  pure ValidationContext
    { ctxDailyUsed     = daily
    , ctxMonthlyUsed   = monthly
    , ctxRecentTxCount = recent
    , ctxAvgTxAmount   = avgAmt
    , ctxFraudConfig   = fc
    }
