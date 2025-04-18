{-# LANGUAGE OverloadedStrings #-}

module Database.Account
  ( getAccountById
  , getAllAccounts
  , updateBalance
  , setFraudFlag
  ) where

import           Database.PostgreSQL.Simple
import           Types

-- | Fetch an account by its UUID.
getAccountById :: Connection -> AccountId -> IO (Maybe Account)
getAccountById conn (AccountId uid) = do
  rows <- query conn
    "SELECT account_id, name, email, balance, currency, \
    \       status, verified, fraud_flagged, \
    \       daily_limit, monthly_limit, per_tx_limit, \
    \       country_code, created_at, updated_at \
    \FROM accounts WHERE account_id = ?"
    (Only uid)
  pure $ case rows of
    [acct] -> Just acct
    _      -> Nothing

-- | Fetch all accounts.
getAllAccounts :: Connection -> IO [Account]
getAllAccounts conn =
  query_ conn
    "SELECT account_id, name, email, balance, currency, \
    \       status, verified, fraud_flagged, \
    \       daily_limit, monthly_limit, per_tx_limit, \
    \       country_code, created_at, updated_at \
    \FROM accounts ORDER BY created_at"

-- | Atomically adjust an account's balance by a delta.
--   Returns the updated account (Nothing if not found).
updateBalance :: Connection -> AccountId -> Double -> IO (Maybe Account)
updateBalance conn (AccountId uid) delta = do
  rows <- query conn
    "UPDATE accounts SET balance = balance + ? \
    \WHERE account_id = ? \
    \RETURNING account_id, name, email, balance, currency, \
    \          status, verified, fraud_flagged, \
    \          daily_limit, monthly_limit, per_tx_limit, \
    \          country_code, created_at, updated_at"
    (delta, uid)
  pure $ case rows of
    [acct] -> Just acct
    _      -> Nothing

-- | Set or clear the fraud flag on an account.
setFraudFlag :: Connection -> AccountId -> Bool -> IO ()
setFraudFlag conn (AccountId uid) flagged' =
  execute conn
    "UPDATE accounts SET fraud_flagged = ? WHERE account_id = ?"
    (flagged', uid)
  >> pure ()
