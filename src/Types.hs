{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( -- * Identifiers
    AccountId(..)
  , TransactionId(..)
    -- * Currency
  , Currency(..)
    -- * Money
  , Money(..)
  , mkMoney
    -- * Account
  , Account(..)
  , AccountStatus(..)
    -- * Payment
  , Payment(..)
  , PaymentRequest(..)
    -- * Transaction
  , Transaction(..)
  , TransactionStatus(..)
    -- * Validation
  , ValidationError(..)
  , ValidationResult(..)
  , validationSuccess
  , validationFailure
  , isValid
  , getErrors
    -- * Config
  , AppConfig(..)
  , defaultConfig
  ) where

import           Data.Aeson
import           Data.Text                          (Text)
import           Data.Time                          (UTCTime)
import           Data.UUID                          (UUID)
import qualified Data.UUID                          as UUID
import           Database.PostgreSQL.Simple.FromField
                   (FromField(..), fromField, returnError, ResultError(..))
import           Database.PostgreSQL.Simple.ToField (ToField(..))
import           Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import           GHC.Generics                       (Generic)

--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

newtype AccountId = AccountId { unAccountId :: UUID }
  deriving (Eq, Ord, Show, Generic, FromField, ToField)

instance FromJSON AccountId where
  parseJSON = withText "AccountId" $ \t ->
    case UUID.fromText t of
      Just u  -> pure (AccountId u)
      Nothing -> fail "Invalid UUID for AccountId"

instance ToJSON AccountId where
  toJSON (AccountId u) = String (UUID.toText u)

newtype TransactionId = TransactionId { unTransactionId :: UUID }
  deriving (Eq, Ord, Show, Generic, FromField, ToField)

instance FromJSON TransactionId where
  parseJSON = withText "TransactionId" $ \t ->
    case UUID.fromText t of
      Just u  -> pure (TransactionId u)
      Nothing -> fail "Invalid UUID for TransactionId"

instance ToJSON TransactionId where
  toJSON (TransactionId u) = String (UUID.toText u)

--------------------------------------------------------------------------------
-- Currency
--------------------------------------------------------------------------------

data Currency
  = USD
  | EUR
  | GBP
  | JPY
  | CHF
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance FromJSON Currency
instance ToJSON Currency

instance FromField Currency where
  fromField f mbs = do
    t <- fromField f mbs
    case (t :: Text) of
      "USD" -> pure USD
      "EUR" -> pure EUR
      "GBP" -> pure GBP
      "JPY" -> pure JPY
      "CHF" -> pure CHF
      _     -> returnError ConversionFailed f "Unknown currency"

instance ToField Currency where
  toField = toField . show

--------------------------------------------------------------------------------
-- Money
--------------------------------------------------------------------------------

data Money = Money
  { amount   :: !Double
  , currency :: !Currency
  } deriving (Eq, Show, Generic)

instance FromJSON Money
instance ToJSON Money

-- | Smart constructor: money amounts must be non-negative.
mkMoney :: Double -> Currency -> Maybe Money
mkMoney amt cur
  | amt >= 0  = Just (Money amt cur)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Account
--------------------------------------------------------------------------------

data AccountStatus
  = Active
  | Suspended
  | Closed
  | PendingVerification
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON AccountStatus
instance ToJSON AccountStatus

instance FromField AccountStatus where
  fromField f mbs = do
    t <- fromField f mbs
    case (t :: Text) of
      "Active"              -> pure Active
      "Suspended"           -> pure Suspended
      "Closed"              -> pure Closed
      "PendingVerification" -> pure PendingVerification
      _                     -> returnError ConversionFailed f "Unknown account status"

instance ToField AccountStatus where
  toField = toField . show

data Account = Account
  { accountId       :: !AccountId
  , accountName     :: !Text
  , accountEmail    :: !Text
  , accountBalance  :: !Double
  , accountCurrency :: !Currency
  , accountStatus   :: !AccountStatus
  , accountVerified :: !Bool
  , fraudFlagged    :: !Bool
  , dailyLimit      :: !Double
  , monthlyLimit    :: !Double
  , perTxLimit      :: !Double
  , countryCode     :: !Text
  , createdAt       :: !UTCTime
  , updatedAt       :: !UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow Account where
  fromRow = Account
    <$> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field

instance ToJSON Account where
  toJSON acct = object
    [ "account_id"     .= accountId acct
    , "name"           .= accountName acct
    , "email"          .= accountEmail acct
    , "balance"        .= accountBalance acct
    , "currency"       .= accountCurrency acct
    , "status"         .= accountStatus acct
    , "verified"       .= accountVerified acct
    , "fraud_flagged"  .= fraudFlagged acct
    , "daily_limit"    .= dailyLimit acct
    , "monthly_limit"  .= monthlyLimit acct
    , "per_tx_limit"   .= perTxLimit acct
    , "country_code"   .= countryCode acct
    , "created_at"     .= createdAt acct
    , "updated_at"     .= updatedAt acct
    ]

--------------------------------------------------------------------------------
-- Payment
--------------------------------------------------------------------------------

data PaymentRequest = PaymentRequest
  { fromAccount :: !AccountId
  , toAccount   :: !AccountId
  , payAmount   :: !Double
  , payCurrency :: !Currency
  , payDescription :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON PaymentRequest where
  parseJSON = withObject "PaymentRequest" $ \v -> PaymentRequest
    <$> v .: "from_account"
    <*> v .: "to_account"
    <*> v .: "amount"
    <*> v .: "currency"
    <*> v .: "description"

instance ToJSON PaymentRequest where
  toJSON pr = object
    [ "from_account" .= fromAccount pr
    , "to_account"   .= toAccount pr
    , "amount"       .= payAmount pr
    , "currency"     .= payCurrency pr
    , "description"  .= payDescription pr
    ]

data Payment = Payment
  { paymentId        :: !TransactionId
  , paymentFrom      :: !AccountId
  , paymentTo        :: !AccountId
  , paymentAmount    :: !Double
  , paymentCurrency  :: !Currency
  , paymentDesc      :: !Text
  , paymentTimestamp :: !UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON Payment where
  toJSON p = object
    [ "payment_id"  .= paymentId p
    , "from"        .= paymentFrom p
    , "to"          .= paymentTo p
    , "amount"      .= paymentAmount p
    , "currency"    .= paymentCurrency p
    , "description" .= paymentDesc p
    , "timestamp"   .= paymentTimestamp p
    ]

--------------------------------------------------------------------------------
-- Transaction
--------------------------------------------------------------------------------

data TransactionStatus
  = Pending
  | Completed
  | Failed
  | Reversed
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON TransactionStatus
instance ToJSON TransactionStatus

instance FromField TransactionStatus where
  fromField f mbs = do
    t <- fromField f mbs
    case (t :: Text) of
      "Pending"   -> pure Pending
      "Completed" -> pure Completed
      "Failed"    -> pure Failed
      "Reversed"  -> pure Reversed
      _           -> returnError ConversionFailed f "Unknown transaction status"

instance ToField TransactionStatus where
  toField = toField . show

data Transaction = Transaction
  { txId        :: !TransactionId
  , txFrom      :: !AccountId
  , txTo        :: !AccountId
  , txAmount    :: !Double
  , txCurrency  :: !Currency
  , txStatus    :: !TransactionStatus
  , txDesc      :: !Text
  , txCreatedAt :: !UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow Transaction where
  fromRow = Transaction
    <$> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field

instance ToJSON Transaction where
  toJSON tx = object
    [ "transaction_id" .= txId tx
    , "from"           .= txFrom tx
    , "to"             .= txTo tx
    , "amount"         .= txAmount tx
    , "currency"       .= txCurrency tx
    , "status"         .= txStatus tx
    , "description"    .= txDesc tx
    , "created_at"     .= txCreatedAt tx
    ]

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Individual validation errors with structured information.
data ValidationError
  = InsufficientBalance   { required :: Double, available :: Double }
  | CurrencyMismatch      { expected :: Currency, got :: Currency }
  | AccountNotActive      { acctStatus :: AccountStatus }
  | AccountNotVerified
  | AccountFraudFlagged
  | ExceedsDailyLimit     { dailyUsed :: Double, dailyMax :: Double, txAmt :: Double }
  | ExceedsMonthlyLimit   { monthlyUsed :: Double, monthlyMax :: Double, txAmt :: Double }
  | ExceedsPerTxLimit     { txLimit :: Double, txAmt :: Double }
  | NegativeAmount
  | ZeroAmount
  | SelfTransfer
  | BlockedCountry        { blocked :: Text }
  | SuspiciousVelocity    { recentCount :: Int, windowMinutes :: Int }
  | AnomalousAmount       { avgAmount :: Double, currentAmount :: Double }
  deriving (Eq, Show, Generic)

instance ToJSON ValidationError where
  toJSON (InsufficientBalance req avail) = object
    [ "error" .= ("insufficient_balance" :: Text)
    , "required" .= req, "available" .= avail ]
  toJSON (CurrencyMismatch ex g) = object
    [ "error" .= ("currency_mismatch" :: Text)
    , "expected" .= show ex, "got" .= show g ]
  toJSON (AccountNotActive s) = object
    [ "error" .= ("account_not_active" :: Text)
    , "status" .= show s ]
  toJSON AccountNotVerified = object
    [ "error" .= ("account_not_verified" :: Text) ]
  toJSON AccountFraudFlagged = object
    [ "error" .= ("account_fraud_flagged" :: Text) ]
  toJSON (ExceedsDailyLimit used mx amt) = object
    [ "error" .= ("exceeds_daily_limit" :: Text)
    , "daily_used" .= used, "daily_max" .= mx, "tx_amount" .= amt ]
  toJSON (ExceedsMonthlyLimit used mx amt) = object
    [ "error" .= ("exceeds_monthly_limit" :: Text)
    , "monthly_used" .= used, "monthly_max" .= mx, "tx_amount" .= amt ]
  toJSON (ExceedsPerTxLimit lim amt) = object
    [ "error" .= ("exceeds_per_tx_limit" :: Text)
    , "limit" .= lim, "tx_amount" .= amt ]
  toJSON NegativeAmount = object [ "error" .= ("negative_amount" :: Text) ]
  toJSON ZeroAmount     = object [ "error" .= ("zero_amount" :: Text) ]
  toJSON SelfTransfer   = object [ "error" .= ("self_transfer" :: Text) ]
  toJSON (BlockedCountry c) = object
    [ "error" .= ("blocked_country" :: Text), "country" .= c ]
  toJSON (SuspiciousVelocity cnt win) = object
    [ "error" .= ("suspicious_velocity" :: Text)
    , "recent_count" .= cnt, "window_minutes" .= win ]
  toJSON (AnomalousAmount avg cur) = object
    [ "error" .= ("anomalous_amount" :: Text)
    , "average" .= avg, "current" .= cur ]

-- | Validation result that accumulates errors (applicative-style).
data ValidationResult
  = Valid
  | Invalid [ValidationError]
  deriving (Eq, Show, Generic)

instance Semigroup ValidationResult where
  Valid       <> Valid       = Valid
  Valid       <> inv         = inv
  inv         <> Valid       = inv
  (Invalid a) <> (Invalid b) = Invalid (a <> b)

instance Monoid ValidationResult where
  mempty = Valid

instance ToJSON ValidationResult where
  toJSON Valid       = object [ "valid" .= True,  "errors" .= ([] :: [ValidationError]) ]
  toJSON (Invalid e) = object [ "valid" .= False, "errors" .= e ]

validationSuccess :: ValidationResult
validationSuccess = Valid

validationFailure :: ValidationError -> ValidationResult
validationFailure e = Invalid [e]

isValid :: ValidationResult -> Bool
isValid Valid = True
isValid _     = False

getErrors :: ValidationResult -> [ValidationError]
getErrors Valid       = []
getErrors (Invalid e) = e

--------------------------------------------------------------------------------
-- App Config
--------------------------------------------------------------------------------

data AppConfig = AppConfig
  { dbConnectionString :: Text
  , serverPort         :: Int
  , blockedCountries   :: [Text]
  , velocityWindow     :: Int    -- ^ minutes
  , velocityMax        :: Int    -- ^ max transactions in window
  , anomalyMultiplier  :: Double -- ^ flag if tx > avg * multiplier
  } deriving (Eq, Show)

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { dbConnectionString = "postgresql://postgres:postgres@localhost:5432/payment_validation"
  , serverPort         = 3000
  , blockedCountries   = ["KP", "IR", "SY"]
  , velocityWindow     = 10
  , velocityMax        = 5
  , anomalyMultiplier  = 10.0
  }
