{-# LANGUAGE OverloadedStrings #-}

module ValidationSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Time.Clock             (UTCTime(..), secondsToDiffTime)
import           Data.Time.Calendar          (fromGregorian)
import           Data.UUID                   (nil, fromWords)

import           Types
import           Validation.Core
import           Validation.Balance
import           Validation.Fraud
import           Validation.Limits
import           Validation.Account
import           Validation.Pipeline

--------------------------------------------------------------------------------
-- Test fixtures
--------------------------------------------------------------------------------

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 1 15) (secondsToDiffTime 43200)

-- | A healthy, active, verified account with plenty of balance.
goodAccount :: Account
goodAccount = Account
  { accountId       = AccountId nil
  , accountName     = "Test User"
  , accountEmail    = "test@example.com"
  , accountBalance  = 10000.0
  , accountCurrency = USD
  , accountStatus   = Active
  , accountVerified = True
  , fraudFlagged    = False
  , dailyLimit      = 10000.0
  , monthlyLimit    = 100000.0
  , perTxLimit      = 5000.0
  , countryCode     = "US"
  , createdAt       = fixedTime
  , updatedAt       = fixedTime
  }

-- | A distinct account ID for the recipient (not nil, to avoid self-transfer).
otherAccountId :: AccountId
otherAccountId = AccountId (fromWords 1 2 3 4)

mkPayment :: Double -> Currency -> PaymentRequest
mkPayment amt cur = PaymentRequest
  { fromAccount = accountId goodAccount
  , toAccount   = otherAccountId
  , payAmount   = amt
  , payCurrency = cur
  , payDescription = "test payment"
  }

goodPayment :: PaymentRequest
goodPayment = mkPayment 500.0 USD

--------------------------------------------------------------------------------
-- Specs
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Core combinators" $ do
    it "mempty validator always succeeds" $
      runValidator mempty goodAccount goodPayment `shouldBe` Valid

    it "combine merges errors from both validators" $ do
      let v1 = ensure (\_ _ -> False) NegativeAmount
          v2 = ensure (\_ _ -> False) ZeroAmount
          combined = v1 <> v2
          result = runValidator combined goodAccount goodPayment
      result `shouldBe` Invalid [NegativeAmount, ZeroAmount]

    it "combineAll with all-passing yields Valid" $ do
      let vs = replicate 5 (ensure (\_ _ -> True) NegativeAmount)
      runValidator (combineAll vs) goodAccount goodPayment `shouldBe` Valid

    it "when' skips validator when predicate is false" $ do
      let v = when' (\_ _ -> False) (ensure (\_ _ -> False) NegativeAmount)
      runValidator v goodAccount goodPayment `shouldBe` Valid

    it "when' applies validator when predicate is true" $ do
      let v = when' (\_ _ -> True) (ensure (\_ _ -> False) NegativeAmount)
      runValidator v goodAccount goodPayment `shouldBe` Invalid [NegativeAmount]

  describe "Balance validation" $ do
    it "passes when balance >= amount" $
      runValidator balanceValidator goodAccount goodPayment `shouldBe` Valid

    it "fails with InsufficientBalance when amount > balance" $ do
      let pay = mkPayment 99999.0 USD
          result = runValidator sufficientFunds goodAccount pay
      isValid result `shouldBe` False
      getErrors result `shouldBe` [InsufficientBalance 99999.0 10000.0]

    it "fails with CurrencyMismatch when currencies differ" $ do
      let pay = mkPayment 100.0 EUR
          result = runValidator currencyMatch goodAccount pay
      isValid result `shouldBe` False
      case getErrors result of
        [CurrencyMismatch ex g] -> do
          ex `shouldBe` USD
          g  `shouldBe` EUR
        _ -> expectationFailure "Expected CurrencyMismatch error"

  describe "Fraud validation" $ do
    it "passes for clean account" $
      runValidator fraudValidator goodAccount goodPayment `shouldBe` Valid

    it "fails when account is fraud-flagged" $ do
      let flagged = goodAccount { fraudFlagged = True }
          result = runValidator notFraudFlagged flagged goodPayment
      getErrors result `shouldBe` [AccountFraudFlagged]

    it "fails when country is blocked" $ do
      let blocked = goodAccount { countryCode = "KP" }
          result = runValidator fraudValidator blocked goodPayment
      getErrors result `shouldBe` [BlockedCountry "KP"]

    it "velocity check fails when over threshold" $ do
      let v = velocityCheck 10 5 6
          result = runValidator v goodAccount goodPayment
      getErrors result `shouldBe` [SuspiciousVelocity 6 10]

    it "velocity check passes when under threshold" $ do
      let v = velocityCheck 10 5 3
      runValidator v goodAccount goodPayment `shouldBe` Valid

    it "anomaly check flags large deviation" $ do
      let v = anomalyCheck 10.0 100.0  -- avg=100, multiplier=10
          pay = mkPayment 1500.0 USD   -- 1500 > 100*10
          result = runValidator v goodAccount pay
      isValid result `shouldBe` False

    it "anomaly check passes for normal amount" $ do
      let v = anomalyCheck 10.0 100.0
          pay = mkPayment 500.0 USD    -- 500 < 100*10
      runValidator v goodAccount pay `shouldBe` Valid

  describe "Limits validation" $ do
    it "passes when amount is within per-tx limit" $ do
      let pay = mkPayment 4999.0 USD
      runValidator perTransactionLimit goodAccount pay `shouldBe` Valid

    it "fails when amount exceeds per-tx limit" $ do
      let pay = mkPayment 6000.0 USD
          result = runValidator perTransactionLimit goodAccount pay
      isValid result `shouldBe` False
      case getErrors result of
        [ExceedsPerTxLimit lim amt] -> do
          lim `shouldBe` 5000.0
          amt `shouldBe` 6000.0
        _ -> expectationFailure "Expected ExceedsPerTxLimit"

    it "daily limit check accumulates with existing usage" $ do
      let v = dailyLimitCheck 9000.0  -- already used 9000 today
          pay = mkPayment 2000.0 USD  -- 9000 + 2000 > 10000
          result = runValidator v goodAccount pay
      isValid result `shouldBe` False

    it "monthly limit check passes when under" $ do
      let v = monthlyLimitCheck 50000.0
          pay = mkPayment 2000.0 USD
      runValidator v goodAccount pay `shouldBe` Valid

  describe "Account validation" $ do
    it "passes for active, verified account" $
      runValidator accountValidator goodAccount goodPayment `shouldBe` Valid

    it "fails for suspended account" $ do
      let suspended = goodAccount { accountStatus = Suspended }
          result = runValidator accountActiveCheck suspended goodPayment
      getErrors result `shouldBe` [AccountNotActive Suspended]

    it "fails for unverified account" $ do
      let unverified = goodAccount { accountVerified = False }
          result = runValidator accountVerifiedCheck unverified goodPayment
      getErrors result `shouldBe` [AccountNotVerified]

    it "accumulates both errors for suspended + unverified" $ do
      let bad = goodAccount { accountStatus = Suspended, accountVerified = False }
          result = runValidator accountValidator bad goodPayment
      length (getErrors result) `shouldBe` 2

  describe "Pipeline" $ do
    it "pure pipeline passes for a fully valid payment" $
      validatePayment goodAccount goodPayment `shouldBe` Valid

    it "pure pipeline accumulates multiple errors" $ do
      let bad = goodAccount
            { accountStatus   = Closed
            , accountVerified = False
            , fraudFlagged    = True
            , accountBalance  = 0
            }
          pay = mkPayment 6000.0 USD
          result = validatePayment bad pay
      isValid result `shouldBe` False
      -- Should have errors from account, fraud, balance, and limits
      length (getErrors result) `shouldSatisfy` (>= 4)

    it "self-transfer is rejected" $ do
      let pay = goodPayment { toAccount = accountId goodAccount }
          result = validatePayment goodAccount pay
      getErrors result `shouldBe` [SelfTransfer]

    it "negative amount is rejected" $ do
      let pay = mkPayment (-100.0) USD
          result = runValidator nonNegativeAmount goodAccount pay
      getErrors result `shouldBe` [NegativeAmount]

    it "zero amount is rejected" $ do
      let pay = mkPayment 0.0 USD
          result = runValidator nonZeroAmount goodAccount pay
      getErrors result `shouldBe` [ZeroAmount]

  describe "ValidationResult monoid laws (QuickCheck)" $ do
    it "mempty <> x == x" $ property $ \n ->
      let x = if n >= (0 :: Int) then Valid else Invalid [NegativeAmount]
      in (mempty <> x) == x

    it "x <> mempty == x" $ property $ \n ->
      let x = if n >= (0 :: Int) then Valid else Invalid [NegativeAmount]
      in (x <> mempty) == x

    it "associativity: (a <> b) <> c == a <> (b <> c)" $ property $ \(a', b', c') ->
      let toVR n = if (n :: Int) >= 0 then Valid else Invalid [NegativeAmount]
          a = toVR a'; b = toVR b'; c = toVR c'
      in ((a <> b) <> c) == (a <> (b <> c))
