{-# LANGUAGE OverloadedStrings #-}

module Validation.Fraud
  ( fraudValidator
  , notFraudFlagged
  , countryNotBlocked
  , velocityCheck
  , anomalyCheck
    -- * Configurable constructors
  , mkFraudValidator
  , FraudConfig(..)
  , defaultFraudConfig
  ) where

import           Data.Text (Text)
import           Types
import           Validation.Core

-- | Configuration for fraud detection thresholds.
data FraudConfig = FraudConfig
  { fcBlockedCountries  :: [Text]
  , fcVelocityWindow    :: Int       -- ^ minutes
  , fcVelocityMax       :: Int       -- ^ max transactions in window
  , fcAnomalyMultiplier :: Double    -- ^ flag if amount > avg * multiplier
  } deriving (Eq, Show)

defaultFraudConfig :: FraudConfig
defaultFraudConfig = FraudConfig
  { fcBlockedCountries  = ["KP", "IR", "SY"]
  , fcVelocityWindow    = 10
  , fcVelocityMax       = 5
  , fcAnomalyMultiplier = 10.0
  }

-- | Build a fraud validator from config.
--   Velocity and anomaly checks require additional context (recent tx count,
--   average amount) that is injected from the IO layer before calling.
mkFraudValidator :: FraudConfig -> Validator
mkFraudValidator cfg =
  notFraudFlagged <> countryNotBlocked (fcBlockedCountries cfg)

-- | Default fraud validator using hard-coded config (pure, no IO context).
fraudValidator :: Validator
fraudValidator = mkFraudValidator defaultFraudConfig

-- | The account must not be flagged for fraud.
notFraudFlagged :: Validator
notFraudFlagged = ensure (\acct _ -> not (fraudFlagged acct)) AccountFraudFlagged

-- | The sender's country must not be on the blocked list.
countryNotBlocked :: [Text] -> Validator
countryNotBlocked blocked = Validator $ \acct _pay ->
  let cc = countryCode acct
  in if cc `elem` blocked
       then validationFailure (BlockedCountry cc)
       else Valid

-- | Velocity check: given the number of recent transactions in the window,
--   fail if over the threshold.  This is a pure validator that receives
--   pre-fetched data.
velocityCheck :: Int -> Int -> Int -> Validator
velocityCheck windowMinutes maxTx recentTxCount =
  Validator $ \_ _ ->
    if recentTxCount >= maxTx
      then validationFailure (SuspiciousVelocity recentTxCount windowMinutes)
      else Valid

-- | Anomaly check: given the historical average transaction amount,
--   flag if the current amount is anomalously large.
anomalyCheck :: Double -> Double -> Validator
anomalyCheck multiplier avgAmt =
  Validator $ \_ pay ->
    let amt = payAmount pay
    in if avgAmt > 0 && amt > avgAmt * multiplier
         then validationFailure (AnomalousAmount avgAmt amt)
         else Valid
