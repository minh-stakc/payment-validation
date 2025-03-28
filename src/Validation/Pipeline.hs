{-# LANGUAGE OverloadedStrings #-}

module Validation.Pipeline
  ( -- * Pure pipeline (no IO context)
    purePipeline
    -- * Full pipeline with IO-derived context
  , fullPipeline
  , ValidationContext(..)
    -- * Running
  , validatePayment
  , validatePaymentWithContext
  ) where

import           Types
import           Validation.Core
import           Validation.Balance  (balanceValidator)
import           Validation.Fraud    (fraudValidator, velocityCheck, anomalyCheck, FraudConfig(..))
import           Validation.Limits   (mkLimitsValidator, perTransactionLimit)
import           Validation.Account  (accountValidator)

-- | Context fetched from the database before running the full pipeline.
data ValidationContext = ValidationContext
  { ctxDailyUsed     :: Double   -- ^ sum of today's completed tx amounts
  , ctxMonthlyUsed   :: Double   -- ^ sum of this month's completed tx amounts
  , ctxRecentTxCount :: Int      -- ^ number of tx in the velocity window
  , ctxAvgTxAmount   :: Double   -- ^ historical average tx amount for sender
  , ctxFraudConfig   :: FraudConfig
  } deriving (Eq, Show)

-- | The pure pipeline: runs all validators that need no IO context.
--   Suitable for unit tests and quick dry-run checks.
purePipeline :: Validator
purePipeline = combineAll
  [ nonNegativeAmount
  , nonZeroAmount
  , notSelfTransfer
  , accountValidator
  , balanceValidator
  , fraudValidator       -- blocked-country + fraud-flag (no IO)
  , perTransactionLimit
  ]

-- | Full pipeline incorporating IO-derived context (daily totals, velocity, etc.).
fullPipeline :: ValidationContext -> Validator
fullPipeline ctx = combineAll
  [ nonNegativeAmount
  , nonZeroAmount
  , notSelfTransfer
  , accountValidator
  , balanceValidator
  , fraudValidator
  , velocityCheck
      (fcVelocityWindow (ctxFraudConfig ctx))
      (fcVelocityMax    (ctxFraudConfig ctx))
      (ctxRecentTxCount ctx)
  , anomalyCheck
      (fcAnomalyMultiplier (ctxFraudConfig ctx))
      (ctxAvgTxAmount ctx)
  , mkLimitsValidator (ctxDailyUsed ctx) (ctxMonthlyUsed ctx)
  ]

-- | Run the pure pipeline against an account and payment.
validatePayment :: Account -> PaymentRequest -> ValidationResult
validatePayment = runValidator purePipeline

-- | Run the full pipeline with IO-derived context.
validatePaymentWithContext :: ValidationContext -> Account -> PaymentRequest -> ValidationResult
validatePaymentWithContext ctx = runValidator (fullPipeline ctx)
