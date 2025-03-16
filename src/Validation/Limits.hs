{-# LANGUAGE OverloadedStrings #-}

module Validation.Limits
  ( limitsValidator
  , perTransactionLimit
  , dailyLimitCheck
  , monthlyLimitCheck
    -- * With pre-fetched totals
  , mkLimitsValidator
  ) where

import           Types
import           Validation.Core

-- | Per-transaction limit: a single payment must not exceed the account's
--   per-transaction cap.  This is fully pure.
perTransactionLimit :: Validator
perTransactionLimit = Validator $ \acct pay ->
  let lim = perTxLimit acct
      amt = payAmount pay
  in if amt <= lim
       then Valid
       else validationFailure $ ExceedsPerTxLimit
         { txLimit = lim
         , txAmt   = amt
         }

-- | Daily limit check.  Requires the sum of today's completed transactions
--   (pre-fetched from the database).
dailyLimitCheck :: Double -> Validator
dailyLimitCheck dailyUsedSoFar = Validator $ \acct pay ->
  let lim = dailyLimit acct
      amt = payAmount pay
  in if dailyUsedSoFar + amt <= lim
       then Valid
       else validationFailure $ ExceedsDailyLimit
         { dailyUsed = dailyUsedSoFar
         , dailyMax  = lim
         , txAmt     = amt
         }

-- | Monthly limit check.  Requires the sum of this month's completed
--   transactions (pre-fetched from the database).
monthlyLimitCheck :: Double -> Validator
monthlyLimitCheck monthlyUsedSoFar = Validator $ \acct pay ->
  let lim = monthlyLimit acct
      amt = payAmount pay
  in if monthlyUsedSoFar + amt <= lim
       then Valid
       else validationFailure $ ExceedsMonthlyLimit
         { monthlyUsed = monthlyUsedSoFar
         , monthlyMax  = lim
         , txAmt       = amt
         }

-- | Build the full limits validator given pre-fetched daily and monthly totals.
mkLimitsValidator :: Double -> Double -> Validator
mkLimitsValidator dailyUsed monthlyUsed =
  perTransactionLimit
    <> dailyLimitCheck dailyUsed
    <> monthlyLimitCheck monthlyUsed

-- | Default limits validator (only checks per-transaction; daily/monthly
--   require IO context and should use mkLimitsValidator).
limitsValidator :: Validator
limitsValidator = perTransactionLimit
