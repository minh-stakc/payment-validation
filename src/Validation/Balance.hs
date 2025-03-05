{-# LANGUAGE OverloadedStrings #-}

module Validation.Balance
  ( balanceValidator
  , sufficientFunds
  , currencyMatch
  ) where

import           Types
import           Validation.Core

-- | Combined balance validator: currency must match and funds must suffice.
balanceValidator :: Validator
balanceValidator = currencyMatch <> sufficientFunds

-- | The payment currency must match the sender's account currency.
currencyMatch :: Validator
currencyMatch = Validator $ \acct pay ->
  if accountCurrency acct == payCurrency pay
    then Valid
    else validationFailure $ CurrencyMismatch
      { expected = accountCurrency acct
      , got      = payCurrency pay
      }

-- | The sender must have enough balance to cover the payment.
sufficientFunds :: Validator
sufficientFunds = Validator $ \acct pay ->
  let bal = accountBalance acct
      amt = payAmount pay
  in if bal >= amt
       then Valid
       else validationFailure $ InsufficientBalance
         { required  = amt
         , available = bal
         }
