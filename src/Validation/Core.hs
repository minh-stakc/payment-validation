{-# LANGUAGE OverloadedStrings #-}

module Validation.Core
  ( -- * Validator type
    Validator(..)
  , mkValidator
  , runValidator
    -- * Combinators
  , combine
  , combineAll
  , when'
  , unless'
  , withContext
    -- * Lifting helpers
  , ensure
  , ensureNot
  , check
    -- * Common validators
  , nonNegativeAmount
  , nonZeroAmount
  , notSelfTransfer
  ) where

import           Types

-- | A Validator is a pure function from an Account and a Payment request
--   to a ValidationResult.  Validators compose monoidal: combining two
--   validators runs both and merges their results, accumulating all errors.
newtype Validator = Validator { runValidator :: Account -> PaymentRequest -> ValidationResult }

-- | Construct a validator from a predicate function.
mkValidator :: (Account -> PaymentRequest -> ValidationResult) -> Validator
mkValidator = Validator

-- | Validators form a semigroup: run both, merge results.
instance Semigroup Validator where
  (Validator f) <> (Validator g) = Validator $ \acct pay ->
    f acct pay <> g acct pay

-- | The identity validator always succeeds.
instance Monoid Validator where
  mempty = Validator $ \_ _ -> Valid

-- | Combine two validators (alias for <>).
combine :: Validator -> Validator -> Validator
combine = (<>)

-- | Combine a list of validators into one.
combineAll :: [Validator] -> Validator
combineAll = mconcat

-- | Conditionally apply a validator.
when' :: (Account -> PaymentRequest -> Bool) -> Validator -> Validator
when' predicate (Validator v) = Validator $ \acct pay ->
  if predicate acct pay
    then v acct pay
    else Valid

-- | Apply a validator unless the predicate holds.
unless' :: (Account -> PaymentRequest -> Bool) -> Validator -> Validator
unless' predicate = when' (\a p -> not (predicate a p))

-- | Wrap a validator so its errors carry extra context (no-op on Valid).
--   This is a structural combinator -- useful for logging/tracing.
withContext :: String -> Validator -> Validator
withContext _ctx v = v  -- ValidationError is already structured; extend if needed

-- | Ensure a predicate holds; if not, produce the given error.
ensure :: (Account -> PaymentRequest -> Bool) -> ValidationError -> Validator
ensure predicate err = Validator $ \acct pay ->
  if predicate acct pay
    then Valid
    else validationFailure err

-- | Ensure a predicate does NOT hold.
ensureNot :: (Account -> PaymentRequest -> Bool) -> ValidationError -> Validator
ensureNot predicate = ensure (\a p -> not (predicate a p))

-- | Run an arbitrary check that may produce zero or more errors.
check :: (Account -> PaymentRequest -> [ValidationError]) -> Validator
check f = Validator $ \acct pay ->
  case f acct pay of
    [] -> Valid
    es -> Invalid es

--------------------------------------------------------------------------------
-- Common validators
--------------------------------------------------------------------------------

-- | Payment amount must be non-negative.
nonNegativeAmount :: Validator
nonNegativeAmount = ensure (\_ p -> payAmount p >= 0) NegativeAmount

-- | Payment amount must not be zero.
nonZeroAmount :: Validator
nonZeroAmount = ensure (\_ p -> payAmount p > 0) ZeroAmount

-- | Sender and receiver must differ.
notSelfTransfer :: Validator
notSelfTransfer = ensure (\_ p -> fromAccount p /= toAccount p) SelfTransfer
