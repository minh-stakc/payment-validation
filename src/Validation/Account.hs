{-# LANGUAGE OverloadedStrings #-}

module Validation.Account
  ( accountValidator
  , accountActiveCheck
  , accountVerifiedCheck
  ) where

import           Types
import           Validation.Core

-- | Combined account verification validator.
accountValidator :: Validator
accountValidator = accountActiveCheck <> accountVerifiedCheck

-- | The account must be in Active status.
accountActiveCheck :: Validator
accountActiveCheck = Validator $ \acct _ ->
  case accountStatus acct of
    Active -> Valid
    s      -> validationFailure (AccountNotActive s)

-- | The account must have completed verification.
accountVerifiedCheck :: Validator
accountVerifiedCheck = ensure (\acct _ -> accountVerified acct) AccountNotVerified
