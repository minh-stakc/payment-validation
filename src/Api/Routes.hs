{-# LANGUAGE OverloadedStrings #-}

module Api.Routes
  ( app
  ) where

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty

import           Types                        (AppConfig(..))
import           Database.Connection          (ConnectionPool)
import           Api.Handlers

-- | Build the Scotty application with all routes.
app :: ConnectionPool -> AppConfig -> ScottyM ()
app pool cfg = do
  -- Middleware: request/response logging
  middleware logStdoutDev

  -- Health check
  get "/health" healthCheck

  -- Dry-run validation (no side effects)
  post "/api/validate" (validatePaymentHandler pool cfg)

  -- Submit and execute a payment
  post "/api/payments" (submitPaymentHandler pool cfg)

  -- Account lookup
  get "/api/accounts/:id" (getAccountHandler pool)

  -- Transaction history for an account
  get "/api/accounts/:id/transactions" (getTransactionsHandler pool)
