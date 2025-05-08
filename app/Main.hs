{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           System.Environment       (lookupEnv)
import qualified Data.Text                as T
import           Web.Scotty               (scotty)

import           Types                    (AppConfig(..), defaultConfig)
import           Database.Connection      (createPool', defaultConnStr)
import           Api.Routes               (app)

main :: IO ()
main = do
  putStrLn "=== Functional Payment Validation Engine ==="

  -- Read configuration from environment, falling back to defaults
  connStr <- defaultConnStr
  port    <- maybe 3000 read <$> lookupEnv "PORT"
  blocked <- maybe (blockedCountries defaultConfig) (map T.strip . T.splitOn "," . T.pack)
               <$> lookupEnv "BLOCKED_COUNTRIES"

  let cfg = defaultConfig
        { dbConnectionString = T.pack (show connStr)
        , serverPort         = port
        , blockedCountries   = blocked
        }

  putStrLn $ "Connecting to database..."
  pool <- createPool' connStr
  putStrLn $ "Database pool created."

  putStrLn $ "Starting server on port " ++ show port ++ "..."
  scotty port (app pool cfg)
