{-# LANGUAGE OverloadedStrings #-}

module Database.Connection
  ( ConnectionPool
  , createPool'
  , withPool
  , defaultConnStr
  ) where

import           Data.ByteString.Char8       (ByteString, pack)
import           Data.Pool                   (Pool, newPool, defaultPoolConfig,
                                              setNumStripes, withResource)
import           Database.PostgreSQL.Simple   (Connection, connectPostgreSQL, close)
import           System.Environment          (lookupEnv)

type ConnectionPool = Pool Connection

-- | Default connection string, overridden by DATABASE_URL env var.
defaultConnStr :: IO ByteString
defaultConnStr = do
  murl <- lookupEnv "DATABASE_URL"
  pure $ case murl of
    Just url -> pack url
    Nothing  -> "postgresql://postgres:postgres@localhost:5432/payment_validation"

-- | Create a connection pool.
--   Uses resource-pool >= 0.4 API.
createPool' :: ByteString -> IO ConnectionPool
createPool' connStr =
  newPool
    . setNumStripes (Just 2)
    $ defaultPoolConfig
        (connectPostgreSQL connStr)  -- create action
        close                        -- destroy action
        60                           -- max idle time (seconds)
        10                           -- max resources

-- | Run an action with a connection from the pool.
withPool :: ConnectionPool -> (Connection -> IO a) -> IO a
withPool = withResource
