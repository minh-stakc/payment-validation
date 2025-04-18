{-# LANGUAGE OverloadedStrings #-}

module Database.Transaction
  ( insertTransaction
  , getTransactionsByAccount
  , getDailyTotal
  , getMonthlyTotal
  , getRecentTransactionCount
  , getAverageTransactionAmount
  , updateTransactionStatus
  ) where

import           Database.PostgreSQL.Simple
import           Types

-- | Insert a new transaction row and return it.
insertTransaction :: Connection
                  -> TransactionId
                  -> AccountId      -- ^ from
                  -> AccountId      -- ^ to
                  -> Double         -- ^ amount
                  -> Currency
                  -> TransactionStatus
                  -> String         -- ^ description
                  -> IO (Maybe Transaction)
insertTransaction conn (TransactionId tid) (AccountId from') (AccountId to') amt cur stat desc = do
  rows <- query conn
    "INSERT INTO transactions \
    \  (transaction_id, from_account, to_account, amount, currency, status, description) \
    \VALUES (?, ?, ?, ?, ?, ?, ?) \
    \RETURNING transaction_id, from_account, to_account, amount, \
    \          currency, status, description, created_at"
    (tid, from', to', amt, show cur, show stat, desc)
  pure $ case rows of
    [tx] -> Just tx
    _    -> Nothing

-- | Get all transactions for a given account (as sender or receiver).
getTransactionsByAccount :: Connection -> AccountId -> IO [Transaction]
getTransactionsByAccount conn (AccountId uid) =
  query conn
    "SELECT transaction_id, from_account, to_account, amount, \
    \       currency, status, description, created_at \
    \FROM transactions \
    \WHERE from_account = ? OR to_account = ? \
    \ORDER BY created_at DESC"
    (uid, uid)

-- | Sum of completed transaction amounts sent by account today.
getDailyTotal :: Connection -> AccountId -> IO Double
getDailyTotal conn (AccountId uid) = do
  [Only total] <- query conn
    "SELECT COALESCE(SUM(amount), 0) \
    \FROM transactions \
    \WHERE from_account = ? \
    \  AND status = 'Completed' \
    \  AND created_at >= CURRENT_DATE"
    (Only uid)
  pure total

-- | Sum of completed transaction amounts sent by account this calendar month.
getMonthlyTotal :: Connection -> AccountId -> IO Double
getMonthlyTotal conn (AccountId uid) = do
  [Only total] <- query conn
    "SELECT COALESCE(SUM(amount), 0) \
    \FROM transactions \
    \WHERE from_account = ? \
    \  AND status = 'Completed' \
    \  AND created_at >= date_trunc('month', CURRENT_DATE)"
    (Only uid)
  pure total

-- | Count of transactions sent by account in the last N minutes.
getRecentTransactionCount :: Connection -> AccountId -> Int -> IO Int
getRecentTransactionCount conn (AccountId uid) windowMinutes = do
  [Only cnt] <- query conn
    "SELECT COUNT(*)::int \
    \FROM transactions \
    \WHERE from_account = ? \
    \  AND created_at >= now() - (? || ' minutes')::interval"
    (uid, show windowMinutes)
  pure cnt

-- | Average transaction amount sent by account (across all time).
getAverageTransactionAmount :: Connection -> AccountId -> IO Double
getAverageTransactionAmount conn (AccountId uid) = do
  [Only avg'] <- query conn
    "SELECT COALESCE(AVG(amount), 0) \
    \FROM transactions \
    \WHERE from_account = ? \
    \  AND status = 'Completed'"
    (Only uid)
  pure avg'

-- | Update the status of a transaction.
updateTransactionStatus :: Connection -> TransactionId -> TransactionStatus -> IO ()
updateTransactionStatus conn (TransactionId tid) stat =
  execute conn
    "UPDATE transactions SET status = ? WHERE transaction_id = ?"
    (show stat, tid)
  >> pure ()
