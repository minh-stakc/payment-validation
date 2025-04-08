-- Payment Validation Engine -- PostgreSQL Schema
-- Apply with: psql -U postgres -d payment_validation -f src/Database/Schema.sql

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS accounts (
    account_id      UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name            TEXT        NOT NULL,
    email           TEXT        NOT NULL UNIQUE,
    balance         DOUBLE PRECISION NOT NULL DEFAULT 0.0 CHECK (balance >= 0),
    currency        TEXT        NOT NULL DEFAULT 'USD',
    status          TEXT        NOT NULL DEFAULT 'PendingVerification',
    verified        BOOLEAN     NOT NULL DEFAULT FALSE,
    fraud_flagged   BOOLEAN     NOT NULL DEFAULT FALSE,
    daily_limit     DOUBLE PRECISION NOT NULL DEFAULT 10000.0,
    monthly_limit   DOUBLE PRECISION NOT NULL DEFAULT 100000.0,
    per_tx_limit    DOUBLE PRECISION NOT NULL DEFAULT 5000.0,
    country_code    TEXT        NOT NULL DEFAULT 'US',
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_accounts_email ON accounts (email);

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS transactions (
    transaction_id  UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    from_account    UUID        NOT NULL REFERENCES accounts(account_id),
    to_account      UUID        NOT NULL REFERENCES accounts(account_id),
    amount          DOUBLE PRECISION NOT NULL CHECK (amount > 0),
    currency        TEXT        NOT NULL,
    status          TEXT        NOT NULL DEFAULT 'Pending',
    description     TEXT        NOT NULL DEFAULT '',
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_transactions_from   ON transactions (from_account);
CREATE INDEX IF NOT EXISTS idx_transactions_to     ON transactions (to_account);
CREATE INDEX IF NOT EXISTS idx_transactions_created ON transactions (created_at);

--------------------------------------------------------------------------------
-- Trigger: auto-update updated_at on accounts
--------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS set_updated_at ON accounts;
CREATE TRIGGER set_updated_at
    BEFORE UPDATE ON accounts
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

--------------------------------------------------------------------------------
-- Seed data (idempotent)
--------------------------------------------------------------------------------

INSERT INTO accounts (account_id, name, email, balance, currency, status, verified, fraud_flagged, daily_limit, monthly_limit, per_tx_limit, country_code)
VALUES
  ('a0000000-0000-0000-0000-000000000001', 'Alice Johnson',  'alice@example.com',   25000.00, 'USD', 'Active', TRUE,  FALSE, 10000.0, 100000.0, 5000.0, 'US'),
  ('a0000000-0000-0000-0000-000000000002', 'Bob Smith',      'bob@example.com',      5000.00, 'USD', 'Active', TRUE,  FALSE, 10000.0, 100000.0, 5000.0, 'GB'),
  ('a0000000-0000-0000-0000-000000000003', 'Charlie Brown',  'charlie@example.com',   500.00, 'EUR', 'Active', TRUE,  FALSE,  2000.0,  20000.0, 1000.0, 'DE'),
  ('a0000000-0000-0000-0000-000000000004', 'Diana Prince',   'diana@example.com',   50000.00, 'USD', 'Suspended', TRUE, FALSE, 10000.0, 100000.0, 5000.0, 'US'),
  ('a0000000-0000-0000-0000-000000000005', 'Eve Hacker',     'eve@example.com',     99999.00, 'USD', 'Active', TRUE,  TRUE,  10000.0, 100000.0, 5000.0, 'US'),
  ('a0000000-0000-0000-0000-000000000006', 'Frank Newbie',   'frank@example.com',    1000.00, 'USD', 'PendingVerification', FALSE, FALSE, 10000.0, 100000.0, 5000.0, 'US')
ON CONFLICT (account_id) DO NOTHING;
