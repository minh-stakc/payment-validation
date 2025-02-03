# Functional Payment Validation Engine

A Haskell + PostgreSQL payment validation system demonstrating composable,
pure-functional validation pipelines with persistent storage.

## Architecture

The engine validates payment requests through a composable pipeline of
independent validation rules:

- **Balance Check** -- ensures the sender has sufficient funds
- **Fraud Detection** -- flags suspicious patterns (velocity, amount anomalies, blocked countries)
- **Transaction Limits** -- enforces daily, per-transaction, and monthly caps
- **Account Verification** -- confirms accounts are active, verified, and not frozen

Each validator is a pure function `Account -> Payment -> ValidationResult`, making
them independently testable without IO. The pipeline composes validators using
a semigroup/monoid pattern on `ValidationResult`, collecting *all* errors rather
than short-circuiting on the first failure.

## Project Structure

```
payment-validation/
  app/Main.hs                   -- Scotty REST API entry point
  src/
    Types.hs                    -- Core domain types
    Validation/
      Core.hs                   -- Validation monad and combinators
      Balance.hs                -- Sufficient-funds check
      Fraud.hs                  -- Fraud flag detection
      Limits.hs                 -- Transaction limit enforcement
      Account.hs                -- Account status verification
      Pipeline.hs               -- Composed validation pipeline
    Database/
      Connection.hs             -- PostgreSQL connection pool
      Account.hs                -- Account queries
      Transaction.hs            -- Transaction queries
      Schema.sql                -- DDL for PostgreSQL
    Api/
      Routes.hs                 -- Scotty route definitions
      Handlers.hs               -- Request handlers
  test/
    Spec.hs                     -- Test runner
    ValidationSpec.hs           -- Pure validation unit tests
```

## Building

```bash
stack build
```

## Running

Ensure PostgreSQL is running, then apply the schema:

```bash
psql -U postgres -d payment_validation -f src/Database/Schema.sql
```

Set the `DATABASE_URL` environment variable (defaults to
`postgresql://postgres:postgres@localhost:5432/payment_validation`) and start:

```bash
stack run
```

The server listens on port 3000 by default.

## API Endpoints

| Method | Path                         | Description                        |
|--------|------------------------------|------------------------------------|
| GET    | /health                      | Health check                       |
| POST   | /api/validate                | Validate a payment (dry run)       |
| POST   | /api/payments                | Submit and process a payment       |
| GET    | /api/accounts/:id            | Retrieve account details           |
| GET    | /api/accounts/:id/transactions | List transactions for an account |

## Testing

```bash
stack test
```

The validation tests run as pure functions -- no database required.

## Design Principles

1. **Purity** -- validation logic is completely pure; side effects are
   confined to the database and HTTP layers.
2. **Composability** -- validators compose via `<>` (Semigroup) and can be
   selectively included or excluded.
3. **Accumulating errors** -- the `Validation` applicative collects all
   failures rather than stopping at the first.
4. **Type safety** -- newtypes and sum types eliminate entire classes of bugs
   (e.g., `AccountId` vs `Int`, `Currency` enum).
