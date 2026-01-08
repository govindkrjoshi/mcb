{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : CircuitBreaker.Exceptions
-- Description : Exception types for circuit breaker operations
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines the exception hierarchy for the circuit breaker library.
-- All circuit breaker exceptions can be caught using 'CircuitBreakerException',
-- or individual exception types can be caught specifically.
--
-- = Exception Hierarchy
--
-- @
-- CircuitBreakerException (parent type)
--   |
--   +-- CircuitOpenException   -- Circuit is open, requests rejected
--   |
--   +-- TimeoutException       -- Operation timed out (from CircuitBreaker.Timeout)
-- @
--
-- = Catching Specific Exceptions
--
-- Catch a specific exception type when you need to handle it differently:
--
-- @
-- import Control.Exception (catch)
-- import CircuitBreaker
--
-- callService :: IO Response
-- callService = withCircuitBreaker cb action
--   \`catch\` \\(CircuitOpenException name) -> do
--     logWarning $ "Circuit " ++ name ++ " is open, using cached response"
--     getCachedResponse
--   \`catch\` \\(TimeoutException duration) -> do
--     logWarning $ "Request timed out after " ++ show duration
--     getDefaultResponse
-- @
--
-- = Catching All Circuit Breaker Exceptions
--
-- Use 'CircuitBreakerException' to catch any exception from the circuit breaker:
--
-- @
-- import Control.Exception (catch)
-- import CircuitBreaker
--
-- callService :: IO Response
-- callService = withCircuitBreaker cb action
--   \`catch\` \\(CircuitBreakerException e) -> do
--     logError $ "Circuit breaker failure: " ++ displayException e
--     getDefaultResponse
-- @
--
-- = Retry Pattern
--
-- Example of retrying with exponential backoff when the circuit is open:
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Exception (catch)
-- import CircuitBreaker
--
-- callWithRetry :: Int -> IO Response
-- callWithRetry maxRetries = go 0
--   where
--     go n
--       | n >= maxRetries = error "Max retries exceeded"
--       | otherwise = withCircuitBreaker cb action
--           \`catch\` \\(CircuitOpenException _) -> do
--             let delayMs = 1000 * (2 ^ n)  -- exponential backoff
--             threadDelay (delayMs * 1000)
--             go (n + 1)
-- @
--
-- = Logging with Context
--
-- Include circuit breaker context in your logs:
--
-- @
-- import Control.Exception (catch)
-- import CircuitBreaker
--
-- callService :: IO Response
-- callService = withCircuitBreaker cb action
--   \`catch\` \\(CircuitOpenException name) -> do
--     logWithContext
--       [ ("circuit_breaker", name)
--       , ("event", "circuit_open")
--       , ("action", "fallback")
--       ]
--       "Circuit breaker rejected request"
--     getFallbackResponse
-- @

module CircuitBreaker.Exceptions
  ( -- * Parent Exception Type
    CircuitBreakerException (..)

    -- * Specific Exceptions
  , CircuitOpenException (..)
  , RateLimitedException (..)
  , BulkheadRejectedException (..)
  , RetriesExhaustedException (..)
  ) where

import Control.Exception (Exception (..), SomeException)
import Data.Typeable (Typeable, cast)

-- | Parent exception type for all circuit breaker exceptions.
--
-- This allows catching all circuit breaker related exceptions with a single
-- handler while still permitting specific exception types to be caught
-- individually.
--
-- ==== __Examples__
--
-- Catch all circuit breaker exceptions:
--
-- @
-- action \`catch\` \\(CircuitBreakerException e) ->
--   putStrLn $ "Circuit breaker error: " ++ displayException e
-- @
--
-- @since 0.1.0.0
data CircuitBreakerException = forall e. Exception e => CircuitBreakerException e

instance Show CircuitBreakerException where
  show (CircuitBreakerException e) = show e

instance Exception CircuitBreakerException where
  displayException (CircuitBreakerException e) = displayException e

-- | Helper function to convert a child exception to CircuitBreakerException.
toCircuitBreakerException :: Exception e => e -> SomeException
toCircuitBreakerException = toException . CircuitBreakerException

-- | Helper function to extract a child exception from CircuitBreakerException.
fromCircuitBreakerException :: Exception e => SomeException -> Maybe e
fromCircuitBreakerException se = do
  CircuitBreakerException e <- fromException se
  cast e

-- | Exception thrown when a circuit breaker is in the Open state.
--
-- When the circuit breaker detects too many failures and transitions to
-- the Open state, all subsequent requests are immediately rejected with
-- this exception until the circuit transitions to HalfOpen.
--
-- ==== __Examples__
--
-- >>> import Control.Exception (displayException)
-- >>> displayException (CircuitOpenException "payment-service")
-- "Circuit breaker 'payment-service' is OPEN"
--
-- Catching the exception:
--
-- @
-- withCircuitBreaker cb action \`catch\` \\(CircuitOpenException name) ->
--   logWarning $ "Service unavailable: " ++ name
-- @
--
-- @since 0.1.0.0
data CircuitOpenException = CircuitOpenException
  { circuitBreakerName :: !String
    -- ^ The name of the circuit breaker that rejected the request
  }
  deriving stock (Eq, Typeable)

instance Show CircuitOpenException where
  show (CircuitOpenException name) = "CircuitOpenException " ++ show name

instance Exception CircuitOpenException where
  displayException (CircuitOpenException name) =
    "Circuit breaker '" ++ name ++ "' is OPEN"

  toException = toCircuitBreakerException
  fromException = fromCircuitBreakerException

-- | Exception thrown when a rate limiter rejects a request.
--
-- This occurs when the token bucket is empty and no tokens are
-- available for the operation.
--
-- ==== __Examples__
--
-- >>> import Control.Exception (displayException)
-- >>> displayException RateLimitedException
-- "Rate limit exceeded"
--
-- Catching the exception:
--
-- @
-- withRateLimit rl action \`catch\` \\RateLimitedException ->
--   logWarning "Rate limit exceeded, backing off"
-- @
--
-- @since 0.1.0.0
data RateLimitedException = RateLimitedException
  deriving stock (Eq, Show, Typeable)

instance Exception RateLimitedException where
  displayException RateLimitedException = "Rate limit exceeded"

  toException = toCircuitBreakerException
  fromException = fromCircuitBreakerException

-- | Exception thrown when a bulkhead rejects a request.
--
-- This occurs when the maximum number of concurrent operations
-- has been reached.
--
-- ==== __Examples__
--
-- >>> import Control.Exception (displayException)
-- >>> displayException (BulkheadRejectedException 20)
-- "Bulkhead rejected: 20 concurrent operations active"
--
-- @since 0.1.0.0
data BulkheadRejectedException = BulkheadRejectedException
  { bulkheadActiveCount :: !Int
    -- ^ The number of active concurrent operations when rejection occurred
  }
  deriving stock (Eq, Show, Typeable)

instance Exception BulkheadRejectedException where
  displayException (BulkheadRejectedException active) =
    "Bulkhead rejected: " ++ show active ++ " concurrent operations active"

  toException = toCircuitBreakerException
  fromException = fromCircuitBreakerException

-- | Exception thrown when all retry attempts have been exhausted.
--
-- Contains the number of attempts made and the last exception encountered.
--
-- ==== __Examples__
--
-- >>> import Control.Exception (displayException, toException)
-- >>> let lastEx = toException (userError "connection refused")
-- >>> putStr $ displayException (RetriesExhaustedException 3 lastEx)
-- Retries exhausted after 3 attempts. Last error: user error (connection refused)
--
-- @since 0.1.0.0
data RetriesExhaustedException = RetriesExhaustedException
  { retriesAttemptsMade :: !Int
    -- ^ The number of retry attempts that were made
  , retriesLastException :: !SomeException
    -- ^ The last exception that caused the final retry to fail
  }
  deriving stock (Show, Typeable)

instance Exception RetriesExhaustedException where
  displayException (RetriesExhaustedException attempts lastEx) =
    "Retries exhausted after " ++ show attempts ++
    " attempts. Last error: " ++ displayException lastEx

  toException = toCircuitBreakerException
  fromException = fromCircuitBreakerException
