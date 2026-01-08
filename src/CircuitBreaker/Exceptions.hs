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
--   ├── CircuitOpenException   -- Circuit is open, requests rejected
--   └── TimeoutException       -- Operation timed out (from CircuitBreaker.Timeout)
-- @
--
-- = Catching Exceptions
--
-- @
-- import Control.Exception (catch, SomeException)
-- import CircuitBreaker.Exceptions
--
-- -- Catch a specific exception
-- action \`catch\` \\(CircuitOpenException name) ->
--   putStrLn $ "Circuit " ++ name ++ " is open"
--
-- -- Catch all circuit breaker exceptions
-- action \`catch\` \\(CircuitBreakerException e) ->
--   putStrLn $ "Circuit breaker error: " ++ displayException e
-- @

module CircuitBreaker.Exceptions
  ( -- * Parent Exception Type
    CircuitBreakerException (..)

    -- * Specific Exceptions
  , CircuitOpenException (..)
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
