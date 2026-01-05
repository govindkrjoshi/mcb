{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : CircuitBreaker.Timeout
-- Description : Timeout support for circuit breaker operations
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides timeout functionality for protecting operations
-- against hanging indefinitely. It can be composed with the circuit breaker
-- to count timeouts as failures.

module CircuitBreaker.Timeout
  ( -- * Exceptions
    TimeoutException (..)
  ) where

import Control.Exception (Exception(..))
import Data.Time.Clock (NominalDiffTime)
import Data.Typeable (Typeable)

-- | Exception thrown when an operation exceeds its timeout duration.
--
-- This exception can be caught and counted as a circuit breaker failure
-- when composed with 'withCircuitBreaker'.
--
-- ==== __Examples__
--
-- >>> import Control.Exception (displayException)
-- >>> displayException (TimeoutException 5)
-- "Operation timed out after 5s"
--
-- @since 0.1.0.0
data TimeoutException = TimeoutException
  { timeoutDuration :: !NominalDiffTime
    -- ^ The duration that was exceeded
  }
  deriving stock (Eq, Typeable)

instance Show TimeoutException where
  show (TimeoutException d) = "TimeoutException " ++ show d

instance Exception TimeoutException where
  displayException (TimeoutException d) =
    "Operation timed out after " ++ show d
