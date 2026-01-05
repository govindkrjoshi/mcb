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

    -- * Timeout Combinator
  , withTimeout
  ) where

import Control.Exception (Exception(..), throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Typeable (Typeable)
import qualified System.Timeout as T

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

-- | Execute an action with a timeout.
--
-- If the action completes within the specified duration, its result is returned.
-- If the action exceeds the timeout, a 'TimeoutException' is thrown.
--
-- The timeout mechanism uses 'System.Timeout.timeout' which properly cancels
-- the action thread when the timeout fires, preventing resource leaks.
--
-- ==== __Examples__
--
-- @
-- -- Action that completes in time
-- result <- withTimeout 5 $ do
--   threadDelay 1000000  -- 1 second
--   pure "done"
-- -- result == "done"
--
-- -- Action that times out
-- withTimeout 1 $ do
--   threadDelay 5000000  -- 5 seconds
--   pure "done"
-- -- throws TimeoutException 1
-- @
--
-- @since 0.1.0.0
withTimeout :: MonadIO m => NominalDiffTime -> IO a -> m a
withTimeout duration action = liftIO $ do
  result <- T.timeout microseconds action
  case result of
    Just a  -> pure a
    Nothing -> throwIO (TimeoutException duration)
  where
    -- Convert NominalDiffTime to microseconds for System.Timeout
    microseconds :: Int
    microseconds = round (nominalDiffTimeToSeconds duration * 1_000_000)
