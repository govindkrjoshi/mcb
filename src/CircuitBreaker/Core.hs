{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : CircuitBreaker.Core
-- Description : Core circuit breaker API
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides the core user-facing API for protecting operations
-- with circuit breaker functionality.
--
-- = Usage
--
-- @
-- import CircuitBreaker
--
-- main :: IO ()
-- main = do
--   cb <- 'newCircuitBreaker' 'defaultConfig'
--   result <- 'withCircuitBreaker' cb $ do
--     -- Your protected operation here
--     callExternalService
--   print result
-- @
--
-- = Exception Handling
--
-- The 'withCircuitBreaker' function catches exceptions from the protected
-- action and records them according to the 'exceptionPredicate' in the config.
-- If the predicate returns 'True' for an exception, it counts as a failure.
-- The original exception is always re-thrown after recording.
--
-- = State Transitions
--
-- When a state transition occurs (e.g., from Closed to Open), the
-- 'onStateTransition' callback from the config is invoked. This callback
-- runs outside of the STM transaction to allow IO operations like logging.
--
-- @since 0.1.0.0

module CircuitBreaker.Core
  ( -- * Core API
    withCircuitBreaker
  ) where

import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, try, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock (getCurrentTime)

import CircuitBreaker.Exceptions (CircuitOpenException (..))
import CircuitBreaker.Internal.State
  ( CircuitBreaker (..)
  , CircuitBreakerState (..)
  , isCallPermittedSTM
  , recordFailureSTM
  , recordSuccessSTM
  )
import CircuitBreaker.Types
  ( CircuitBreakerConfig (..)
  , State (..)
  )

-- | Execute an action with circuit breaker protection.
--
-- This is the primary API for using the circuit breaker. It:
--
-- 1. Checks if the call is permitted based on the current circuit state
-- 2. Throws 'CircuitOpenException' immediately if the circuit is open
-- 3. Executes the user action if the call is permitted
-- 4. Catches any exceptions from the user action
-- 5. Records success or failure in the circuit breaker state
-- 6. Triggers state transition callbacks if the state changed
-- 7. Re-throws any exception from the user action
--
-- ==== __Examples__
--
-- Basic usage:
--
-- @
-- cb <- newCircuitBreaker defaultConfig
-- result <- withCircuitBreaker cb $ do
--   response <- httpGet "https://api.example.com/data"
--   parseResponse response
-- @
--
-- With exception handling:
--
-- @
-- import Control.Exception (catch)
--
-- result <- withCircuitBreaker cb apiCall
--   \`catch\` \\(CircuitOpenException name) -> do
--     logWarning $ "Circuit " ++ name ++ " is open"
--     return defaultValue
-- @
--
-- ==== __Thread Safety__
--
-- This function is thread-safe. Multiple threads can call 'withCircuitBreaker'
-- concurrently on the same 'CircuitBreaker' instance. State changes are atomic.
--
-- ==== __Performance__
--
-- Overhead is minimal (less than 1ms) as it involves:
--
-- * Two STM transactions (permission check and result recording)
-- * One timestamp read
-- * Potential callback invocation on state change
--
-- @since 0.1.0.0
withCircuitBreaker
  :: forall m a. (MonadIO m, MonadCatch m)
  => CircuitBreaker
  -> m a
  -> m a
withCircuitBreaker cb action = do
  -- Get current time for permission check and result recording
  now <- liftIO getCurrentTime

  -- Step 1: Check if call is permitted (STM transaction)
  -- This also handles Open -> HalfOpen transition
  (permitted, stateBeforeAction) <- liftIO $ atomically $ isCallPermittedSTM cb now

  -- Step 2: If not permitted, throw CircuitOpenException
  if not permitted
    then throwM $ CircuitOpenException circuitName
    else do
      -- Step 3: Execute the user action and catch any exceptions
      result <- try action

      case result of
        Right value -> do
          -- Step 4a: Record success and check for state transition
          (oldState, newState) <- liftIO $ do
            recordNow <- getCurrentTime
            atomically $ do
              stateAfter <- recordSuccessSTM cb recordNow
              pure (cbsState stateBeforeAction, cbsState stateAfter)

          -- Step 5: Execute callback outside STM if state changed
          liftIO $ invokeCallbackIfChanged config oldState newState

          -- Return the successful result
          pure value

        Left (ex :: SomeException) -> do
          -- Step 4b: Determine if exception counts as failure
          let countsAsFailure = exceptionPredicate config ex

          -- Record result based on predicate
          (oldState, newState) <- liftIO $ do
            recordNow <- getCurrentTime
            atomically $ do
              stateAfter <-
                if countsAsFailure
                  then recordFailureSTM cb recordNow
                  else recordSuccessSTM cb recordNow
              pure (cbsState stateBeforeAction, cbsState stateAfter)

          -- Step 5: Execute callback outside STM if state changed
          liftIO $ invokeCallbackIfChanged config oldState newState

          -- Step 6: Re-throw the original exception
          throwM ex
  where
    config :: CircuitBreakerConfig
    config = cbConfig cb

    -- Circuit name for error messages (using show of config for now)
    -- In a future version, this could be a dedicated name field
    circuitName :: String
    circuitName = "circuit-breaker"

-- | Invoke the state transition callback if the state changed.
--
-- This function compares the old and new states and invokes the
-- 'onStateTransition' callback if they differ.
invokeCallbackIfChanged :: CircuitBreakerConfig -> State -> State -> IO ()
invokeCallbackIfChanged config oldState newState
  | oldState /= newState = onStateTransition config oldState newState
  | otherwise = pure ()
