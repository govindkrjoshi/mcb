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
-- = Idempotency
--
-- Result recording is idempotent: each action execution records exactly one
-- result (success or failure), even under async exceptions or concurrent load.
-- This is achieved using a one-shot recording mechanism protected by mask.
--
-- @since 0.1.0.0

module CircuitBreaker.Core
  ( -- * Core API
    withCircuitBreaker
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask, mask, try, throwM, onException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock (getCurrentTime)

import CircuitBreaker.Exceptions (CircuitOpenException (..))
import CircuitBreaker.Internal.State
  ( CircuitBreaker (..)
  , CircuitBreakerState (..)
  , isCallPermittedSTM
  , readState
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
-- 5. Records success or failure in the circuit breaker state (exactly once)
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
-- ==== __Idempotency__
--
-- Result recording is guaranteed to happen exactly once per action execution,
-- even under async exceptions (like timeouts) or concurrent load. This is
-- achieved using a one-shot recording mechanism that:
--
-- * Uses a TVar flag to ensure recording happens at most once
-- * Uses 'mask' to ensure recording is not interrupted by async exceptions
-- * Records in an 'onException' handler to ensure recording even if interrupted
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
  :: forall m a. (MonadIO m, MonadMask m)
  => CircuitBreaker
  -> m a
  -> m a
withCircuitBreaker cb action = do
  -- Get current time for permission check
  now <- liftIO getCurrentTime

  -- Step 1: Check if call is permitted (STM transaction)
  -- This also handles Open -> HalfOpen transition
  -- We capture the state before permission check to detect Open -> HalfOpen
  (permitted, stateBeforePermissionCheck, stateAfterPermissionCheck) <- liftIO $ atomically $ do
    stateBefore <- readState cb
    (perm, stateAfter) <- isCallPermittedSTM cb now
    pure (perm, stateBefore, stateAfter)

  -- Invoke callback if permission check caused a transition (e.g., Open -> HalfOpen)
  let stateBeforeCheck = cbsState stateBeforePermissionCheck
      stateAfterCheck = cbsState stateAfterPermissionCheck
  liftIO $ invokeCallbackIfChanged config stateBeforeCheck stateAfterCheck

  -- Step 2: If not permitted, throw CircuitOpenException
  if not permitted
    then throwM $ CircuitOpenException circuitName
    else do
      -- Create a one-shot recording flag to ensure idempotent result recording.
      -- This TVar is set to True after recording, preventing double-counting.
      recordedFlag <- liftIO $ newTVarIO False

      -- Use mask to control when async exceptions are delivered.
      -- This ensures result recording completes even if an async exception
      -- (like TimeoutException) is pending.
      mask $ \restore -> do
        -- Execute the action with async exceptions restored, but set up
        -- an onException handler to record failure if interrupted.
        result <- restore (try action) `onException`
          -- If an async exception interrupts us, record failure (once)
          liftIO (recordResultOnce recordedFlag stateAfterPermissionCheck True)

        case result of
          Right value -> do
            -- Step 4a: Record success (once) and check for state transition
            liftIO $ recordResultOnce recordedFlag stateAfterPermissionCheck False
            pure value

          Left (ex :: SomeException) -> do
            -- Step 4b: Determine if exception counts as failure
            let countsAsFailure = exceptionPredicate config ex

            -- Record result based on predicate (once)
            liftIO $ recordResultOnce recordedFlag stateAfterPermissionCheck countsAsFailure

            -- Step 5: Re-throw the original exception
            throwM ex
  where
    config :: CircuitBreakerConfig
    config = cbConfig cb

    -- Circuit name for error messages (using show of config for now)
    -- In a future version, this could be a dedicated name field
    circuitName :: String
    circuitName = "circuit-breaker"

    -- | Record a result exactly once, ensuring idempotency.
    --
    -- This function uses a TVar flag to ensure that even if called multiple
    -- times (e.g., from both normal path and exception handler), the result
    -- is recorded at most once.
    --
    -- The function is safe to call from any thread and handles the case
    -- where another recording has already happened.
    recordResultOnce :: TVar Bool -> CircuitBreakerState -> Bool -> IO ()
    recordResultOnce recordedFlag stateAfterPermissionCheck isFailure = do
      recordNow <- getCurrentTime
      (shouldRecord, oldState, newState) <- atomically $ do
        alreadyRecorded <- readTVar recordedFlag
        if alreadyRecorded
          then pure (False, cbsState stateAfterPermissionCheck, cbsState stateAfterPermissionCheck)
          else do
            writeTVar recordedFlag True
            stateAfter <-
              if isFailure
                then recordFailureSTM cb recordNow
                else recordSuccessSTM cb recordNow
            pure (True, cbsState stateAfterPermissionCheck, cbsState stateAfter)

      -- Execute callback outside STM if state changed and we recorded
      when shouldRecord $ invokeCallbackIfChanged config oldState newState

-- | Invoke the state transition callback if the state changed.
--
-- This function compares the old and new states and invokes the
-- 'onStateTransition' callback if they differ.
invokeCallbackIfChanged :: CircuitBreakerConfig -> State -> State -> IO ()
invokeCallbackIfChanged config oldState newState
  | oldState /= newState = onStateTransition config oldState newState
  | otherwise = pure ()
