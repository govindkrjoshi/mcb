{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : CircuitBreaker.Internal.State
-- Description : STM-based state management for circuit breakers
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides thread-safe state management for circuit breakers
-- using Software Transactional Memory (STM). It implements atomic read
-- and write operations that maintain consistency under concurrent access.
--
-- = Internal Module
--
-- This is an internal module. The public API is exported from "CircuitBreaker".
--
-- = Thread Safety
--
-- All operations in this module are thread-safe. Multiple threads can
-- concurrently read and update circuit breaker state without data races.
-- STM provides automatic retry semantics when transactions conflict.
--
-- = State Machine
--
-- The circuit breaker implements a three-state machine:
--
-- * __Closed__: Normal operation. Calls are permitted and results are recorded.
--   Transitions to Open when failure rate exceeds threshold with minimum calls.
--
-- * __Open__: Fast-fail mode. Calls are rejected immediately.
--   Transitions to HalfOpen after wait duration elapses.
--
-- * __HalfOpen__: Testing mode. Limited calls are permitted to test recovery.
--   Transitions to Closed on success, or back to Open on failure.

module CircuitBreaker.Internal.State
  ( -- * Circuit Breaker State
    CircuitBreakerState (..)

    -- * Circuit Breaker Handle
  , CircuitBreaker (..)

    -- * Construction
  , newCircuitBreaker

    -- * STM Operations
  , readState
  , updateState

    -- * Convenient Queries
  , getCurrentState
  , getFailureRateIO
  , getCircuitBreakerState

    -- * State Transition Predicates
  , shouldOpen
  , shouldTransitionToHalfOpen

    -- * State Transition Functions
  , transitionToOpen
  , transitionToHalfOpen
  , transitionToClosed

    -- * Call Permission
  , isCallPermitted
  , isCallPermittedSTM

    -- * Result Recording
  , recordSuccess
  , recordFailure
  , recordSuccessSTM
  , recordFailureSTM

    -- * Constants
  , minimumCallsForOpen
  , minimumCallsForOpenWithWindowSize
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

import CircuitBreaker.SlidingWindow
  ( SlidingWindow
  , getFailureRate
  , getTotalCalls
  , insertResult
  , newSlidingWindow
  , reset
  )
import CircuitBreaker.Types
  ( CircuitBreakerConfig (..)
  , HalfOpenPermits (..)
  , State (..)
  , WaitDuration (..)
  , unFailureThreshold
  , unHalfOpenPermits
  , unSlidingWindowSize
  , unWaitDuration
  )

-- | The internal state of a circuit breaker.
--
-- This record contains all mutable state that changes during circuit
-- breaker operation. It is wrapped in a 'TVar' for thread-safe access.
--
-- @since 0.1.0.0
data CircuitBreakerState = CircuitBreakerState
  { cbsState :: !State
    -- ^ The current circuit breaker state (Closed, Open, or HalfOpen).
  , cbsSlidingWindow :: !SlidingWindow
    -- ^ The sliding window tracking recent call results.
  , cbsLastStateChange :: !UTCTime
    -- ^ Timestamp of the last state transition.
  , cbsHalfOpenAttempts :: !Int
    -- ^ Number of test calls made in the current HalfOpen state.
    -- Reset to 0 when entering HalfOpen, incremented with each permitted call.
  }
  deriving stock (Eq, Show)

-- | A handle to a circuit breaker instance.
--
-- The circuit breaker wraps a 'TVar' containing the current state along
-- with the immutable configuration. Create instances using 'newCircuitBreaker'.
--
-- The handle is safe to share across threads. All operations are thread-safe.
--
-- @since 0.1.0.0
data CircuitBreaker = CircuitBreaker
  { cbStateVar :: !(TVar CircuitBreakerState)
    -- ^ The mutable state wrapped in a TVar for thread-safe access.
  , cbConfig :: !CircuitBreakerConfig
    -- ^ The immutable configuration for this circuit breaker.
  }

instance Show CircuitBreaker where
  show cb = "CircuitBreaker {config = " <> show (cbConfig cb) <> "}"

-- | Create a new circuit breaker with the given configuration.
--
-- The circuit breaker is initialized in the 'Closed' state with an empty
-- sliding window and the current timestamp.
--
-- ==== __Examples__
--
-- @
-- cb <- newCircuitBreaker defaultConfig
-- state <- getCurrentState cb
-- -- state == Closed
-- @
--
-- @since 0.1.0.0
newCircuitBreaker :: CircuitBreakerConfig -> IO CircuitBreaker
newCircuitBreaker config = do
  now <- getCurrentTime
  let windowSize = unSlidingWindowSize (slidingWindowSize config)
      initialState = CircuitBreakerState
        { cbsState = Closed
        , cbsSlidingWindow = newSlidingWindow windowSize
        , cbsLastStateChange = now
        , cbsHalfOpenAttempts = 0
        }
  stateVar <- newTVarIO initialState
  pure CircuitBreaker
    { cbStateVar = stateVar
    , cbConfig = config
    }

-- | Read the current circuit breaker state atomically.
--
-- This STM action can be composed with other STM actions in a single
-- atomic transaction.
--
-- ==== __Examples__
--
-- @
-- atomically $ do
--   state <- readState cb
--   -- use state in a larger transaction
-- @
--
-- @since 0.1.0.0
readState :: CircuitBreaker -> STM CircuitBreakerState
readState = readTVar . cbStateVar

-- | Update the circuit breaker state atomically.
--
-- This STM action can be composed with other STM actions in a single
-- atomic transaction.
--
-- ==== __Examples__
--
-- @
-- atomically $ do
--   state <- readState cb
--   let newState = state { cbsState = Open }
--   updateState cb newState
-- @
--
-- @since 0.1.0.0
updateState :: CircuitBreaker -> CircuitBreakerState -> STM ()
updateState cb = writeTVar (cbStateVar cb)

-- | Get the current state (Closed, Open, or HalfOpen) of the circuit breaker.
--
-- This is a convenience function that extracts just the 'State' value.
-- For more detailed information, use 'getCircuitBreakerState'.
--
-- ==== __Examples__
--
-- @
-- cb <- newCircuitBreaker defaultConfig
-- state <- getCurrentState cb
-- case state of
--   Closed   -> putStrLn "Circuit is closed"
--   Open     -> putStrLn "Circuit is open"
--   HalfOpen -> putStrLn "Circuit is half-open"
-- @
--
-- @since 0.1.0.0
getCurrentState :: CircuitBreaker -> IO State
getCurrentState cb = atomically $ cbsState <$> readState cb

-- | Get the current failure rate of the circuit breaker.
--
-- Returns a value between 0.0 (no failures) and 1.0 (all failures).
-- Returns 0.0 if no calls have been recorded yet.
--
-- ==== __Examples__
--
-- @
-- cb <- newCircuitBreaker defaultConfig
-- rate <- getFailureRateIO cb
-- -- rate == 0.0 (no calls recorded yet)
-- @
--
-- @since 0.1.0.0
getFailureRateIO :: CircuitBreaker -> IO Double
getFailureRateIO cb = atomically $ do
  state <- readState cb
  pure $ getFailureRate (cbsSlidingWindow state)

-- | Get the full circuit breaker state.
--
-- This returns a snapshot of the complete internal state including
-- the state, sliding window, and last state change timestamp.
--
-- ==== __Examples__
--
-- @
-- cb <- newCircuitBreaker defaultConfig
-- cbState <- getCircuitBreakerState cb
-- print (cbsState cbState)
-- print (cbsLastStateChange cbState)
-- @
--
-- @since 0.1.0.0
getCircuitBreakerState :: CircuitBreaker -> IO CircuitBreakerState
getCircuitBreakerState cb = atomically $ readState cb

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Minimum number of calls required before the circuit can open.
--
-- This prevents the circuit from opening due to a high failure rate when
-- only a small number of calls have been made (e.g., 1 failure out of 1 call
-- = 100% failure rate).
--
-- @since 0.1.0.0
minimumCallsForOpen :: Int
minimumCallsForOpen = 5

-- | Calculate the minimum calls for opening based on sliding window size.
--
-- Returns the larger of 'minimumCallsForOpen' (5) or 10% of the window size.
-- This ensures that under concurrent load, we have enough samples to make
-- a meaningful decision about whether to open the circuit.
--
-- This fix addresses the "timeout storm" problem (mcb-abq) where under high
-- concurrent load, failing requests might be recorded before successful ones,
-- causing a transient spike in failure rate that prematurely opens the circuit.
--
-- ==== __Examples__
--
-- >>> minimumCallsForOpenWithWindowSize 10
-- 5
--
-- >>> minimumCallsForOpenWithWindowSize 100
-- 10
--
-- >>> minimumCallsForOpenWithWindowSize 1000
-- 100
--
-- @since 0.1.0.0
minimumCallsForOpenWithWindowSize :: Int -> Int
minimumCallsForOpenWithWindowSize windowSize =
  max minimumCallsForOpen (windowSize `div` 10)

-- ---------------------------------------------------------------------------
-- State Transition Predicates
-- ---------------------------------------------------------------------------

-- | Check if the circuit breaker should transition from Closed to Open.
--
-- The circuit should open when:
--
-- 1. The sliding window has at least enough calls for a meaningful sample
--    (the larger of 5 or 10% of the window size)
-- 2. The failure rate equals or exceeds the configured threshold
--
-- The minimum sample size requirement ensures that under concurrent load,
-- where result recording order is non-deterministic, the circuit doesn't
-- open due to transient spikes in failure rate.
--
-- ==== __Examples__
--
-- @
-- -- With threshold 0.5, window size 100, and 10 calls (6 failures):
-- -- minimum calls = max(5, 100 div 10) = 10
-- -- totalCalls (10) >= minimumCalls (10) = True
-- -- failure rate = 0.6 >= 0.5 = True
-- -- shouldOpen returns True
--
-- -- With threshold 0.5, window size 100, and 5 calls (5 failures):
-- -- minimum calls = max(5, 100 div 10) = 10
-- -- totalCalls (5) >= minimumCalls (10) = False
-- -- shouldOpen returns False (not enough samples yet)
-- @
--
-- @since 0.1.0.0
shouldOpen :: CircuitBreakerConfig -> CircuitBreakerState -> Bool
shouldOpen config cbState =
  let window = cbsSlidingWindow cbState
      windowSize = unSlidingWindowSize (slidingWindowSize config)
      minCalls = minimumCallsForOpenWithWindowSize windowSize
      threshold = unFailureThreshold (failureThreshold config)
      totalCalls = getTotalCalls window
      failureRate = getFailureRate window
  in totalCalls >= minCalls && failureRate >= threshold

-- | Check if the circuit breaker should transition from Open to HalfOpen.
--
-- The circuit should transition to HalfOpen when the wait duration has
-- elapsed since the last state transition.
--
-- ==== __Examples__
--
-- @
-- -- With waitDuration = 30 seconds
-- -- If lastStateChange was 31 seconds ago:
-- -- shouldTransitionToHalfOpen returns True
-- @
--
-- @since 0.1.0.0
shouldTransitionToHalfOpen :: CircuitBreakerConfig -> CircuitBreakerState -> UTCTime -> Bool
shouldTransitionToHalfOpen config cbState now =
  let waitTime = unWaitDuration (waitDuration config)
      transitionTime = addUTCTime waitTime (cbsLastStateChange cbState)
  in now >= transitionTime

-- ---------------------------------------------------------------------------
-- State Transition Functions
-- ---------------------------------------------------------------------------

-- | Transition the circuit breaker to the Open state.
--
-- This function:
--
-- * Sets the state to 'Open'
-- * Preserves the sliding window (does NOT reset - mcb-oz8 fix)
-- * Records the transition timestamp
-- * Resets the half-open attempts counter to 0
--
-- Note: The sliding window is intentionally NOT reset when transitioning
-- to Open. This ensures that in-flight calls that were permitted before
-- the transition can still have their results recorded accurately.
--
-- Note: This does not invoke the state transition callback. The caller
-- is responsible for invoking the callback if needed.
--
-- @since 0.1.0.0
transitionToOpen :: UTCTime -> CircuitBreakerState -> CircuitBreakerState
transitionToOpen now cbState = cbState
  { cbsState = Open
  , cbsLastStateChange = now
  , cbsHalfOpenAttempts = 0
  }

-- | Transition the circuit breaker to the HalfOpen state.
--
-- This function:
--
-- * Sets the state to 'HalfOpen'
-- * Resets the half-open attempts counter to 0
-- * Records the transition timestamp
--
-- Note: The sliding window is NOT reset, as we want to continue
-- tracking results to determine if recovery is successful.
--
-- @since 0.1.0.0
transitionToHalfOpen :: UTCTime -> CircuitBreakerState -> CircuitBreakerState
transitionToHalfOpen now cbState = cbState
  { cbsState = HalfOpen
  , cbsLastStateChange = now
  , cbsHalfOpenAttempts = 0
  }

-- | Transition the circuit breaker to the Closed state.
--
-- This function:
--
-- * Sets the state to 'Closed'
-- * Resets the sliding window (clear all recorded results)
-- * Records the transition timestamp
--
-- @since 0.1.0.0
transitionToClosed :: UTCTime -> CircuitBreakerState -> CircuitBreakerState
transitionToClosed now cbState = cbState
  { cbsState = Closed
  , cbsSlidingWindow = reset (cbsSlidingWindow cbState)
  , cbsLastStateChange = now
  , cbsHalfOpenAttempts = 0
  }

-- ---------------------------------------------------------------------------
-- Call Permission
-- ---------------------------------------------------------------------------

-- | Check if a call is permitted based on the current circuit breaker state.
--
-- Returns a tuple of (permitted, newState) where:
--
-- * @permitted@ indicates whether the call should be allowed
-- * @newState@ is the potentially updated state (e.g., if transitioning to HalfOpen)
--
-- Behavior by state:
--
-- * __Closed__: Always permitted. State unchanged.
-- * __Open__: Permitted only if wait duration has elapsed (transitions to HalfOpen).
-- * __HalfOpen__: Permitted only if within the half-open permits limit.
--
-- ==== __Examples__
--
-- @
-- cb <- newCircuitBreaker defaultConfig
-- now <- getCurrentTime
-- (permitted, _) <- isCallPermitted cb now
-- -- permitted == True (circuit starts Closed)
-- @
--
-- @since 0.1.0.0
isCallPermitted :: CircuitBreaker -> UTCTime -> IO (Bool, CircuitBreakerState)
isCallPermitted cb now = atomically $ isCallPermittedSTM cb now

-- | STM version of 'isCallPermitted'.
--
-- This can be composed with other STM operations in a single transaction.
--
-- @since 0.1.0.0
isCallPermittedSTM :: CircuitBreaker -> UTCTime -> STM (Bool, CircuitBreakerState)
isCallPermittedSTM cb now = do
  cbState <- readState cb
  let config = cbConfig cb
  case cbsState cbState of
    Closed -> do
      -- Closed state: always permit calls
      pure (True, cbState)

    Open -> do
      -- Open state: check if we should transition to HalfOpen
      if shouldTransitionToHalfOpen config cbState now
        then do
          -- Transition to HalfOpen and permit the first test call
          let newState = (transitionToHalfOpen now cbState)
                { cbsHalfOpenAttempts = 1 }  -- Count this call
          updateState cb newState
          pure (True, newState)
        else do
          -- Still in cooldown period, reject call
          pure (False, cbState)

    HalfOpen -> do
      -- HalfOpen state: check if we're within the permits limit
      let maxPermits = unHalfOpenPermits (halfOpenPermits config)
          currentAttempts = cbsHalfOpenAttempts cbState
      if currentAttempts < maxPermits
        then do
          -- Permit the call and increment the counter
          let newState = cbState { cbsHalfOpenAttempts = currentAttempts + 1 }
          updateState cb newState
          pure (True, newState)
        else do
          -- Exceeded permits, reject call
          pure (False, cbState)

-- ---------------------------------------------------------------------------
-- Result Recording
-- ---------------------------------------------------------------------------

-- | Record a successful call result.
--
-- Behavior by state:
--
-- * __Closed__: Records success in sliding window.
-- * __Open__: No-op (calls are rejected in Open state).
-- * __HalfOpen__: Records success. If all permitted calls succeeded,
--   transitions to Closed.
--
-- Returns the updated state.
--
-- @since 0.1.0.0
recordSuccess :: CircuitBreaker -> UTCTime -> IO CircuitBreakerState
recordSuccess cb now = atomically $ recordSuccessSTM cb now

-- | STM version of 'recordSuccess'.
--
-- @since 0.1.0.0
recordSuccessSTM :: CircuitBreaker -> UTCTime -> STM CircuitBreakerState
recordSuccessSTM cb now = do
  cbState <- readState cb
  let config = cbConfig cb
  case cbsState cbState of
    Closed -> do
      -- Record success in sliding window
      let newWindow = insertResult True (cbsSlidingWindow cbState)
          newState = cbState { cbsSlidingWindow = newWindow }
      updateState cb newState
      pure newState

    Open -> do
      -- No-op in Open state (shouldn't happen normally)
      pure cbState

    HalfOpen -> do
      -- Record success and check if we can transition to Closed
      let newWindow = insertResult True (cbsSlidingWindow cbState)
          maxPermits = unHalfOpenPermits (halfOpenPermits config)
          newState = cbState { cbsSlidingWindow = newWindow }
      -- If we've completed all permitted test calls successfully,
      -- transition to Closed
      if cbsHalfOpenAttempts cbState >= maxPermits
        then do
          let closedState = transitionToClosed now newState
          updateState cb closedState
          pure closedState
        else do
          updateState cb newState
          pure newState

-- | Record a failed call result.
--
-- Behavior by state:
--
-- * __Closed__: Records failure in sliding window. Transitions to Open
--   if threshold is exceeded.
-- * __Open__: No-op (calls are rejected in Open state).
-- * __HalfOpen__: Transitions immediately back to Open.
--
-- Returns the updated state.
--
-- @since 0.1.0.0
recordFailure :: CircuitBreaker -> UTCTime -> IO CircuitBreakerState
recordFailure cb now = atomically $ recordFailureSTM cb now

-- | STM version of 'recordFailure'.
--
-- @since 0.1.0.0
recordFailureSTM :: CircuitBreaker -> UTCTime -> STM CircuitBreakerState
recordFailureSTM cb now = do
  cbState <- readState cb
  let config = cbConfig cb
  case cbsState cbState of
    Closed -> do
      -- Record failure in sliding window
      let newWindow = insertResult False (cbsSlidingWindow cbState)
          newState = cbState { cbsSlidingWindow = newWindow }
      -- Check if we should transition to Open
      if shouldOpen config newState
        then do
          let openState = transitionToOpen now newState
          updateState cb openState
          pure openState
        else do
          updateState cb newState
          pure newState

    Open -> do
      -- No-op in Open state (shouldn't happen normally)
      pure cbState

    HalfOpen -> do
      -- Any failure in HalfOpen immediately transitions to Open
      let openState = transitionToOpen now cbState
      updateState cb openState
      pure openState
