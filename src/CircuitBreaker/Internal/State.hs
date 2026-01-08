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
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.Time.Clock (UTCTime, getCurrentTime)

import CircuitBreaker.SlidingWindow (SlidingWindow, getFailureRate, newSlidingWindow)
import CircuitBreaker.Types
  ( CircuitBreakerConfig (..)
  , State (..)
  , unSlidingWindowSize
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
