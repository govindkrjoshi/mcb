-- |
-- Module      : CircuitBreaker
-- Description : Production-grade circuit breaker for Haskell
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- A robust, type-safe circuit breaker implementation for building
-- resilient distributed systems in Haskell.
--
-- = Overview
--
-- The circuit breaker pattern prevents cascading failures in distributed
-- systems by detecting failures and failing fast when a dependency becomes
-- unavailable.
--
-- = Quick Start
--
-- @
-- import CircuitBreaker
--
-- main :: IO ()
-- main = do
--   cb <- newCircuitBreaker defaultConfig
--   result <- withCircuitBreaker cb $ do
--     -- Your protected operation here
--     pure "success"
--   print result
-- @
--
-- = Configuration
--
-- Use the builder pattern to configure the circuit breaker:
--
-- @
-- config = defaultConfig
--   & setFailureThreshold 0.5    -- Open when 50% of calls fail
--   & setSlidingWindowSize 20    -- Consider last 20 calls
--   & setWaitDuration 60         -- Wait 60 seconds before half-open
--   & setHalfOpenPermits 3       -- Allow 3 test calls in half-open
-- @

module CircuitBreaker
  ( -- * Circuit Breaker State
    State (..)

    -- * Configuration
  , CircuitBreakerConfig (..)
  , defaultConfig

    -- * Configuration Newtypes
  , FailureThreshold
  , unFailureThreshold
  , mkFailureThreshold
  , SlidingWindowSize
  , unSlidingWindowSize
  , mkSlidingWindowSize
  , WaitDuration
  , unWaitDuration
  , mkWaitDuration
  , HalfOpenPermits
  , unHalfOpenPermits
  , mkHalfOpenPermits

    -- * Builder Pattern
  , (&)
  , setFailureThreshold
  , setSlidingWindowSize
  , setWaitDuration
  , setHalfOpenPermits
  , setExceptionPredicate
  , setOnStateTransition

    -- * Timeout
  , TimeoutException (..)
  ) where

import CircuitBreaker.Timeout
import CircuitBreaker.Types
