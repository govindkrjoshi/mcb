{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : CircuitBreaker.Types
-- Description : Core data types for the circuit breaker
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines the core data types used by the circuit breaker,
-- including state representation, configuration, and newtype wrappers
-- for type-safe configuration values.

module CircuitBreaker.Types
  ( -- * Circuit Breaker State
    State (..)

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

    -- * Configuration
  , CircuitBreakerConfig (..)
  , defaultConfig

    -- * Builder Pattern
  , (&)
  , setFailureThreshold
  , setSlidingWindowSize
  , setWaitDuration
  , setHalfOpenPermits
  , setExceptionPredicate
  , setOnStateTransition
  ) where

import Control.Exception (SomeException)
import Data.Function ((&))
import Data.Time.Clock (NominalDiffTime)

-- | The state of a circuit breaker.
--
-- The circuit breaker transitions between these states based on
-- the success or failure of protected operations.
--
-- @since 0.1.0.0
data State
  = Closed
    -- ^ Normal operation. Requests are allowed through and failures are tracked.
  | Open
    -- ^ Circuit is open due to too many failures. Requests fail fast.
  | HalfOpen
    -- ^ Testing if the service has recovered. Limited requests are allowed.
  deriving stock (Eq, Show, Ord, Bounded, Enum)

-- | Failure threshold as a ratio between 0 (exclusive) and 1 (inclusive).
--
-- Represents the failure rate that triggers the circuit to open.
-- For example, a threshold of 0.5 means the circuit opens when 50% of
-- requests fail.
--
-- @since 0.1.0.0
newtype FailureThreshold = FailureThreshold
  { unFailureThreshold :: Double
    -- ^ Extract the raw 'Double' value from a 'FailureThreshold'.
  }
  deriving stock (Eq, Show, Ord)

-- | Create a 'FailureThreshold' with validation.
--
-- The threshold must be greater than 0 and at most 1.
--
-- ==== __Examples__
--
-- >>> mkFailureThreshold 0.5
-- Right (FailureThreshold {unFailureThreshold = 0.5})
--
-- >>> mkFailureThreshold 0
-- Left "FailureThreshold must be > 0 and <= 1, got: 0.0"
--
-- >>> mkFailureThreshold 1.5
-- Left "FailureThreshold must be > 0 and <= 1, got: 1.5"
--
-- @since 0.1.0.0
mkFailureThreshold :: Double -> Either String FailureThreshold
mkFailureThreshold x
  | x > 0 && x <= 1 = Right (FailureThreshold x)
  | otherwise = Left $ "FailureThreshold must be > 0 and <= 1, got: " <> show x

-- | Size of the sliding window for tracking failures.
--
-- This determines how many recent calls are considered when
-- calculating the failure rate.
--
-- @since 0.1.0.0
newtype SlidingWindowSize = SlidingWindowSize
  { unSlidingWindowSize :: Int
    -- ^ Extract the raw 'Int' value from a 'SlidingWindowSize'.
  }
  deriving stock (Eq, Show, Ord)

-- | Create a 'SlidingWindowSize' with validation.
--
-- The window size must be greater than 0.
--
-- ==== __Examples__
--
-- >>> mkSlidingWindowSize 10
-- Right (SlidingWindowSize {unSlidingWindowSize = 10})
--
-- >>> mkSlidingWindowSize 0
-- Left "SlidingWindowSize must be > 0, got: 0"
--
-- @since 0.1.0.0
mkSlidingWindowSize :: Int -> Either String SlidingWindowSize
mkSlidingWindowSize x
  | x > 0 = Right (SlidingWindowSize x)
  | otherwise = Left $ "SlidingWindowSize must be > 0, got: " <> show x

-- | Duration to wait before transitioning from Open to HalfOpen.
--
-- This is the "cool down" period during which the circuit breaker
-- will not allow any requests through.
--
-- @since 0.1.0.0
newtype WaitDuration = WaitDuration
  { unWaitDuration :: NominalDiffTime
    -- ^ Extract the raw 'NominalDiffTime' value from a 'WaitDuration'.
  }
  deriving stock (Eq, Show, Ord)

-- | Create a 'WaitDuration' with validation.
--
-- The duration must be non-negative.
--
-- ==== __Examples__
--
-- >>> mkWaitDuration 30
-- Right (WaitDuration {unWaitDuration = 30s})
--
-- >>> mkWaitDuration (-1)
-- Left "WaitDuration must be >= 0, got: -1s"
--
-- @since 0.1.0.0
mkWaitDuration :: NominalDiffTime -> Either String WaitDuration
mkWaitDuration x
  | x >= 0 = Right (WaitDuration x)
  | otherwise = Left $ "WaitDuration must be >= 0, got: " <> show x

-- | Number of calls permitted in the HalfOpen state.
--
-- When the circuit is half-open, this many test requests are allowed
-- through to determine if the service has recovered.
--
-- @since 0.1.0.0
newtype HalfOpenPermits = HalfOpenPermits
  { unHalfOpenPermits :: Int
    -- ^ Extract the raw 'Int' value from 'HalfOpenPermits'.
  }
  deriving stock (Eq, Show, Ord)

-- | Create 'HalfOpenPermits' with validation.
--
-- The number of permits must be greater than 0.
--
-- ==== __Examples__
--
-- >>> mkHalfOpenPermits 3
-- Right (HalfOpenPermits {unHalfOpenPermits = 3})
--
-- >>> mkHalfOpenPermits 0
-- Left "HalfOpenPermits must be > 0, got: 0"
--
-- @since 0.1.0.0
mkHalfOpenPermits :: Int -> Either String HalfOpenPermits
mkHalfOpenPermits x
  | x > 0 = Right (HalfOpenPermits x)
  | otherwise = Left $ "HalfOpenPermits must be > 0, got: " <> show x

-- | Configuration for a circuit breaker.
--
-- Use 'defaultConfig' and the builder pattern to construct a configuration:
--
-- @
-- config = defaultConfig
--   & setFailureThreshold 0.5
--   & setSlidingWindowSize 20
--   & setWaitDuration 60
-- @
--
-- @since 0.1.0.0
data CircuitBreakerConfig = CircuitBreakerConfig
  { failureThreshold :: !FailureThreshold
    -- ^ The failure rate threshold that triggers the circuit to open.
  , slidingWindowSize :: !SlidingWindowSize
    -- ^ The number of calls to consider for failure rate calculation.
  , waitDuration :: !WaitDuration
    -- ^ Time to wait before transitioning from Open to HalfOpen.
  , halfOpenPermits :: !HalfOpenPermits
    -- ^ Number of test calls allowed in HalfOpen state.
  , exceptionPredicate :: !(SomeException -> Bool)
    -- ^ Predicate to determine if an exception counts as a failure.
  , onStateTransition :: !(State -> State -> IO ())
    -- ^ Callback invoked when the circuit breaker changes state.
  }

instance Show CircuitBreakerConfig where
  show cfg = mconcat
    [ "CircuitBreakerConfig {"
    , "failureThreshold = ", show (failureThreshold cfg)
    , ", slidingWindowSize = ", show (slidingWindowSize cfg)
    , ", waitDuration = ", show (waitDuration cfg)
    , ", halfOpenPermits = ", show (halfOpenPermits cfg)
    , ", exceptionPredicate = <function>"
    , ", onStateTransition = <function>"
    , "}"
    ]

-- | Default circuit breaker configuration.
--
-- * Failure threshold: 50% (0.5)
-- * Sliding window size: 10 calls
-- * Wait duration: 30 seconds
-- * Half-open permits: 3 calls
-- * Exception predicate: all exceptions count as failures
-- * State transition callback: no-op
--
-- @since 0.1.0.0
defaultConfig :: CircuitBreakerConfig
defaultConfig = CircuitBreakerConfig
  { failureThreshold = FailureThreshold 0.5
  , slidingWindowSize = SlidingWindowSize 10
  , waitDuration = WaitDuration 30
  , halfOpenPermits = HalfOpenPermits 3
  , exceptionPredicate = const True
  , onStateTransition = \_ _ -> pure ()
  }

-- | Set the failure threshold.
--
-- @since 0.1.0.0
setFailureThreshold :: Double -> CircuitBreakerConfig -> CircuitBreakerConfig
setFailureThreshold x cfg = case mkFailureThreshold x of
  Right ft -> cfg { failureThreshold = ft }
  Left err -> error err

-- | Set the sliding window size.
--
-- @since 0.1.0.0
setSlidingWindowSize :: Int -> CircuitBreakerConfig -> CircuitBreakerConfig
setSlidingWindowSize x cfg = case mkSlidingWindowSize x of
  Right sw -> cfg { slidingWindowSize = sw }
  Left err -> error err

-- | Set the wait duration in seconds.
--
-- @since 0.1.0.0
setWaitDuration :: NominalDiffTime -> CircuitBreakerConfig -> CircuitBreakerConfig
setWaitDuration x cfg = case mkWaitDuration x of
  Right wd -> cfg { waitDuration = wd }
  Left err -> error err

-- | Set the number of half-open permits.
--
-- @since 0.1.0.0
setHalfOpenPermits :: Int -> CircuitBreakerConfig -> CircuitBreakerConfig
setHalfOpenPermits x cfg = case mkHalfOpenPermits x of
  Right hp -> cfg { halfOpenPermits = hp }
  Left err -> error err

-- | Set the exception predicate.
--
-- The predicate determines whether an exception should count as a failure.
-- Return 'True' to count the exception as a failure, 'False' to ignore it.
--
-- @since 0.1.0.0
setExceptionPredicate :: (SomeException -> Bool) -> CircuitBreakerConfig -> CircuitBreakerConfig
setExceptionPredicate p cfg = cfg { exceptionPredicate = p }

-- | Set the state transition callback.
--
-- The callback receives the old state and the new state.
--
-- @since 0.1.0.0
setOnStateTransition :: (State -> State -> IO ()) -> CircuitBreakerConfig -> CircuitBreakerConfig
setOnStateTransition f cfg = cfg { onStateTransition = f }
