{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : CircuitBreaker.Resilience
-- Description : Unified resilience pattern composition
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides utilities for composing multiple resilience patterns
-- (circuit breaker, timeout, bulkhead, rate limiter, retry) into a single
-- unified wrapper.
--
-- = Composition Order
--
-- Patterns are applied in this order (outermost to innermost):
--
-- @
-- Retry → Circuit Breaker → Timeout → Bulkhead → Rate Limiter → Action
-- @
--
-- This order ensures:
--
-- * Retry wraps everything, allowing retries of failed circuit breaker calls
-- * Circuit breaker sees aggregated failures from all layers below
-- * Timeout prevents any single operation from blocking indefinitely
-- * Bulkhead limits concurrent load before rate limiting
-- * Rate limiter is closest to the action, smoothing request rate
--
-- = Usage
--
-- @
-- import CircuitBreaker
--
-- main :: IO ()
-- main = do
--   cb <- newCircuitBreaker defaultConfig
--   bh <- newBulkhead (BulkheadConfig 20)
--   rl <- newRateLimiter (RateLimiterConfig 100 500)
--
--   let config = defaultResilienceConfig
--         & addRetry defaultRetryConfig
--         & addCircuitBreaker cb
--         & addTimeout 5
--         & addBulkhead bh
--         & addRateLimiter rl
--
--   result <- withResilience config callExternalService
--   print result
-- @
--
-- @since 0.1.0.0

module CircuitBreaker.Resilience
  ( -- * Configuration
    ResilienceConfig (..)
  , defaultResilienceConfig

    -- * Composition
  , withResilience

    -- * Builders
  , addCircuitBreaker
  , addTimeout
  , addBulkhead
  , addRateLimiter
  , addRetry
  ) where

import Data.Time.Clock (NominalDiffTime)

import CircuitBreaker.Bulkhead (Bulkhead, withBulkhead)
import CircuitBreaker.Core (withCircuitBreaker)
import CircuitBreaker.Internal.State (CircuitBreaker)
import CircuitBreaker.RateLimiter (RateLimiter, withRateLimit)
import CircuitBreaker.Retry (RetryConfig, withRetry)
import CircuitBreaker.Timeout (withTimeout)

-- | Unified configuration for all resilience patterns.
--
-- Use 'defaultResilienceConfig' and builder functions to construct:
--
-- @
-- config = defaultResilienceConfig
--   & addCircuitBreaker cb
--   & addTimeout 5
--   & addRetry defaultRetryConfig
-- @
--
-- Only non-'Nothing' components are applied.
--
-- @since 0.1.0.0
data ResilienceConfig = ResilienceConfig
  { resRetry :: !(Maybe RetryConfig)
    -- ^ Optional retry configuration (outermost)
  , resCircuitBreaker :: !(Maybe CircuitBreaker)
    -- ^ Optional circuit breaker
  , resTimeout :: !(Maybe NominalDiffTime)
    -- ^ Optional timeout duration
  , resBulkhead :: !(Maybe Bulkhead)
    -- ^ Optional bulkhead for concurrency limiting
  , resRateLimiter :: !(Maybe RateLimiter)
    -- ^ Optional rate limiter (innermost)
  }

instance Show ResilienceConfig where
  show cfg = mconcat
    [ "ResilienceConfig {"
    , "retry = ", showMaybe "RetryConfig" (resRetry cfg)
    , ", circuitBreaker = ", showMaybe "CircuitBreaker" (resCircuitBreaker cfg)
    , ", timeout = ", maybe "Nothing" show (resTimeout cfg)
    , ", bulkhead = ", showMaybe "Bulkhead" (resBulkhead cfg)
    , ", rateLimiter = ", showMaybe "RateLimiter" (resRateLimiter cfg)
    , "}"
    ]
    where
      showMaybe :: String -> Maybe a -> String
      showMaybe name = maybe "Nothing" (const $ "Just <" ++ name ++ ">")

-- | Default resilience configuration with no patterns enabled.
--
-- @since 0.1.0.0
defaultResilienceConfig :: ResilienceConfig
defaultResilienceConfig = ResilienceConfig
  { resRetry = Nothing
  , resCircuitBreaker = Nothing
  , resTimeout = Nothing
  , resBulkhead = Nothing
  , resRateLimiter = Nothing
  }

-- | Execute an action with all configured resilience patterns.
--
-- Patterns are applied in order: Retry → Circuit Breaker → Timeout → Bulkhead → Rate Limiter
--
-- Any pattern set to 'Nothing' in the config is skipped.
--
-- ==== __Examples__
--
-- @
-- -- Full stack
-- let config = defaultResilienceConfig
--       & addRetry defaultRetryConfig
--       & addCircuitBreaker cb
--       & addTimeout 5
--       & addBulkhead bh
--       & addRateLimiter rl
--
-- result <- withResilience config $ callExternalService
--
-- -- Just circuit breaker and timeout
-- let simpleConfig = defaultResilienceConfig
--       & addCircuitBreaker cb
--       & addTimeout 5
--
-- result <- withResilience simpleConfig $ callExternalService
-- @
--
-- @since 0.1.0.0
withResilience
  :: ResilienceConfig
  -> IO a
  -> IO a
withResilience config action =
  -- Apply patterns from outermost to innermost
  maybeApply withRetry (resRetry config) $
    maybeApply withCircuitBreaker (resCircuitBreaker config) $
      maybeApplyTimeout (resTimeout config) $
        maybeApply withBulkhead (resBulkhead config) $
          maybeApply withRateLimit (resRateLimiter config) $
            action
  where
    -- Helper to optionally apply a pattern
    maybeApply :: (a -> IO b -> IO b) -> Maybe a -> IO b -> IO b
    maybeApply _ Nothing = id
    maybeApply f (Just a) = f a

    -- Special case for timeout which takes NominalDiffTime directly
    maybeApplyTimeout :: Maybe NominalDiffTime -> IO a -> IO a
    maybeApplyTimeout Nothing = id
    maybeApplyTimeout (Just t) = withTimeout t

-- | Add retry configuration to the resilience config.
--
-- @since 0.1.0.0
addRetry :: RetryConfig -> ResilienceConfig -> ResilienceConfig
addRetry r cfg = cfg { resRetry = Just r }

-- | Add a circuit breaker to the resilience config.
--
-- @since 0.1.0.0
addCircuitBreaker :: CircuitBreaker -> ResilienceConfig -> ResilienceConfig
addCircuitBreaker cb cfg = cfg { resCircuitBreaker = Just cb }

-- | Add a timeout to the resilience config.
--
-- The timeout is specified in seconds as 'NominalDiffTime'.
--
-- @since 0.1.0.0
addTimeout :: NominalDiffTime -> ResilienceConfig -> ResilienceConfig
addTimeout t cfg = cfg { resTimeout = Just t }

-- | Add a bulkhead to the resilience config.
--
-- @since 0.1.0.0
addBulkhead :: Bulkhead -> ResilienceConfig -> ResilienceConfig
addBulkhead bh cfg = cfg { resBulkhead = Just bh }

-- | Add a rate limiter to the resilience config.
--
-- @since 0.1.0.0
addRateLimiter :: RateLimiter -> ResilienceConfig -> ResilienceConfig
addRateLimiter rl cfg = cfg { resRateLimiter = Just rl }
