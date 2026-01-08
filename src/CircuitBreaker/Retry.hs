{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : CircuitBreaker.Retry
-- Description : Retry pattern with configurable backoff strategies
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides a retry mechanism with configurable backoff strategies.
-- Supports exponential backoff with jitter to prevent thundering herd problems.
--
-- = Backoff Strategies
--
-- * 'ExponentialBackoff' - Delays grow exponentially with decorrelated jitter
-- * 'ConstantBackoff' - Fixed delay between retries
-- * 'NoBackoff' - Retry immediately
--
-- = Usage
--
-- @
-- import CircuitBreaker
--
-- main :: IO ()
-- main = do
--   result <- 'withRetry' 'defaultRetryConfig' $ do
--     response <- httpGet \"https://api.example.com\"
--     parseResponse response
--   print result
-- @
--
-- = Composition with Circuit Breaker
--
-- Retry should be the outermost layer, wrapping the circuit breaker:
--
-- @
-- result <- withRetry defaultRetryConfig $
--             withCircuitBreaker cb $
--               withTimeout (seconds 5) $
--                 callExternalService
-- @
--
-- @since 0.1.0.0

module CircuitBreaker.Retry
  ( -- * Retry Configuration
    RetryConfig (..)
  , BackoffStrategy (..)
  , defaultRetryConfig

    -- * Retry Combinator
  , withRetry

    -- * Builders
  , setMaxAttempts
  , setBackoffStrategy
  , setShouldRetry
  , setOnRetry
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, throwM, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock (NominalDiffTime)
import System.Random (randomRIO)

import CircuitBreaker.Exceptions (RetriesExhaustedException (..))

-- | Backoff strategy for retry attempts.
--
-- @since 0.1.0.0
data BackoffStrategy
  = ExponentialBackoff
      !NominalDiffTime  -- ^ Base delay (e.g., 0.1 for 100ms)
      !NominalDiffTime  -- ^ Maximum delay cap
      !Double           -- ^ Multiplier (typically 2.0)
    -- ^ Exponential backoff with decorrelated jitter.
    -- Each retry delay is: @random(base, min(cap, previousDelay * multiplier))@
  | ConstantBackoff !NominalDiffTime
    -- ^ Fixed delay between retries.
  | NoBackoff
    -- ^ No delay between retries (immediate retry).
  deriving stock (Eq, Show)

-- | Configuration for the retry mechanism.
--
-- Use 'defaultRetryConfig' and builders to create a configuration:
--
-- @
-- config = defaultRetryConfig
--   & setMaxAttempts 5
--   & setBackoffStrategy (ExponentialBackoff 0.1 30 2.0)
-- @
--
-- @since 0.1.0.0
data RetryConfig = RetryConfig
  { retryMaxAttempts :: !Int
    -- ^ Maximum number of attempts (including the initial attempt).
    -- Must be >= 1.
  , retryBackoff :: !BackoffStrategy
    -- ^ Strategy for calculating delay between retries.
  , retryShouldRetry :: !(SomeException -> Bool)
    -- ^ Predicate to determine if an exception should trigger a retry.
    -- Return 'True' to retry, 'False' to fail immediately.
  , retryOnRetry :: !(Int -> NominalDiffTime -> IO ())
    -- ^ Callback invoked before each retry. Receives the attempt number
    -- (1-indexed) and the delay that will be applied.
  }

instance Show RetryConfig where
  show cfg = mconcat
    [ "RetryConfig {"
    , "retryMaxAttempts = ", show (retryMaxAttempts cfg)
    , ", retryBackoff = ", show (retryBackoff cfg)
    , ", retryShouldRetry = <function>"
    , ", retryOnRetry = <function>"
    , "}"
    ]

-- | Default retry configuration.
--
-- * Maximum attempts: 3
-- * Backoff: Exponential with 100ms base, 30s cap, 2x multiplier
-- * Should retry: All exceptions
-- * On retry: No-op
--
-- @since 0.1.0.0
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { retryMaxAttempts = 3
  , retryBackoff = ExponentialBackoff 0.1 30 2.0
  , retryShouldRetry = const True
  , retryOnRetry = \_ _ -> pure ()
  }

-- | Execute an action with retry logic.
--
-- If the action fails and the exception matches the retry predicate,
-- the action is retried according to the backoff strategy.
--
-- If all retries are exhausted, throws 'RetriesExhaustedException'
-- containing the last exception.
--
-- ==== __Examples__
--
-- @
-- -- Basic retry with defaults
-- result <- withRetry defaultRetryConfig $ httpGet url
--
-- -- Custom retry configuration
-- let config = defaultRetryConfig
--       & setMaxAttempts 5
--       & setBackoffStrategy (ConstantBackoff 1.0)
--       & setShouldRetry isTransientError
--
-- result <- withRetry config $ callFlakeyService
-- @
--
-- @since 0.1.0.0
withRetry
  :: forall m a. (MonadIO m, MonadCatch m)
  => RetryConfig
  -> m a
  -> m a
withRetry config action = go 1 (getInitialDelay config)
  where
    go :: Int -> NominalDiffTime -> m a
    go attempt prevDelay = do
      result <- try action
      case result of
        Right value -> pure value
        Left (ex :: SomeException) -> do
          let shouldRetry = retryShouldRetry config ex
              maxAttempts = retryMaxAttempts config
              canRetry = attempt < maxAttempts && shouldRetry

          if canRetry
            then do
              -- Calculate delay with jitter
              delay <- liftIO $ calculateDelay config prevDelay

              -- Invoke callback
              liftIO $ retryOnRetry config attempt delay

              -- Wait
              liftIO $ threadDelay (nominalDiffTimeToMicroseconds delay)

              -- Retry
              go (attempt + 1) delay
            else
              -- All retries exhausted
              throwM $ RetriesExhaustedException attempt ex

-- | Calculate the delay for the next retry attempt.
calculateDelay :: RetryConfig -> NominalDiffTime -> IO NominalDiffTime
calculateDelay config prevDelay = case retryBackoff config of
  NoBackoff -> pure 0

  ConstantBackoff delay -> pure delay

  ExponentialBackoff base cap multiplier -> do
    -- Decorrelated jitter: random between base and min(cap, prev * multiplier)
    let maxDelay = min cap (prevDelay * realToFrac multiplier)
        minDelay = base
    if maxDelay <= minDelay
      then pure minDelay
      else do
        jitter <- randomRIO (realToFrac minDelay, realToFrac maxDelay)
        pure (realToFrac (jitter :: Double))

-- | Get the initial delay for the first retry.
getInitialDelay :: RetryConfig -> NominalDiffTime
getInitialDelay config = case retryBackoff config of
  NoBackoff -> 0
  ConstantBackoff delay -> delay
  ExponentialBackoff base _ _ -> base

-- | Convert NominalDiffTime to microseconds for threadDelay.
nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds t = round (realToFrac t * 1000000 :: Double)

-- | Set the maximum number of attempts.
--
-- @since 0.1.0.0
setMaxAttempts :: Int -> RetryConfig -> RetryConfig
setMaxAttempts n cfg = cfg { retryMaxAttempts = max 1 n }

-- | Set the backoff strategy.
--
-- @since 0.1.0.0
setBackoffStrategy :: BackoffStrategy -> RetryConfig -> RetryConfig
setBackoffStrategy s cfg = cfg { retryBackoff = s }

-- | Set the retry predicate.
--
-- The predicate determines whether a specific exception should trigger
-- a retry. Return 'True' to retry, 'False' to fail immediately.
--
-- ==== __Examples__
--
-- @
-- -- Only retry on timeout exceptions
-- config = defaultRetryConfig
--   & setShouldRetry isTimeoutException
--
-- -- Retry on any IO exception
-- config = defaultRetryConfig
--   & setShouldRetry (\\e -> isJust (fromException e :: Maybe IOError))
-- @
--
-- @since 0.1.0.0
setShouldRetry :: (SomeException -> Bool) -> RetryConfig -> RetryConfig
setShouldRetry p cfg = cfg { retryShouldRetry = p }

-- | Set the retry callback.
--
-- The callback is invoked before each retry with the attempt number
-- and the delay that will be applied.
--
-- ==== __Examples__
--
-- @
-- -- Log each retry
-- config = defaultRetryConfig
--   & setOnRetry (\\attempt delay ->
--       putStrLn $ "Retry " ++ show attempt ++ " after " ++ show delay)
-- @
--
-- @since 0.1.0.0
setOnRetry :: (Int -> NominalDiffTime -> IO ()) -> RetryConfig -> RetryConfig
setOnRetry f cfg = cfg { retryOnRetry = f }
