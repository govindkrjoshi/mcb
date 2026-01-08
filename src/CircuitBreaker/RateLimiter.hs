{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : CircuitBreaker.RateLimiter
-- Description : Token bucket rate limiter for controlling request rates
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides a token bucket rate limiter for controlling the rate
-- of operations. The rate limiter allows a configurable number of operations
-- per second with burst capacity.
--
-- = Token Bucket Algorithm
--
-- The token bucket algorithm works as follows:
--
-- * Tokens are added to the bucket at a fixed rate (tokens per second)
-- * The bucket has a maximum capacity (burst size)
-- * Each operation consumes one token
-- * If no tokens are available, the operation is rejected
--
-- = Usage
--
-- @
-- import CircuitBreaker
--
-- main :: IO ()
-- main = do
--   -- Allow 100 requests per second with burst of 500
--   rl <- 'newRateLimiter' ('RateLimiterConfig' 100 500)
--   result <- 'withRateLimit' rl $ do
--     callExternalService
--   print result
-- @
--
-- = Composition with Circuit Breaker
--
-- Rate limiters are typically applied closest to the actual operation,
-- inside the circuit breaker:
--
-- @
-- result <- withCircuitBreaker cb $
--             withRateLimit rl $
--               callExternalService
-- @
--
-- @since 0.1.0.0

module CircuitBreaker.RateLimiter
  ( -- * Rate Limiter
    RateLimiter
  , RateLimiterConfig (..)
  , newRateLimiter
  , withRateLimit

    -- * Low-level Operations
  , tryConsume
  , getAvailableTokens
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

import CircuitBreaker.Exceptions (RateLimitedException (..))

-- | Configuration for a rate limiter.
--
-- @since 0.1.0.0
data RateLimiterConfig = RateLimiterConfig
  { rlcRate :: !Double
    -- ^ Tokens added per second. For example, @100@ means 100 operations
    -- per second are permitted on average.
  , rlcCapacity :: !Double
    -- ^ Maximum number of tokens (burst capacity). When the bucket is full,
    -- this many operations can happen in quick succession.
  }
  deriving stock (Eq, Show)

-- | A token bucket rate limiter.
--
-- Create with 'newRateLimiter' and use with 'withRateLimit'.
--
-- @since 0.1.0.0
data RateLimiter = RateLimiter
  { rlTokens :: !(TVar Double)
    -- ^ Current number of tokens in the bucket
  , rlLastRefill :: !(TVar UTCTime)
    -- ^ Time of the last token refill
  , rlConfig :: !RateLimiterConfig
    -- ^ Configuration for this rate limiter
  }

-- | Create a new rate limiter with the specified configuration.
--
-- The bucket starts full (at capacity).
--
-- ==== __Examples__
--
-- @
-- -- 100 requests per second, burst of 500
-- rl <- newRateLimiter (RateLimiterConfig 100 500)
--
-- -- 10 requests per second, burst of 20
-- rl <- newRateLimiter (RateLimiterConfig 10 20)
-- @
--
-- @since 0.1.0.0
newRateLimiter :: RateLimiterConfig -> IO RateLimiter
newRateLimiter config = do
  now <- getCurrentTime
  tokens <- newTVarIO (rlcCapacity config)
  lastRefill <- newTVarIO now
  pure RateLimiter
    { rlTokens = tokens
    , rlLastRefill = lastRefill
    , rlConfig = config
    }

-- | Execute an action with rate limiting.
--
-- If a token is available, it is consumed and the action is executed.
-- If no tokens are available, throws 'RateLimitedException'.
--
-- ==== __Examples__
--
-- @
-- rl <- newRateLimiter (RateLimiterConfig 100 500)
--
-- -- This will succeed up to 500 times in quick succession,
-- -- then at a rate of 100 per second
-- result <- withRateLimit rl $ httpGet "https://api.example.com"
-- @
--
-- @since 0.1.0.0
withRateLimit
  :: (MonadIO m, MonadCatch m)
  => RateLimiter
  -> m a
  -> m a
withRateLimit rl action = do
  acquired <- liftIO $ tryConsume rl 1.0
  if acquired
    then action
    else throwM RateLimitedException

-- | Try to consume tokens from the rate limiter.
--
-- Returns 'True' if the tokens were successfully consumed,
-- 'False' if insufficient tokens are available.
--
-- This is a low-level operation. Prefer 'withRateLimit' for most use cases.
--
-- ==== __Examples__
--
-- @
-- -- Try to consume 1 token
-- success <- tryConsume rl 1.0
-- when success $ do
--   performOperation
--
-- -- Try to consume multiple tokens for batch operations
-- success <- tryConsume rl 10.0
-- @
--
-- @since 0.1.0.0
tryConsume :: RateLimiter -> Double -> IO Bool
tryConsume rl amount = do
  now <- getCurrentTime
  atomically $ do
    lastTime <- readTVar (rlLastRefill rl)
    currentTokens <- readTVar (rlTokens rl)

    -- Calculate tokens to add based on elapsed time
    let elapsed = realToFrac (diffUTCTime now lastTime) :: Double
        rate = rlcRate (rlConfig rl)
        capacity = rlcCapacity (rlConfig rl)
        tokensToAdd = elapsed * rate
        newTokens = min capacity (currentTokens + tokensToAdd)

    -- Try to consume tokens
    if newTokens >= amount
      then do
        writeTVar (rlTokens rl) (newTokens - amount)
        writeTVar (rlLastRefill rl) now
        pure True
      else do
        -- Still update time and tokens even on failure
        writeTVar (rlTokens rl) newTokens
        writeTVar (rlLastRefill rl) now
        pure False

-- | Get the current number of available tokens.
--
-- This is useful for monitoring and debugging.
--
-- @since 0.1.0.0
getAvailableTokens :: RateLimiter -> IO Double
getAvailableTokens rl = do
  now <- getCurrentTime
  atomically $ do
    lastTime <- readTVar (rlLastRefill rl)
    currentTokens <- readTVar (rlTokens rl)

    let elapsed = realToFrac (diffUTCTime now lastTime) :: Double
        rate = rlcRate (rlConfig rl)
        capacity = rlcCapacity (rlConfig rl)
        tokensToAdd = elapsed * rate
        newTokens = min capacity (currentTokens + tokensToAdd)

    pure newTokens
