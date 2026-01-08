{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : CircuitBreaker.Fallback
-- Description : Fallback pattern for graceful degradation
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides fallback combinators for graceful degradation when
-- operations fail. The fallback pattern allows you to specify an alternative
-- action or value to use when the primary action throws an exception.
--
-- = Usage
--
-- @
-- import CircuitBreaker
--
-- -- Simple fallback with a default value
-- result <- withFallback (pure defaultValue) riskyOperation
--
-- -- Fallback with an alternative action
-- result <- withFallback getCachedData fetchFromRemote
--
-- -- Fallback only on specific exceptions
-- result <- withFallbackOn isCircuitOpenException getFromCache $
--             callExternalService
-- @
--
-- = Composition with Other Patterns
--
-- Fallback is typically applied as the outermost layer:
--
-- @
-- result <- withFallback (pure cachedResponse) $
--             withRetry defaultRetryConfig $
--               withCircuitBreaker cb $
--                 withTimeout (seconds 5) $
--                   callExternalService
-- @
--
-- This ensures that if all retries fail or the circuit opens, the fallback
-- is used as a last resort.
--
-- @since 0.1.0.0

module CircuitBreaker.Fallback
  ( -- * Fallback Combinators
    withFallback
  , withFallbackOn
  , withFallbackValue
  , withFallbackValueOn

    -- * Fallback Predicates
  , isCircuitBreakerException
  , isCircuitOpenException
  , isTimeoutException
  , isRateLimitedException
  , isBulkheadRejectedException
  , isRetriesExhaustedException
  ) where

import Control.Exception (SomeException, fromException)
import Control.Monad.Catch (MonadCatch, catch, throwM, try)

import CircuitBreaker.Exceptions
  ( BulkheadRejectedException
  , CircuitBreakerException
  , CircuitOpenException
  , RateLimitedException
  , RetriesExhaustedException
  )
import CircuitBreaker.Timeout (TimeoutException)

-- | Execute an action with a fallback if the action throws any exception.
--
-- This is the most general fallback combinator. If the primary action throws
-- any exception (synchronous), the fallback action is executed instead.
--
-- ==== __Examples__
--
-- @
-- -- Use cached data if the network call fails
-- result <- withFallback getCachedData fetchFromNetwork
--
-- -- Return a default value on failure
-- result <- withFallback (pure "default") riskyComputation
-- @
--
-- ==== __Exception Handling__
--
-- If both the primary action and the fallback action throw exceptions,
-- the exception from the fallback action is propagated.
--
-- @since 0.1.0.0
withFallback
  :: MonadCatch m
  => m a        -- ^ Fallback action to execute if primary fails
  -> m a        -- ^ Primary action to attempt
  -> m a
withFallback fallbackAction primaryAction =
  primaryAction `catch` \(_ :: SomeException) -> fallbackAction

-- | Execute an action with a fallback only when specific exceptions occur.
--
-- This combinator allows fine-grained control over which exceptions trigger
-- the fallback. Exceptions that don't match the predicate are re-thrown.
--
-- ==== __Examples__
--
-- @
-- -- Only fallback on circuit breaker exceptions
-- result <- withFallbackOn isCircuitBreakerException getCachedData $
--             callExternalService
--
-- -- Only fallback on timeout or rate limit
-- let shouldFallback e = isTimeoutException e || isRateLimitedException e
-- result <- withFallbackOn shouldFallback getDefault fetchData
-- @
--
-- @since 0.1.0.0
withFallbackOn
  :: forall m a. MonadCatch m
  => (SomeException -> Bool)  -- ^ Predicate to determine if fallback should be used
  -> m a                      -- ^ Fallback action to execute if predicate matches
  -> m a                      -- ^ Primary action to attempt
  -> m a
withFallbackOn shouldFallback fallbackAction primaryAction = do
  result <- try primaryAction
  case result of
    Right value -> pure value
    Left ex
      | shouldFallback ex -> fallbackAction
      | otherwise -> throwM ex

-- | Execute an action with a pure fallback value if the action throws any exception.
--
-- This is a convenience combinator for when the fallback is a pure value
-- rather than an IO action.
--
-- ==== __Examples__
--
-- @
-- -- Return empty list on failure
-- result <- withFallbackValue [] fetchItems
--
-- -- Return Nothing on failure
-- result <- withFallbackValue Nothing (Just \<$\> fetchData)
-- @
--
-- @since 0.1.0.0
withFallbackValue
  :: MonadCatch m
  => a          -- ^ Pure fallback value to return if primary fails
  -> m a        -- ^ Primary action to attempt
  -> m a
withFallbackValue value = withFallback (pure value)

-- | Execute an action with a pure fallback value only when specific exceptions occur.
--
-- This combines 'withFallbackOn' with a pure fallback value.
--
-- ==== __Examples__
--
-- @
-- -- Return default only on circuit open
-- result <- withFallbackValueOn isCircuitOpenException defaultResponse $
--             callService
-- @
--
-- @since 0.1.0.0
withFallbackValueOn
  :: MonadCatch m
  => (SomeException -> Bool)  -- ^ Predicate to determine if fallback should be used
  -> a                        -- ^ Pure fallback value to return if predicate matches
  -> m a                      -- ^ Primary action to attempt
  -> m a
withFallbackValueOn shouldFallback value = withFallbackOn shouldFallback (pure value)

-- | Check if an exception is any circuit breaker exception.
--
-- This matches 'CircuitOpenException', 'TimeoutException', 'RateLimitedException',
-- 'BulkheadRejectedException', and 'RetriesExhaustedException'.
--
-- @since 0.1.0.0
isCircuitBreakerException :: SomeException -> Bool
isCircuitBreakerException ex =
  case fromException ex :: Maybe CircuitBreakerException of
    Just _ -> True
    Nothing -> False

-- | Check if an exception is specifically a 'CircuitOpenException'.
--
-- @since 0.1.0.0
isCircuitOpenException :: SomeException -> Bool
isCircuitOpenException ex =
  case fromException ex :: Maybe CircuitOpenException of
    Just _ -> True
    Nothing -> False

-- | Check if an exception is specifically a 'TimeoutException'.
--
-- @since 0.1.0.0
isTimeoutException :: SomeException -> Bool
isTimeoutException ex =
  case fromException ex :: Maybe TimeoutException of
    Just _ -> True
    Nothing -> False

-- | Check if an exception is specifically a 'RateLimitedException'.
--
-- @since 0.1.0.0
isRateLimitedException :: SomeException -> Bool
isRateLimitedException ex =
  case fromException ex :: Maybe RateLimitedException of
    Just _ -> True
    Nothing -> False

-- | Check if an exception is specifically a 'BulkheadRejectedException'.
--
-- @since 0.1.0.0
isBulkheadRejectedException :: SomeException -> Bool
isBulkheadRejectedException ex =
  case fromException ex :: Maybe BulkheadRejectedException of
    Just _ -> True
    Nothing -> False

-- | Check if an exception is specifically a 'RetriesExhaustedException'.
--
-- @since 0.1.0.0
isRetriesExhaustedException :: SomeException -> Bool
isRetriesExhaustedException ex =
  case fromException ex :: Maybe RetriesExhaustedException of
    Just _ -> True
    Nothing -> False
