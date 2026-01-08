{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : CircuitBreaker.Predicate
-- Description : Exception predicate helpers for circuit breakers
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides helper functions for creating exception predicates
-- used by the circuit breaker to determine which exceptions count as failures.
--
-- = Overview
--
-- By default, all exceptions count as circuit breaker failures. However,
-- in many applications you want to distinguish between:
--
-- * __Service failures__ (5xx errors, timeouts): Should count as failures
-- * __Client errors__ (4xx errors): Usually should NOT count as failures
-- * __Expected business exceptions__ (not found, validation errors): Should NOT count
--
-- This module provides combinators to build predicates for these scenarios.
--
-- = Usage
--
-- @
-- import CircuitBreaker
-- import CircuitBreaker.Predicate
-- import Data.Proxy (Proxy(..))
--
-- -- Ignore NotFound exceptions (they don't count as failures)
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('ignoreException' (Proxy :: Proxy NotFoundException))
--
-- -- Only count ServerException as failures
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('onlyException' (Proxy :: Proxy ServerException))
--
-- -- Ignore multiple exception types
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('ignoreExceptions'
--       [ 'ignoreException' (Proxy :: Proxy NotFoundException)
--       , 'ignoreException' (Proxy :: Proxy ValidationException)
--       ])
-- @
--
-- = Writing Custom Predicates
--
-- Predicates have the type @'SomeException' -> 'Bool'@ where:
--
-- * 'True' means the exception counts as a failure
-- * 'False' means the exception is ignored (not counted as failure)
--
-- Use 'Control.Exception.fromException' to safely cast and match:
--
-- @
-- isServerError :: SomeException -> Bool
-- isServerError e = case fromException e of
--   Just (HttpException status _) -> statusCode status >= 500
--   Nothing -> True  -- Unknown exceptions count as failures
-- @
--
-- __Important__: Predicates should be pure, fast, and should not throw
-- exceptions themselves.
--
-- @since 0.1.0.0

module CircuitBreaker.Predicate
  ( -- * Default Predicate
    defaultExceptionPredicate

    -- * Single Exception Type Predicates
  , ignoreException
  , onlyException
  , matchException

    -- * Combining Predicates
  , combinePredicates
  , anyPredicate
  , allPredicate
  , ignoreExceptions
  , onlyExceptions

    -- * Utility Functions
  , negatePredicate
  , whenMatches
  ) where

import Control.Exception (Exception, SomeException, fromException)
import Data.Proxy (Proxy (..))

-- | The default exception predicate that treats all exceptions as failures.
--
-- This is used by 'defaultConfig' and returns 'True' for any exception,
-- meaning all exceptions will be recorded as circuit breaker failures.
--
-- ==== __Examples__
--
-- @
-- -- These are equivalent:
-- config1 = 'defaultConfig'
-- config2 = 'defaultConfig' '&' 'setExceptionPredicate' 'defaultExceptionPredicate'
-- @
--
-- @since 0.1.0.0
defaultExceptionPredicate :: SomeException -> Bool
defaultExceptionPredicate = const True

-- | Create a predicate that ignores (does not count as failure) a specific
-- exception type.
--
-- Use this when you want most exceptions to count as failures, but want to
-- ignore certain expected exception types.
--
-- ==== __Examples__
--
-- @
-- data NotFoundException = NotFoundException
--   deriving (Show, Exception)
--
-- -- NotFound exceptions won't open the circuit
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('ignoreException' (Proxy :: Proxy NotFoundException))
-- @
--
-- @since 0.1.0.0
ignoreException :: forall e. Exception e => Proxy e -> SomeException -> Bool
ignoreException _ ex = case fromException ex :: Maybe e of
  Just _  -> False  -- Matched exception type, ignore it (not a failure)
  Nothing -> True   -- Different exception type, count as failure

-- | Create a predicate that only counts a specific exception type as failure.
--
-- Use this when you only want certain exception types to affect the circuit
-- breaker, ignoring all others.
--
-- ==== __Examples__
--
-- @
-- data ServerException = ServerException String
--   deriving (Show, Exception)
--
-- -- Only ServerException will count as failures
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('onlyException' (Proxy :: Proxy ServerException))
-- @
--
-- @since 0.1.0.0
onlyException :: forall e. Exception e => Proxy e -> SomeException -> Bool
onlyException _ ex = case fromException ex :: Maybe e of
  Just _  -> True   -- Matched exception type, count as failure
  Nothing -> False  -- Different exception type, ignore it

-- | Check if an exception matches a specific type.
--
-- This is a lower-level helper that returns 'True' if the exception matches
-- the given type, 'False' otherwise. Unlike 'ignoreException' and
-- 'onlyException', this function's result is not intended to be used directly
-- as a predicate, but rather as a building block.
--
-- ==== __Examples__
--
-- @
-- data TimeoutException = TimeoutException
--   deriving (Show, Exception)
--
-- -- Custom predicate using matchException
-- myPredicate :: SomeException -> Bool
-- myPredicate ex
--   | 'matchException' (Proxy :: Proxy TimeoutException) ex = True  -- Timeouts count
--   | 'matchException' (Proxy :: Proxy NotFoundException) ex = False -- Not found ignored
--   | otherwise = True  -- Default: count as failure
-- @
--
-- @since 0.1.0.0
matchException :: forall e. Exception e => Proxy e -> SomeException -> Bool
matchException _ ex = case fromException ex :: Maybe e of
  Just _  -> True
  Nothing -> False

-- | Combine multiple predicates with AND logic.
--
-- An exception counts as a failure only if ALL predicates return 'True'.
-- This is useful when you need multiple conditions to be satisfied.
--
-- ==== __Examples__
--
-- @
-- -- Only count as failure if exception is both a ServerException
-- -- AND passes some additional check
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('combinePredicates'
--       [ 'onlyException' (Proxy :: Proxy ServerException)
--       , isRecoverableError
--       ])
-- @
--
-- @since 0.1.0.0
combinePredicates :: [SomeException -> Bool] -> SomeException -> Bool
combinePredicates predicates ex = all (\p -> p ex) predicates

-- | Alias for 'combinePredicates' with clearer semantics.
--
-- An exception counts as a failure only if ALL predicates return 'True'.
--
-- @since 0.1.0.0
allPredicate :: [SomeException -> Bool] -> SomeException -> Bool
allPredicate = combinePredicates

-- | Combine multiple predicates with OR logic.
--
-- An exception counts as a failure if ANY predicate returns 'True'.
-- This is useful when you want to count multiple types of exceptions.
--
-- ==== __Examples__
--
-- @
-- -- Count as failure if it's either a ServerException OR a TimeoutException
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('anyPredicate'
--       [ 'onlyException' (Proxy :: Proxy ServerException)
--       , 'onlyException' (Proxy :: Proxy TimeoutException)
--       ])
-- @
--
-- @since 0.1.0.0
anyPredicate :: [SomeException -> Bool] -> SomeException -> Bool
anyPredicate predicates ex = any (\p -> p ex) predicates

-- | Create a predicate that ignores multiple exception types.
--
-- This is a convenience function for combining multiple 'ignoreException'
-- predicates. An exception counts as a failure unless it matches ANY of
-- the ignored types.
--
-- ==== __Examples__
--
-- @
-- -- Ignore NotFound and ValidationException, count everything else
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('ignoreExceptions'
--       [ 'ignoreException' (Proxy :: Proxy NotFoundException)
--       , 'ignoreException' (Proxy :: Proxy ValidationException)
--       ])
-- @
--
-- __Note__: This uses AND logic - an exception is ignored only if it passes
-- ALL the ignore predicates. Since each predicate returns 'False' only for
-- its specific type, this effectively creates an "ignore any of these" behavior.
--
-- @since 0.1.0.0
ignoreExceptions :: [SomeException -> Bool] -> SomeException -> Bool
ignoreExceptions = combinePredicates

-- | Create a predicate that only counts specific exception types as failures.
--
-- This is a convenience function for combining multiple 'onlyException'
-- predicates. An exception counts as a failure if it matches ANY of
-- the specified types.
--
-- ==== __Examples__
--
-- @
-- -- Only count ServerException or TimeoutException as failures
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('onlyExceptions'
--       [ 'onlyException' (Proxy :: Proxy ServerException)
--       , 'onlyException' (Proxy :: Proxy TimeoutException)
--       ])
-- @
--
-- @since 0.1.0.0
onlyExceptions :: [SomeException -> Bool] -> SomeException -> Bool
onlyExceptions = anyPredicate

-- | Negate a predicate.
--
-- Flips the result of a predicate: failures become non-failures and vice versa.
--
-- ==== __Examples__
--
-- @
-- -- Count as failure everything EXCEPT what 'onlyException' would match
-- -- (This is equivalent to 'ignoreException')
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' ('negatePredicate' ('onlyException' (Proxy :: Proxy SomeExc)))
-- @
--
-- @since 0.1.0.0
negatePredicate :: (SomeException -> Bool) -> SomeException -> Bool
negatePredicate p ex = not (p ex)

-- | Apply a custom check when an exception matches a specific type.
--
-- This function allows you to inspect the actual exception value and make
-- decisions based on its contents.
--
-- ==== __Examples__
--
-- @
-- data HttpException = HttpException Int String
--   deriving (Show, Exception)
--
-- -- Only count 5xx errors as failures, not 4xx
-- isServerError :: SomeException -> Bool
-- isServerError = 'whenMatches' (Proxy :: Proxy HttpException)
--   (\\(HttpException status _) -> status >= 500)
--   True  -- Default for non-matching exceptions
--
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' isServerError
-- @
--
-- @since 0.1.0.0
whenMatches
  :: forall e. Exception e
  => Proxy e              -- ^ The exception type to match
  -> (e -> Bool)          -- ^ Predicate to apply if exception matches
  -> Bool                 -- ^ Default result for non-matching exceptions
  -> SomeException        -- ^ The exception to test
  -> Bool
whenMatches _ check defaultResult ex = case fromException ex :: Maybe e of
  Just e' -> check e'
  Nothing -> defaultResult
