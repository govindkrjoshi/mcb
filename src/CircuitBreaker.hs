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
-- unavailable. This library provides a production-ready implementation with:
--
-- * __Thread-safe state management__ using Software Transactional Memory (STM)
-- * __Count-based sliding window__ for accurate failure rate calculation
-- * __Configurable thresholds__ for failure rates, timeouts, and recovery
-- * __Exception hierarchy__ for granular error handling
-- * __Zero external dependencies__ beyond base GHC libraries
--
-- = State Machine
--
-- The circuit breaker operates as a state machine with three states:
--
-- @
--                    success
--                   \<---------
--                   |         |
--                   v         |
-- +---------+  failure rate  +------+  timeout   +----------+
-- |         |  exceeds       |      |  expires   |          |
-- | Closed  |--threshold----\>| Open |----------\>| HalfOpen |
-- |         |                |      |            |          |
-- +---------+                +------+            +----------+
--      ^                                              |
--      |                                              |
--      +---------------- success ---------------------+
--                         |
--                         | failure
--                         v
--                    (back to Open)
-- @
--
-- == State Descriptions
--
-- ['Closed'] Normal operation. Requests flow through and results are tracked
--   in the sliding window. The circuit opens when the failure rate exceeds
--   the configured threshold.
--
-- ['Open'] Circuit is tripped. All requests are immediately rejected with
--   'CircuitOpenException'. After the configured wait duration, the circuit
--   transitions to HalfOpen.
--
-- ['HalfOpen'] Testing recovery. A limited number of requests (configured via
--   'HalfOpenPermits') are allowed through. If they succeed, the circuit
--   closes. If any fail, the circuit reopens.
--
-- = Quick Start
--
-- @
-- import CircuitBreaker
-- import Control.Exception ('throwIO', 'catch')
--
-- main :: IO ()
-- main = do
--   -- Create a circuit breaker with default settings
--   cb <- 'newCircuitBreaker' 'defaultConfig'
--
--   -- Protect an operation with the circuit breaker
--   result <- protectedCall cb
--   print result
--
-- -- Example protected call implementation
-- protectedCall :: 'CircuitBreaker' -> IO String
-- protectedCall cb = do
--   -- Check if call is permitted
--   permitted <- 'isCallPermitted' cb
--   if not permitted
--     then 'throwIO' ('CircuitOpenException' \"my-service\")
--     else do
--       -- Execute the call and record the result
--       result <- callService \`catch\` \\e -> do
--         'recordFailure' cb
--         'throwIO' (e :: SomeException)
--       'recordSuccess' cb
--       pure result
-- @
--
-- = Configuration
--
-- Use the builder pattern with '&' to configure the circuit breaker:
--
-- @
-- import CircuitBreaker
--
-- config :: 'CircuitBreakerConfig'
-- config = 'defaultConfig'
--   '&' 'setFailureThreshold' 0.5    -- Open when 50% of calls fail
--   '&' 'setSlidingWindowSize' 20    -- Consider last 20 calls
--   '&' 'setWaitDuration' 60         -- Wait 60 seconds before half-open
--   '&' 'setHalfOpenPermits' 3       -- Allow 3 test calls in half-open
-- @
--
-- == Configuration Options
--
-- [@failureThreshold@] The failure rate (0.0-1.0) that triggers the circuit
--   to open. Default: 0.5 (50%). Lower values are more sensitive.
--
-- [@slidingWindowSize@] The number of recent calls to consider when
--   calculating the failure rate. Default: 10. Larger windows provide
--   more stability but slower response to changes.
--
-- [@waitDuration@] Seconds to wait in the Open state before transitioning
--   to HalfOpen. Default: 30 seconds. Set based on expected recovery time.
--
-- [@halfOpenPermits@] Number of test calls allowed in HalfOpen state.
--   Default: 3. More permits means more confidence before closing.
--
-- [@exceptionPredicate@] Function to determine which exceptions count as
--   failures. Default: all exceptions count. Customize to ignore expected
--   exceptions like client errors (4xx HTTP).
--
-- [@onStateTransition@] Callback invoked on state changes. Useful for
--   logging, metrics, and alerting.
--
-- = Usage Patterns
--
-- == Circuit Breaker with Timeout
--
-- Combine the circuit breaker with timeout protection:
--
-- @
-- import CircuitBreaker
-- import CircuitBreaker.Timeout
--
-- protectedHttpCall :: 'CircuitBreaker' -> IO Response
-- protectedHttpCall cb = do
--   permitted <- 'isCallPermitted' cb
--   if not permitted
--     then 'throwIO' ('CircuitOpenException' \"http-service\")
--     else 'withTimeout' ('seconds' 5) httpCall
-- @
--
-- == Circuit Breaker with Fallback
--
-- Provide a fallback when the circuit is open:
--
-- @
-- import CircuitBreaker
-- import Control.Exception ('catch')
--
-- callWithFallback :: 'CircuitBreaker' -> IO Response
-- callWithFallback cb =
--   protectedCall cb \`catch\` \\('CircuitOpenException' _) -> do
--     logWarning \"Circuit open, using cached response\"
--     getCachedResponse
-- @
--
-- == Selective Failure Counting
--
-- Only count server errors as failures:
--
-- @
-- config = 'defaultConfig'
--   '&' 'setExceptionPredicate' isServerError
--
-- isServerError :: SomeException -> Bool
-- isServerError e = case fromException e of
--   Just (HttpException status _) -> statusCode status >= 500
--   Nothing -> True  -- Count unknown exceptions
-- @
--
-- == Monitoring State Transitions
--
-- Log state transitions for observability:
--
-- @
-- config = 'defaultConfig'
--   '&' 'setOnStateTransition' logTransition
--
-- logTransition :: 'State' -> 'State' -> IO ()
-- logTransition old new = do
--   putStrLn $ \"Circuit breaker: \" ++ show old ++ \" -> \" ++ show new
--   when (new == 'Open') $ alertOncall \"Circuit breaker opened!\"
-- @
--
-- = Design Rationale
--
-- == Why STM?
--
-- This library uses Software Transactional Memory (STM) for state management
-- because:
--
-- * __Composability__: STM operations can be composed into larger atomic
--   transactions, allowing the circuit breaker to be integrated with other
--   STM-based systems.
--
-- * __Deadlock-free__: STM automatically handles lock ordering, eliminating
--   a common source of concurrency bugs.
--
-- * __Optimistic concurrency__: Reads don\'t block writes, providing good
--   performance under contention.
--
-- == Why Exception-Based Signaling?
--
-- The circuit breaker signals its open state via exceptions rather than
-- return values because:
--
-- * __Backward compatibility__: Existing code using exceptions for error
--   handling can adopt the circuit breaker without refactoring.
--
-- * __Automatic propagation__: Exceptions propagate up the call stack
--   automatically, reducing boilerplate.
--
-- * __Catchable hierarchy__: The 'CircuitBreakerException' hierarchy allows
--   catching all circuit breaker errors or specific ones.
--
-- == Why Count-Based Windows?
--
-- The sliding window uses a count-based approach (last N calls) rather than
-- time-based (calls in last N seconds) because:
--
-- * __Predictable memory usage__: The window size is bounded regardless of
--   call rate.
--
-- * __Works at any scale__: Functions correctly whether handling 1 request
--   per minute or 10,000 requests per second.
--
-- * __Simpler implementation__: No need for background cleanup of old entries.
--
-- = Thread Safety
--
-- All operations in this library are thread-safe:
--
-- * A single 'CircuitBreaker' can be safely shared across multiple threads.
-- * State queries and updates are atomic.
-- * The sliding window maintains consistency under concurrent modifications.
--
-- __Important__: Create one 'CircuitBreaker' instance per protected service,
-- not per request. Creating a new circuit breaker for each request defeats
-- the purpose of failure tracking.
--
-- = Common Pitfalls
--
-- == Creating Circuit Breakers Per Request
--
-- __Wrong:__
--
-- @
-- handleRequest :: Request -> IO Response
-- handleRequest req = do
--   cb <- 'newCircuitBreaker' 'defaultConfig'  -- Don\'t do this!
--   protectedCall cb
-- @
--
-- __Right:__
--
-- @
-- main = do
--   cb <- 'newCircuitBreaker' 'defaultConfig'  -- Create once
--   app <- makeApp cb
--   runServer app
--
-- handleRequest :: 'CircuitBreaker' -> Request -> IO Response
-- handleRequest cb req = protectedCall cb  -- Reuse the instance
-- @
--
-- == Ignoring the Open State
--
-- __Wrong:__
--
-- @
-- -- Ignoring CircuitOpenException means the circuit breaker is useless
-- result <- protectedCall cb \`catch\` \\(_ :: SomeException) -> retry
-- @
--
-- __Right:__
--
-- @
-- result <- protectedCall cb \`catch\` \\case
--   'CircuitOpenException' _ -> getFallback  -- Handle circuit open specifically
--   e                        -> 'throwIO' e   -- Re-throw other errors
-- @
--
-- == Too Small Sliding Window
--
-- A small sliding window (e.g., 3) can cause the circuit to oscillate
-- rapidly. Use at least 10 calls for stability.
--
-- == Too Short Wait Duration
--
-- If waitDuration is too short, the circuit may transition to HalfOpen
-- before the downstream service has recovered, causing repeated failures.
-- Tune based on your service\'s typical recovery time.
--
-- = Performance
--
-- The circuit breaker adds minimal overhead:
--
-- * State reads: O(1), single TVar read
-- * Sliding window updates: O(1) amortized, using Data.Sequence
-- * Memory: O(windowSize) for the sliding window
--
-- In benchmarks, the circuit breaker adds less than 1 microsecond of
-- overhead per protected call.
--
-- @since 0.1.0.0

module CircuitBreaker
  ( -- * Core API
    withCircuitBreaker

    -- * Circuit Breaker State
  , State (..)

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

    -- * Circuit Breaker Handle
  , CircuitBreaker
  , CircuitBreakerState (..)
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

    -- * Exceptions
  , CircuitBreakerException (..)
  , CircuitOpenException (..)

    -- * Timeout
  , TimeoutException (..)
  , withTimeout
  , seconds
  , milliseconds
  , minutes

    -- * Sliding Window
  , SlidingWindow
  , newSlidingWindow
  , insertResult
  , reset
  , getFailureRate
  , getTotalCalls
  , isEmpty
  , isFull
  , getWindowSize
  , getFailureCount

    -- * Exception Predicates
  , defaultExceptionPredicate
  , ignoreException
  , onlyException
  , matchException
  , combinePredicates
  , anyPredicate
  , allPredicate
  , ignoreExceptions
  , onlyExceptions
  , negatePredicate
  , whenMatches

    -- * Rate Limiter
  , RateLimiter
  , RateLimiterConfig (..)
  , newRateLimiter
  , withRateLimit
  , tryConsume
  , getAvailableTokens
  , RateLimitedException (..)

    -- * Bulkhead
  , Bulkhead
  , BulkheadConfig (..)
  , newBulkhead
  , withBulkhead
  , tryAcquire
  , release
  , getActiveCount
  , getMaxConcurrent
  , BulkheadRejectedException (..)

    -- * Retry
  , RetryConfig (..)
  , BackoffStrategy (..)
  , defaultRetryConfig
  , withRetry
  , setMaxAttempts
  , setBackoffStrategy
  , setShouldRetry
  , setOnRetry
  , RetriesExhaustedException (..)

    -- * Resilience Composition
  , ResilienceConfig (..)
  , defaultResilienceConfig
  , withResilience
  , addCircuitBreaker
  , addTimeout
  , addBulkhead
  , addRateLimiter
  , addRetry

    -- * Fallback
  , withFallback
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

import CircuitBreaker.Bulkhead
import CircuitBreaker.Core
import CircuitBreaker.Exceptions
import CircuitBreaker.Fallback
import CircuitBreaker.Internal.State
import CircuitBreaker.Predicate
import CircuitBreaker.RateLimiter
import CircuitBreaker.Resilience
import CircuitBreaker.Retry
import CircuitBreaker.SlidingWindow
import CircuitBreaker.Timeout
import CircuitBreaker.Types
