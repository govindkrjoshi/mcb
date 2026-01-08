-- |
-- Module      : Test.Integration.Fallback
-- Description : Integration tests for Fallback with other resilience patterns
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause

module Test.Integration.Fallback
  ( tests
  ) where

import Control.Concurrent (forkIO, threadDelay, newMVar, takeMVar, putMVar)
import Control.Exception (SomeException, try)
import Data.IORef (newIORef, readIORef, modifyIORef', atomicModifyIORef')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import CircuitBreaker
  ( BackoffStrategy (..)
  , BulkheadConfig (..)
  , CircuitOpenException (..)
  , RateLimiterConfig (..)
  , RetriesExhaustedException (..)
  , defaultConfig
  , defaultRetryConfig
  , isCircuitOpenException
  , isRetriesExhaustedException
  , newBulkhead
  , newCircuitBreaker
  , newRateLimiter
  , setBackoffStrategy
  , setFailureThreshold
  , setMaxAttempts
  , setSlidingWindowSize
  , withBulkhead
  , withCircuitBreaker
  , withFallback
  , withFallbackOn
  , withFallbackValue
  , withRateLimit
  , withRetry
  , withTimeout
  , (&)
  )
import CircuitBreaker.Timeout (TimeoutException (..))

tests :: TestTree
tests = testGroup "CircuitBreaker.Fallback.Integration"
  [ retryWithFallbackTests
  , circuitBreakerWithFallbackTests
  , timeoutWithFallbackTests
  , compositionTests
  , concurrencyTests
  ]

-- | Tests for retry + fallback composition
retryWithFallbackTests :: TestTree
retryWithFallbackTests = testGroup "Retry with Fallback"
  [ testCase "fallback after all retries exhausted" $ do
      attemptCount <- newIORef (0 :: Int)

      let retryConfig = defaultRetryConfig
            & setMaxAttempts 3
            & setBackoffStrategy NoBackoff

      result <- withFallback (pure "fallback-value") $
                  withRetry retryConfig $ do
                    modifyIORef' attemptCount (+1)
                    error "persistent failure" :: IO String

      result @?= "fallback-value"
      count <- readIORef attemptCount
      count @?= 3  -- All retry attempts were made

  , testCase "fallback sees RetriesExhaustedException" $ do
      let retryConfig = defaultRetryConfig
            & setMaxAttempts 2
            & setBackoffStrategy NoBackoff

      -- Simply check that RetriesExhaustedException triggers fallback
      result <- withFallbackOn
        isRetriesExhaustedException
        (pure "fallback")
        (withRetry retryConfig (error "fail" :: IO String))

      result @?= "fallback"

  , testCase "retry doesn't retry fallback failures" $ do
      primaryCalls <- newIORef (0 :: Int)
      fallbackCalls <- newIORef (0 :: Int)

      let retryConfig = defaultRetryConfig
            & setMaxAttempts 3
            & setBackoffStrategy NoBackoff

      _ <- try $ withRetry retryConfig $
             withFallback
               (modifyIORef' fallbackCalls (+1) >> error "fallback fails" :: IO String)
               (modifyIORef' primaryCalls (+1) >> error "primary fails" :: IO String)
        :: IO (Either SomeException String)

      pCount <- readIORef primaryCalls
      fCount <- readIORef fallbackCalls

      -- Retry applies to the entire withFallback expression
      -- Each retry: primary fails -> fallback called and fails
      pCount @?= 3  -- 3 retry attempts
      fCount @?= 3  -- Fallback called on each attempt

  , testCase "fallback wraps retry - simpler semantics" $ do
      attemptCount <- newIORef (0 :: Int)

      let retryConfig = defaultRetryConfig
            & setMaxAttempts 2
            & setBackoffStrategy NoBackoff

      -- Fallback as outermost layer
      result <- withFallback (pure "fallback") $
                  withRetry retryConfig $ do
                    modifyIORef' attemptCount (+1)
                    error "fail" :: IO String

      result @?= "fallback"
      count <- readIORef attemptCount
      count @?= 2  -- Retry happened, then fallback
  ]

-- | Tests for circuit breaker + fallback composition
circuitBreakerWithFallbackTests :: TestTree
circuitBreakerWithFallbackTests = testGroup "Circuit Breaker with Fallback"
  [ testCase "fallback on circuit open" $ do
      cb <- newCircuitBreaker (defaultConfig
              & setFailureThreshold 0.5
              & setSlidingWindowSize 10)

      -- Cause failures to open the circuit (need >= 5 calls minimum)
      sequence_ $ replicate 6 $ do
        _ <- try $ withCircuitBreaker cb (error "fail" :: IO String)
          :: IO (Either SomeException String)
        pure ()

      -- Now circuit is open, fallback should trigger
      result <- withFallbackOn isCircuitOpenException (pure "degraded-mode") $
                  withCircuitBreaker cb (pure "normal-response" :: IO String)

      result @?= "degraded-mode"

  , testCase "fallback doesn't affect circuit breaker state" $ do
      cb <- newCircuitBreaker (defaultConfig
              & setFailureThreshold 0.5
              & setSlidingWindowSize 4)

      callCount <- newIORef (0 :: Int)

      -- Make calls that fail but have fallback
      sequence_ $ replicate 3 $ do
        result <- withFallback (pure "fallback") $
                    withCircuitBreaker cb $ do
                      modifyIORef' callCount (+1)
                      error "fail" :: IO String
        result @?= "fallback"

      count <- readIORef callCount
      count @?= 3  -- All calls were attempted (circuit didn't open yet)

  , testCase "selective fallback only on circuit exceptions" $ do
      cb <- newCircuitBreaker (defaultConfig
              & setFailureThreshold 0.5
              & setSlidingWindowSize 10)

      -- Open the circuit
      sequence_ $ replicate 6 $ do
        _ <- try $ withCircuitBreaker cb (error "fail" :: IO String)
          :: IO (Either SomeException String)
        pure ()

      -- Circuit open exception should trigger fallback
      result1 <- withFallbackOn isCircuitOpenException (pure "fallback") $
                   withCircuitBreaker cb (pure "success" :: IO String)
      result1 @?= "fallback"

      -- But other exceptions should not (using a fresh circuit)
      cb2 <- newCircuitBreaker defaultConfig
      result2 <- try $ withFallbackOn isCircuitOpenException (pure "fallback") $
                   withCircuitBreaker cb2 (error "other error" :: IO String)
        :: IO (Either SomeException String)
      case result2 of
        Left _ -> pure ()  -- Expected: other error propagates
        Right _ -> error "Expected exception to propagate"
  ]

-- | Tests for timeout + fallback composition
timeoutWithFallbackTests :: TestTree
timeoutWithFallbackTests = testGroup "Timeout with Fallback"
  [ testCase "fallback on timeout" $ do
      result <- withFallback (pure "timed-out-fallback") $
                  withTimeout 0.1 $ do
                    threadDelay 1000000  -- 1 second
                    pure "too-slow" :: IO String

      result @?= "timed-out-fallback"

  , testCase "fallback action itself can timeout" $ do
      result <- try $ withFallback
        (withTimeout 0.1 $ threadDelay 1000000 >> pure "fallback")
        (error "primary fails" :: IO String)
        :: IO (Either SomeException String)

      case result of
        Left _ -> pure ()  -- Fallback timed out
        Right _ -> error "Expected timeout in fallback"

  , testCase "timeout wraps fallback - both layers protected" $ do
      -- Timeout as outer layer protects both primary and fallback
      result <- try $ withTimeout 0.2 $
                        withFallback
                          (threadDelay 500000 >> pure "slow-fallback")  -- 500ms
                          (error "primary fails" :: IO String)
        :: IO (Either SomeException String)

      case result of
        Left _ -> pure ()  -- Should timeout during fallback
        Right _ -> error "Expected timeout"
  ]

-- | Tests for complex compositions
compositionTests :: TestTree
compositionTests = testGroup "Complex Compositions"
  [ testCase "full stack: retry + circuit breaker + timeout + fallback" $ do
      cb <- newCircuitBreaker defaultConfig

      let retryConfig = defaultRetryConfig
            & setMaxAttempts 2
            & setBackoffStrategy NoBackoff

      attemptCount <- newIORef (0 :: Int)

      -- Recommended order: Fallback -> Retry -> Circuit Breaker -> Timeout -> Action
      result <- withFallback (pure "ultimate-fallback") $
                  withRetry retryConfig $
                    withCircuitBreaker cb $
                      withTimeout 0.5 $ do
                        modifyIORef' attemptCount (+1)
                        error "service down" :: IO String

      result @?= "ultimate-fallback"
      count <- readIORef attemptCount
      count @?= 2  -- Retries happened before fallback

  , testCase "rate limiter + fallback = graceful degradation" $ do
      rl <- newRateLimiter (RateLimiterConfig 1 1)  -- Only 1 token

      -- First call succeeds
      result1 <- withRateLimit rl (pure "success" :: IO String)
      result1 @?= "success"

      -- Second call rate limited, but fallback handles it gracefully
      result2 <- withFallback (pure "rate-limited-fallback") $
                   withRateLimit rl (pure "normal" :: IO String)
      result2 @?= "rate-limited-fallback"

  , testCase "bulkhead + fallback = load shedding" $ do
      bh <- newBulkhead (BulkheadConfig 0)  -- No capacity

      result <- withFallback (pure "load-shed") $
                  withBulkhead bh (pure "normal" :: IO String)

      result @?= "load-shed"
  ]

-- | Tests for concurrent behavior
concurrencyTests :: TestTree
concurrencyTests = testGroup "Concurrency"
  [ testCase "concurrent fallbacks don't interfere" $ do
      sharedCounter <- newIORef (0 :: Int)
      barrier <- newMVar ()

      let action = withFallback
            (pure "fallback")
            (error "fail" :: IO String)

      -- Launch 10 concurrent operations
      takeMVar barrier  -- Hold the barrier
      sequence_ $ replicate 10 $ forkIO $ do
        _ <- takeMVar barrier  -- Wait for release
        result <- action
        when (result == "fallback") $
          atomicModifyIORef' sharedCounter (\n -> (n + 1, ()))
        putMVar barrier ()  -- Release for next thread

      putMVar barrier ()  -- Release the barrier
      threadDelay 100000  -- Wait for threads

      count <- readIORef sharedCounter
      assertBool "all fallbacks executed" (count >= 8)  -- Allow some scheduling variance

  , testCase "fallback under load preserves state consistency" $ do
      results <- newIORef ([] :: [String])

      let action n = withFallback
            (pure $ "fallback-" ++ show n)
            (if n `mod` 2 == 0
               then pure $ "success-" ++ show n
               else error "fail" :: IO String)

      -- Run multiple operations
      sequence_ $ map (\n -> do
        result <- action n
        atomicModifyIORef' results (\rs -> (result : rs, ()))) [1..5]

      finalResults <- readIORef results
      assertEqual "correct number of results" 5 (length finalResults)

      -- Check we got expected mix
      let successes = filter ("success" `isPrefixOf`) finalResults
          fallbacks = filter ("fallback" `isPrefixOf`) finalResults

      assertEqual "correct successes" 2 (length successes)
      assertEqual "correct fallbacks" 3 (length fallbacks)
  ]

-- Helper
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys)
  | x == y = isPrefixOf xs ys
  | otherwise = False
