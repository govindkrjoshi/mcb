-- |
-- Module      : Test.Integration.Resilience
-- Description : Integration tests for Resilience composition
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause

module Test.Integration.Resilience
  ( tests
  ) where

import Control.Exception (SomeException, try)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import CircuitBreaker
  ( BackoffStrategy (..)
  , BulkheadConfig (..)
  , BulkheadRejectedException (..)
  , CircuitOpenException (..)
  , RateLimitedException (..)
  , RateLimiterConfig (..)
  , ResilienceConfig (..)
  , RetriesExhaustedException (..)
  , addBulkhead
  , addCircuitBreaker
  , addRateLimiter
  , addRetry
  , addTimeout
  , defaultConfig
  , defaultResilienceConfig
  , defaultRetryConfig
  , newBulkhead
  , newCircuitBreaker
  , newRateLimiter
  , setBackoffStrategy
  , setFailureThreshold
  , setMaxAttempts
  , setSlidingWindowSize
  , withResilience
  , (&)
  )

tests :: TestTree
tests = testGroup "CircuitBreaker.Resilience"
  [ configTests
  , compositionTests
  , integrationTests
  ]

configTests :: TestTree
configTests = testGroup "ResilienceConfig"
  [ testCase "defaultResilienceConfig has all Nothing" $ do
      let config = defaultResilienceConfig
      assertBool "no retry" (isNothing $ resRetry config)
      assertBool "no circuit breaker" (isNothing $ resCircuitBreaker config)
      assertBool "no timeout" (isNothing $ resTimeout config)
      assertBool "no bulkhead" (isNothing $ resBulkhead config)
      assertBool "no rate limiter" (isNothing $ resRateLimiter config)

  , testCase "config has Show instance" $ do
      let config = defaultResilienceConfig
      assertBool "has show" (length (show config) > 0)

  , testCase "builders set components correctly" $ do
      cb <- newCircuitBreaker defaultConfig
      bh <- newBulkhead (BulkheadConfig 10)
      rl <- newRateLimiter (RateLimiterConfig 100 500)
      let config = defaultResilienceConfig
            & addRetry defaultRetryConfig
            & addCircuitBreaker cb
            & addTimeout 5
            & addBulkhead bh
            & addRateLimiter rl
      assertBool "has retry" (isJust $ resRetry config)
      assertBool "has circuit breaker" (isJust $ resCircuitBreaker config)
      assertBool "has timeout" (isJust $ resTimeout config)
      assertBool "has bulkhead" (isJust $ resBulkhead config)
      assertBool "has rate limiter" (isJust $ resRateLimiter config)
  ]

compositionTests :: TestTree
compositionTests = testGroup "withResilience"
  [ testCase "empty config just runs action" $ do
      result <- withResilience defaultResilienceConfig (pure "success" :: IO String)
      result @?= "success"

  , testCase "with circuit breaker only" $ do
      cb <- newCircuitBreaker defaultConfig
      let config = defaultResilienceConfig & addCircuitBreaker cb
      result <- withResilience config (pure "success" :: IO String)
      result @?= "success"

  , testCase "with rate limiter only" $ do
      rl <- newRateLimiter (RateLimiterConfig 100 500)
      let config = defaultResilienceConfig & addRateLimiter rl
      result <- withResilience config (pure "success" :: IO String)
      result @?= "success"

  , testCase "with bulkhead only" $ do
      bh <- newBulkhead (BulkheadConfig 10)
      let config = defaultResilienceConfig & addBulkhead bh
      result <- withResilience config (pure "success" :: IO String)
      result @?= "success"

  , testCase "with retry only" $ do
      callCount <- newIORef (0 :: Int)
      let retryConfig = defaultRetryConfig
            & setMaxAttempts 3
            & setBackoffStrategy NoBackoff
          config = defaultResilienceConfig & addRetry retryConfig
      result <- withResilience config $ do
        count <- readIORef callCount
        modifyIORef' callCount (+1)
        if count < 2
          then error "transient"
          else pure "success"
      result @?= "success"

  , testCase "with all components" $ do
      cb <- newCircuitBreaker defaultConfig
      bh <- newBulkhead (BulkheadConfig 10)
      rl <- newRateLimiter (RateLimiterConfig 100 500)
      let config = defaultResilienceConfig
            & addRetry (defaultRetryConfig & setMaxAttempts 2 & setBackoffStrategy NoBackoff)
            & addCircuitBreaker cb
            & addTimeout 5
            & addBulkhead bh
            & addRateLimiter rl
      result <- withResilience config (pure "success" :: IO String)
      result @?= "success"
  ]

integrationTests :: TestTree
integrationTests = testGroup "Integration"
  [ testCase "circuit breaker opens after failures" $ do
      cb <- newCircuitBreaker (defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10)  -- Larger window
      let config = defaultResilienceConfig & addCircuitBreaker cb

      -- Cause failures to open the circuit (need >= minimumCallsForOpen which is 5)
      sequence_ $ replicate 6 $ do
        _ <- try $ withResilience config (error "fail" :: IO String) :: IO (Either SomeException String)
        pure ()

      -- Next call should be rejected
      result <- try $ withResilience config (pure "should not run" :: IO String)
      case result of
        Left (_ :: CircuitOpenException) -> pure ()
        Right _ -> error "Expected CircuitOpenException"

  , testCase "retry wraps circuit breaker failures" $ do
      callCount <- newIORef (0 :: Int)
      cb <- newCircuitBreaker (defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 4)
      let config = defaultResilienceConfig
            & addRetry (defaultRetryConfig & setMaxAttempts 2 & setBackoffStrategy NoBackoff)
            & addCircuitBreaker cb

      -- Action succeeds
      result <- withResilience config $ do
        modifyIORef' callCount (+1)
        pure "success"
      result @?= "success"
      count <- readIORef callCount
      count @?= 1  -- Only called once since it succeeded

  , testCase "rate limiter rejects when exhausted" $ do
      rl <- newRateLimiter (RateLimiterConfig 1 1)  -- 1 token only
      let config = defaultResilienceConfig & addRateLimiter rl

      -- First call uses the token
      result1 <- withResilience config (pure "first" :: IO String)
      result1 @?= "first"

      -- Second call should be rate limited
      result2 <- try $ withResilience config (pure "second" :: IO String)
      case result2 of
        Left RateLimitedException -> pure ()
        Right _ -> error "Expected RateLimitedException"

  , testCase "bulkhead rejects when full" $ do
      bh <- newBulkhead (BulkheadConfig 0)  -- No capacity!
      let config = defaultResilienceConfig & addBulkhead bh

      result <- try $ withResilience config (pure "should not run" :: IO String)
      case result of
        Left (_ :: BulkheadRejectedException) -> pure ()
        Right _ -> error "Expected BulkheadRejectedException"
  ]
