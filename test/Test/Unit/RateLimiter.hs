-- |
-- Module      : Test.Unit.RateLimiter
-- Description : Unit tests for RateLimiter
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause

module Test.Unit.RateLimiter
  ( tests
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall, SomeException, try)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import CircuitBreaker
  ( RateLimitedException (..)
  , RateLimiterConfig (..)
  , getAvailableTokens
  , newRateLimiter
  , tryConsume
  , withRateLimit
  )

tests :: TestTree
tests = testGroup "CircuitBreaker.RateLimiter"
  [ configTests
  , newRateLimiterTests
  , tryConsumeTests
  , withRateLimitTests
  ]

configTests :: TestTree
configTests = testGroup "RateLimiterConfig"
  [ testCase "can create config with rate and capacity" $ do
      let config = RateLimiterConfig 100 500
      rlcRate config @?= 100
      rlcCapacity config @?= 500

  , testCase "config has Show instance" $ do
      let config = RateLimiterConfig 100 500
      length (show config) > 0 @?= True

  , testCase "config has Eq instance" $ do
      let config1 = RateLimiterConfig 100 500
          config2 = RateLimiterConfig 100 500
          config3 = RateLimiterConfig 200 500
      assertEqual "same configs equal" config1 config2
      assertBool "different configs not equal" (config1 /= config3)
  ]

newRateLimiterTests :: TestTree
newRateLimiterTests = testGroup "newRateLimiter"
  [ testCase "creates rate limiter with full bucket" $ do
      rl <- newRateLimiter (RateLimiterConfig 100 500)
      tokens <- getAvailableTokens rl
      assertBool "bucket starts at capacity" (tokens >= 499 && tokens <= 500)

  , testCase "available tokens does not exceed capacity" $ do
      rl <- newRateLimiter (RateLimiterConfig 100 10)
      threadDelay 100000  -- 100ms = 10 tokens should be added, but capped at 10
      tokens <- getAvailableTokens rl
      assertBool "tokens capped at capacity" (tokens <= 10)
  ]

tryConsumeTests :: TestTree
tryConsumeTests = testGroup "tryConsume"
  [ testCase "consume succeeds when tokens available" $ do
      rl <- newRateLimiter (RateLimiterConfig 100 500)
      success <- tryConsume rl 1.0
      assertBool "should consume successfully" success

  , testCase "consume fails when insufficient tokens" $ do
      rl <- newRateLimiter (RateLimiterConfig 1 1)  -- 1 token capacity
      _ <- tryConsume rl 1.0  -- Consume the only token
      success <- tryConsume rl 1.0  -- Try to consume another
      assertBool "should fail to consume" (not success)

  , testCase "consume multiple tokens at once" $ do
      rl <- newRateLimiter (RateLimiterConfig 100 10)
      success <- tryConsume rl 5.0
      assertBool "should consume 5 tokens" success
      tokens <- getAvailableTokens rl
      assertBool "should have ~5 tokens left" (tokens >= 4 && tokens <= 6)

  , testCase "tokens refill over time" $ do
      rl <- newRateLimiter (RateLimiterConfig 100 10)  -- 100 per second = 1 per 10ms
      _ <- tryConsume rl 10.0  -- Empty the bucket
      tokens1 <- getAvailableTokens rl
      assertBool "bucket should be empty" (tokens1 < 1)
      threadDelay 50000  -- Wait 50ms (should add ~5 tokens)
      tokens2 <- getAvailableTokens rl
      assertBool "tokens should refill" (tokens2 >= 3 && tokens2 <= 7)
  ]

withRateLimitTests :: TestTree
withRateLimitTests = testGroup "withRateLimit"
  [ testCase "action executes when tokens available" $ do
      rl <- newRateLimiter (RateLimiterConfig 100 500)
      result <- withRateLimit rl (pure "success" :: IO String)
      result @?= "success"

  , testCase "throws RateLimitedException when no tokens" $ do
      rl <- newRateLimiter (RateLimiterConfig 1 1)
      _ <- tryConsume rl 1.0  -- Empty the bucket
      result <- try $ withRateLimit rl (pure "success" :: IO String)
      case result of
        Left RateLimitedException -> pure ()
        Right _ -> error "Expected RateLimitedException"

  , testCase "action exception propagates through" $ do
      rl <- newRateLimiter (RateLimiterConfig 100 500)
      result <- try $ withRateLimit rl (error "test error" :: IO String)
      case result of
        Left e -> assertBool "should propagate error" ("test" `elem` words (show (e :: SomeException)))
        Right _ -> error "Expected exception"

  , testCase "multiple rapid calls respect rate limit" $ do
      rl <- newRateLimiter (RateLimiterConfig 5 5)  -- 5 per second, burst of 5
      -- First 5 should succeed (using burst capacity)
      results <- sequence $ replicate 5 $ withRateLimit rl (pure True :: IO Bool)
      assertEqual "first 5 should succeed" 5 (length $ filter id results)
      -- 6th should fail immediately
      result <- try $ withRateLimit rl (pure True :: IO Bool)
      case result of
        Left RateLimitedException -> pure ()
        Right _ -> error "6th call should be rate limited"
  ]
