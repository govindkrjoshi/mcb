-- |
-- Module      : Test.Unit.Retry
-- Description : Unit tests for Retry
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause

module Test.Unit.Retry
  ( tests
  ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (try)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import CircuitBreaker
  ( BackoffStrategy (..)
  , RetriesExhaustedException (..)
  , RetryConfig (..)
  , defaultRetryConfig
  , setBackoffStrategy
  , setMaxAttempts
  , setOnRetry
  , setShouldRetry
  , withRetry
  , (&)
  )

tests :: TestTree
tests = testGroup "CircuitBreaker.Retry"
  [ configTests
  , backoffStrategyTests
  , withRetryTests
  , callbackTests
  ]

configTests :: TestTree
configTests = testGroup "RetryConfig"
  [ testCase "defaultRetryConfig has sensible defaults" $ do
      let config = defaultRetryConfig
      retryMaxAttempts config @?= 3

  , testCase "setMaxAttempts updates config" $ do
      let config = defaultRetryConfig & setMaxAttempts 5
      retryMaxAttempts config @?= 5

  , testCase "setMaxAttempts enforces minimum of 1" $ do
      let config = defaultRetryConfig & setMaxAttempts 0
      retryMaxAttempts config @?= 1

  , testCase "config has Show instance" $ do
      let config = defaultRetryConfig
      length (show config) > 0 @?= True
  ]

backoffStrategyTests :: TestTree
backoffStrategyTests = testGroup "BackoffStrategy"
  [ testCase "NoBackoff has Show instance" $ do
      show NoBackoff @?= "NoBackoff"

  , testCase "ConstantBackoff has Show instance" $ do
      let strat = ConstantBackoff 1.0
      assertBool "has show" (length (show strat) > 0)

  , testCase "ExponentialBackoff has Show instance" $ do
      let strat = ExponentialBackoff 0.1 30 2.0
      assertBool "has show" (length (show strat) > 0)

  , testCase "setBackoffStrategy updates config" $ do
      let config = defaultRetryConfig & setBackoffStrategy NoBackoff
      retryBackoff config @?= NoBackoff
  ]

withRetryTests :: TestTree
withRetryTests = testGroup "withRetry"
  [ testCase "successful action returns result immediately" $ do
      let config = defaultRetryConfig & setMaxAttempts 3
      result <- withRetry config (pure "success" :: IO String)
      result @?= "success"

  , testCase "action is only called once on success" $ do
      callCount <- newIORef (0 :: Int)
      let config = defaultRetryConfig & setMaxAttempts 3 & setBackoffStrategy NoBackoff
      _ <- withRetry config $ do
        modifyIORef' callCount (+1)
        pure ()
      count <- readIORef callCount
      count @?= 1

  , testCase "retries on failure until success" $ do
      callCount <- newIORef (0 :: Int)
      let config = defaultRetryConfig
            & setMaxAttempts 5
            & setBackoffStrategy NoBackoff
      result <- withRetry config $ do
        count <- readIORef callCount
        writeIORef callCount (count + 1)
        if count < 2
          then error "transient error"
          else pure "success"
      result @?= "success"
      finalCount <- readIORef callCount
      finalCount @?= 3  -- Failed twice, succeeded on third

  , testCase "throws RetriesExhaustedException when all retries fail" $ do
      let config = defaultRetryConfig
            & setMaxAttempts 3
            & setBackoffStrategy NoBackoff
      result <- try $ withRetry config (error "always fails" :: IO String)
      case result of
        Left (RetriesExhaustedException attempts _) -> attempts @?= 3
        Right _ -> error "Expected RetriesExhaustedException"

  , testCase "respects shouldRetry predicate" $ do
      callCount <- newIORef (0 :: Int)
      let config = defaultRetryConfig
            & setMaxAttempts 5
            & setBackoffStrategy NoBackoff
            & setShouldRetry (const False)  -- Never retry
      result <- try $ withRetry config $ do
        modifyIORef' callCount (+1)
        error "never retry this"
      case result of
        Left (RetriesExhaustedException attempts _) -> attempts @?= 1
        Right (_ :: String) -> error "Expected RetriesExhaustedException"

  , testCase "ConstantBackoff delays between retries" $ do
      startTime <- getCurrentTimeMillis
      callCount <- newIORef (0 :: Int)
      let config = defaultRetryConfig
            & setMaxAttempts 3
            & setBackoffStrategy (ConstantBackoff 0.05)  -- 50ms delay
      result <- try $ withRetry config $ do
        count <- readIORef callCount
        writeIORef callCount (count + 1)
        error "fail"
      endTime <- getCurrentTimeMillis
      case result of
        Left (RetriesExhaustedException _ _) -> do
          -- Should have delayed twice (between 3 attempts)
          let elapsed = endTime - startTime
          assertBool ("should take at least 90ms, took " ++ show elapsed) (elapsed >= 90)
        Right (_ :: String) -> error "Expected RetriesExhaustedException"
  ]

callbackTests :: TestTree
callbackTests = testGroup "Callbacks"
  [ testCase "onRetry callback is invoked before each retry" $ do
      retryAttempts <- newMVar ([] :: [Int])
      let config = defaultRetryConfig
            & setMaxAttempts 4
            & setBackoffStrategy NoBackoff
            & setOnRetry (\attempt _ -> modifyMVar_ retryAttempts (pure . (attempt:)))
      _ <- try $ withRetry config (error "fail" :: IO String) :: IO (Either RetriesExhaustedException String)
      attempts <- readMVar retryAttempts
      -- Should have called onRetry for attempts 1, 2, 3 (before retries 2, 3, 4)
      assertEqual "should record 3 retry callbacks" [3, 2, 1] attempts

  , testCase "onRetry receives correct delay" $ do
      delays <- newMVar ([] :: [Double])
      let config = defaultRetryConfig
            & setMaxAttempts 2
            & setBackoffStrategy (ConstantBackoff 0.5)
            & setOnRetry (\_ delay -> modifyMVar_ delays (pure . (realToFrac delay:)))
      _ <- try $ withRetry config (error "fail" :: IO String) :: IO (Either RetriesExhaustedException String)
      recordedDelays <- readMVar delays
      case recordedDelays of
        [d] -> assertBool "delay should be ~0.5" (d >= 0.4 && d <= 0.6)
        _ -> error "Expected exactly one retry callback"
  ]

-- Helper to get current time in milliseconds
getCurrentTimeMillis :: IO Integer
getCurrentTimeMillis = do
  now <- getCurrentTime
  -- Convert to milliseconds since Unix epoch (approximation)
  pure $ round (realToFrac (diffUTCTime now epochStart) * 1000 :: Double)
  where
    epochStart = read "1970-01-01 00:00:00 UTC"
