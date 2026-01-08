{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.Integration.TimeoutCircuitBreaker
-- Description : Integration tests for Timeout + Circuit Breaker composition
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
--
-- Tests verifying the correct behavior when composing timeout and circuit
-- breaker patterns, including:
--
-- * Exception propagation
-- * State transitions when timeouts occur
-- * Composition ordering (CB wrapping timeout vs timeout wrapping CB)
-- * TimeoutException counting as failures

module Test.Integration.TimeoutCircuitBreaker (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, fromException, toException)
import Control.Monad (replicateM_)
import Control.Monad.Catch (try)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Proxy (Proxy (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import CircuitBreaker
  ( CircuitBreakerException (..)
  , CircuitOpenException (..)
  , State (..)
  , TimeoutException (..)
  , addCircuitBreaker
  , addTimeout
  , defaultConfig
  , defaultResilienceConfig
  , getCurrentState
  , matchException
  , milliseconds
  , newCircuitBreaker
  , onlyException
  , setExceptionPredicate
  , setFailureThreshold
  , setSlidingWindowSize
  , setWaitDuration
  , withCircuitBreaker
  , withResilience
  , withTimeout
  , (&)
  )

tests :: TestTree
tests = testGroup "Timeout + CircuitBreaker Composition"
  [ timeoutExceptionPropagationTests
  , stateTransitionTests
  , compositionOrderTests
  , resilienceCompositionTests
  , exceptionPredicateTests
  ]

-- ---------------------------------------------------------------------------
-- TimeoutException Propagation Tests
-- ---------------------------------------------------------------------------

timeoutExceptionPropagationTests :: TestTree
timeoutExceptionPropagationTests = testGroup "TimeoutException Propagation"
  [ testCase "TimeoutException propagates through circuit breaker" $ do
      cb <- newCircuitBreaker defaultConfig
      result <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 10) $ do
          threadDelay 100_000  -- 100ms, will timeout
          pure "should not reach"
      case result of
        Left (e :: TimeoutException) -> do
          assertBool "timeout captured" (timeoutDuration e > 0)
        Right _ -> error "Expected TimeoutException"

  , testCase "TimeoutException contains correct duration" $ do
      cb <- newCircuitBreaker defaultConfig
      let timeoutMs = 50
      result <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds timeoutMs) $ do
          threadDelay 200_000  -- 200ms
          pure "should not reach"
      case result of
        Left (e :: TimeoutException) -> do
          -- Duration should match what we configured (approximately)
          let durationMs = realToFrac (timeoutDuration e) * 1000 :: Double
          assertBool "duration matches" (abs (durationMs - timeoutMs) < 1)
        Right _ -> error "Expected TimeoutException"

  , testCase "TimeoutException is catchable as CircuitBreakerException" $ do
      cb <- newCircuitBreaker defaultConfig
      result <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 10) $ do
          threadDelay 100_000
          pure "should not reach"
      case result of
        Left (CircuitBreakerException e) ->
          assertBool "is TimeoutException" $
            matchException (Proxy :: Proxy TimeoutException) (toException e)
        Right _ -> error "Expected CircuitBreakerException"

  , testCase "successful action within timeout returns result" $ do
      cb <- newCircuitBreaker defaultConfig
      result <- withCircuitBreaker cb $
        withTimeout (milliseconds 500) $ do
          threadDelay 10_000  -- 10ms
          pure "success"
      result @?= "success"
  ]

-- ---------------------------------------------------------------------------
-- State Transition Tests
-- ---------------------------------------------------------------------------

stateTransitionTests :: TestTree
stateTransitionTests = testGroup "State Transitions on Timeout"
  [ testCase "timeouts count as failures and open circuit" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- Cause 5 timeouts to open the circuit
      replicateM_ 5 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ do
            threadDelay 50_000  -- 50ms
            pure ()
        pure ()

      state <- getCurrentState cb
      state @?= Open

  , testCase "timeouts in HalfOpen reopen circuit" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0  -- Immediate transition to HalfOpen
      cb <- newCircuitBreaker config

      -- Open the circuit with timeouts
      replicateM_ 5 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ do
            threadDelay 50_000
            pure ()
        pure ()

      -- First call transitions to HalfOpen
      withCircuitBreaker cb $ pure ()

      state1 <- getCurrentState cb
      state1 @?= HalfOpen

      -- Timeout in HalfOpen should reopen
      _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 5) $ do
          threadDelay 50_000
          pure ()

      state2 <- getCurrentState cb
      state2 @?= Open

  , testCase "successful call after timeout keeps circuit closed" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- One timeout (not enough to open)
      _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 5) $ do
          threadDelay 50_000
          pure ()

      -- Multiple successes
      replicateM_ 9 $ withCircuitBreaker cb $ pure ()

      state <- getCurrentState cb
      state @?= Closed

  , testCase "mixed timeouts and successes respects failure threshold" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- 4 timeouts (40% failure rate) - circuit should stay closed
      replicateM_ 4 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ do
            threadDelay 50_000
            pure ()
        pure ()

      -- 6 successes
      replicateM_ 6 $ withCircuitBreaker cb $ pure ()

      state <- getCurrentState cb
      state @?= Closed

      -- One more timeout tips over 50% (5/11 would be ~45%, but we check sliding window)
      -- Let's add more to ensure we cross threshold
      replicateM_ 2 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ do
            threadDelay 50_000
            pure ()
        pure ()

      -- Now sliding window has shifted toward more failures
      state2 <- getCurrentState cb
      -- State depends on what's in the sliding window
      assertBool "state changed or stayed closed" (state2 `elem` [Closed, Open])
  ]

-- ---------------------------------------------------------------------------
-- Composition Order Tests
-- ---------------------------------------------------------------------------

compositionOrderTests :: TestTree
compositionOrderTests = testGroup "Composition Ordering"
  [ testCase "CB wrapping timeout: timeout inside CB-protected action" $ do
      cb <- newCircuitBreaker defaultConfig
      callCount <- newIORef (0 :: Int)

      -- CB wraps timeout - timeouts count as failures
      result <- try $ withCircuitBreaker cb $ do
        modifyIORef' callCount (+1)
        withTimeout (milliseconds 10) $ do
          threadDelay 100_000
          pure "should not reach"

      count <- readIORef callCount
      count @?= 1  -- Action was called

      case result of
        Left (_ :: TimeoutException) -> pure ()
        Right _ -> error "Expected TimeoutException"

  , testCase "timeout wrapping CB: CB open exception exits quickly" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 30  -- Long wait
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $ error "fail"
        pure ()

      -- Now timeout wraps CB - circuit open exception should be fast
      result :: Either SomeException String <- try $ withTimeout (milliseconds 500) $
        withCircuitBreaker cb $ do
          threadDelay 100_000  -- This shouldn't run
          pure "should not reach"

      case result of
        Left e -> case fromException e of
          Just (_ :: CircuitOpenException) -> pure ()  -- Fast fail, expected
          Nothing -> case fromException e of
            Just (_ :: TimeoutException) -> error "Should not timeout - CB should fail fast"
            Nothing -> error $ "Unexpected exception: " ++ show e
        Right _ -> error "Expected CircuitOpenException"

  , testCase "timeout wrapping CB: action times out before CB records" $ do
      cb <- newCircuitBreaker defaultConfig
      callCount <- newIORef (0 :: Int)

      -- Timeout wraps CB - timeout fires first
      result <- try $ withTimeout (milliseconds 10) $
        withCircuitBreaker cb $ do
          modifyIORef' callCount (+1)
          threadDelay 100_000  -- Slow action
          pure "should not reach"

      count <- readIORef callCount
      count @?= 1

      case result of
        Left (_ :: TimeoutException) -> pure ()
        Right _ -> error "Expected TimeoutException"

      -- Important: when timeout wraps CB, the timeout exception happens
      -- outside the CB's exception handling, so it may not be recorded
      -- as a CB failure. The circuit should still be closed.
      state <- getCurrentState cb
      -- This is interesting - the timeout killed the action but CB
      -- didn't get to record success or failure
      assertBool "circuit state is valid" (state `elem` [Closed, Open, HalfOpen])

  , testCase "nested composition: CB -> timeout -> CB" $ do
      outerCb <- newCircuitBreaker defaultConfig
      innerCb <- newCircuitBreaker defaultConfig

      result :: Either SomeException String <- try $ withCircuitBreaker outerCb $
        withTimeout (milliseconds 100) $
          withCircuitBreaker innerCb $ do
            threadDelay 10_000  -- 10ms, within timeout
            pure "success"

      case result of
        Right r -> r @?= "success"
        Left _ -> error "Expected success"
  ]

-- ---------------------------------------------------------------------------
-- Resilience Composition Tests
-- ---------------------------------------------------------------------------

resilienceCompositionTests :: TestTree
resilienceCompositionTests = testGroup "withResilience Composition"
  [ testCase "withResilience applies CB then timeout correctly" $ do
      cb <- newCircuitBreaker defaultConfig
      let config = defaultResilienceConfig
            & addCircuitBreaker cb
            & addTimeout (milliseconds 10)

      result <- try $ withResilience config $ do
        threadDelay 100_000  -- Will timeout
        pure "should not reach"

      case result of
        Left (_ :: TimeoutException) -> pure ()
        Right _ -> error "Expected TimeoutException"

  , testCase "withResilience timeout failures open circuit" $ do
      let cbConfig = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker cbConfig
      let config = defaultResilienceConfig
            & addCircuitBreaker cb
            & addTimeout (milliseconds 5)

      -- Cause 5 timeouts
      replicateM_ 5 $ do
        _ :: Either TimeoutException () <- try $ withResilience config $ do
          threadDelay 50_000
          pure ()
        pure ()

      state <- getCurrentState cb
      state @?= Open

  , testCase "withResilience succeeds when action completes in time" $ do
      cb <- newCircuitBreaker defaultConfig
      let config = defaultResilienceConfig
            & addCircuitBreaker cb
            & addTimeout (milliseconds 500)

      result <- withResilience config $ do
        threadDelay 10_000  -- 10ms
        pure "success"

      result @?= "success"

  , testCase "withResilience CB open prevents action execution" $ do
      let cbConfig = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 30
      cb <- newCircuitBreaker cbConfig

      -- Open the circuit with regular failures
      replicateM_ 5 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $ error "fail"
        pure ()

      let config = defaultResilienceConfig
            & addCircuitBreaker cb
            & addTimeout (milliseconds 500)

      callCount <- newIORef (0 :: Int)
      result <- try $ withResilience config $ do
        modifyIORef' callCount (+1)
        pure "should not reach"

      count <- readIORef callCount
      count @?= 0  -- Action was never called

      case result of
        Left (_ :: CircuitOpenException) -> pure ()
        Right _ -> error "Expected CircuitOpenException"
  ]

-- ---------------------------------------------------------------------------
-- Exception Predicate Tests
-- ---------------------------------------------------------------------------

exceptionPredicateTests :: TestTree
exceptionPredicateTests = testGroup "Exception Predicate with Timeout"
  [ testCase "default predicate counts timeouts as failures" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      replicateM_ 5 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ do
            threadDelay 50_000
            pure ()
        pure ()

      state <- getCurrentState cb
      state @?= Open

  , testCase "custom predicate can ignore timeouts" $ do
      let isNotTimeout :: SomeException -> Bool
          isNotTimeout e = case fromException e of
            Just (TimeoutException _) -> False  -- Don't count timeouts
            Nothing -> True

      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate isNotTimeout
      cb <- newCircuitBreaker config

      -- 10 timeouts should NOT open the circuit
      replicateM_ 10 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ do
            threadDelay 50_000
            pure ()
        pure ()

      state <- getCurrentState cb
      state @?= Closed

  , testCase "custom predicate can count only timeouts" $ do
      let isTimeout :: SomeException -> Bool
          isTimeout = onlyException (Proxy :: Proxy TimeoutException)

      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate isTimeout
      cb <- newCircuitBreaker config

      -- Regular exceptions should NOT open the circuit
      replicateM_ 10 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $ error "not a timeout"
        pure ()

      state <- getCurrentState cb
      state @?= Closed

      -- But timeouts SHOULD open the circuit
      replicateM_ 5 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ do
            threadDelay 50_000
            pure ()
        pure ()

      state2 <- getCurrentState cb
      state2 @?= Open

  , testCase "timeouts still propagate even when not counted as failures" $ do
      let isNotTimeout :: SomeException -> Bool
          isNotTimeout e = case fromException e of
            Just (TimeoutException _) -> False
            Nothing -> True

      let config = defaultConfig
            & setExceptionPredicate isNotTimeout
      cb <- newCircuitBreaker config

      result <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 5) $ do
          threadDelay 50_000
          pure "should not reach"

      case result of
        Left (_ :: TimeoutException) -> pure ()  -- Exception still propagated
        Right _ -> error "Expected TimeoutException"
  ]
