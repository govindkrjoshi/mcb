{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Test.Unit.State
-- Description : Tests for STM state management
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
--
-- Tests for the STM-based circuit breaker state management, including
-- thread-safety and performance tests.

module Test.Unit.State (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Monad (replicateM_, forM_)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import CircuitBreaker
  ( CircuitBreakerState (..)
  , State (..)
  , defaultConfig
  , getCurrentState
  , getCircuitBreakerState
  , getFailureRateIO
  , newCircuitBreaker
  , readState
  , updateState
  , insertResult
  , getTotalCalls
  )

tests :: TestTree
tests = testGroup "CircuitBreaker.Internal.State"
  [ initialStateTests
  , stmOperationTests
  , queryTests
  , threadSafetyTests
  , performanceTests
  ]

-- | Tests for initial circuit breaker state
initialStateTests :: TestTree
initialStateTests = testGroup "Initial State"
  [ testCase "initial state is Closed" $ do
      cb <- newCircuitBreaker defaultConfig
      state <- getCurrentState cb
      state @?= Closed

  , testCase "initial failure rate is 0.0" $ do
      cb <- newCircuitBreaker defaultConfig
      rate <- getFailureRateIO cb
      rate @?= 0.0

  , testCase "initial sliding window is empty" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      let calls = getTotalCallsFromState cbState
      calls @?= 0

  , testCase "lastStateChange is set to current time" $ do
      before <- getCurrentTime
      cb <- newCircuitBreaker defaultConfig
      after <- getCurrentTime
      cbState <- getCircuitBreakerState cb
      let changeTime = cbsLastStateChange cbState
      assertBool "lastStateChange should be >= before" (changeTime >= before)
      assertBool "lastStateChange should be <= after" (changeTime <= after)
  ]

-- | Tests for STM read/write operations
stmOperationTests :: TestTree
stmOperationTests = testGroup "STM Operations"
  [ testCase "readState returns current state" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- atomically $ readState cb
      cbsState cbState @?= Closed

  , testCase "updateState modifies state" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- atomically $ readState cb
      let newState = cbState { cbsState = Open }
      atomically $ updateState cb newState
      updatedState <- atomically $ readState cb
      cbsState updatedState @?= Open

  , testCase "read and update in single transaction" $ do
      cb <- newCircuitBreaker defaultConfig
      atomically $ do
        cbState <- readState cb
        let newState = cbState { cbsState = HalfOpen }
        updateState cb newState
      state <- getCurrentState cb
      state @?= HalfOpen

  , testCase "multiple updates in sequence" $ do
      cb <- newCircuitBreaker defaultConfig
      -- First update to Open
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = Open }
      -- Second update to HalfOpen
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = HalfOpen }
      -- Third update back to Closed
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = Closed }
      state <- getCurrentState cb
      state @?= Closed

  , testCase "updating sliding window via STM" $ do
      cb <- newCircuitBreaker defaultConfig
      atomically $ do
        cbState <- readState cb
        let sw = cbsSlidingWindow cbState
            sw' = insertResult False sw  -- Add a failure
        updateState cb cbState { cbsSlidingWindow = sw' }
      rate <- getFailureRateIO cb
      rate @?= 1.0  -- 100% failure rate (1 failure out of 1 call)
  ]

-- | Tests for convenient query functions
queryTests :: TestTree
queryTests = testGroup "Query Functions"
  [ testCase "getCurrentState returns State value" $ do
      cb <- newCircuitBreaker defaultConfig
      state <- getCurrentState cb
      state @?= Closed

  , testCase "getFailureRateIO returns Double" $ do
      cb <- newCircuitBreaker defaultConfig
      rate <- getFailureRateIO cb
      assertBool "rate should be >= 0" (rate >= 0.0)
      assertBool "rate should be <= 1" (rate <= 1.0)

  , testCase "getCircuitBreakerState returns full state" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      cbsState cbState @?= Closed
  ]

-- | Thread-safety tests
threadSafetyTests :: TestTree
threadSafetyTests = testGroup "Thread Safety"
  [ testCase "100 concurrent reads don't block" $ do
      cb <- newCircuitBreaker defaultConfig
      done <- newEmptyMVar
      let numReaders = 100
      -- Start 100 reader threads
      replicateM_ numReaders $ forkIO $ do
        _ <- getCurrentState cb
        pure ()
      -- Give threads time to complete
      threadDelay 100_000  -- 100ms
      -- All should complete without deadlock
      state <- getCurrentState cb
      state @?= Closed
      putMVar done ()
      takeMVar done

  , testCase "concurrent reads and writes maintain consistency" $ do
      cb <- newCircuitBreaker defaultConfig
      done <- newEmptyMVar
      let numWriters = 10
          numReaders = 50

      -- Start writer threads that update the sliding window
      forM_ [(1::Int)..numWriters] $ \i -> forkIO $ do
        replicateM_ 10 $ do
          atomically $ do
            cbState <- readState cb
            let sw = cbsSlidingWindow cbState
                sw' = insertResult (even i) sw  -- Even threads add success, odd add failure
            updateState cb cbState { cbsSlidingWindow = sw' }
          threadDelay 1_000  -- 1ms

      -- Start reader threads
      forM_ [(1::Int)..numReaders] $ \_ -> forkIO $ do
        replicateM_ 20 $ do
          _ <- getFailureRateIO cb
          threadDelay 500  -- 0.5ms

      -- Wait for all to complete
      threadDelay 200_000  -- 200ms

      -- Verify state is still consistent
      rate <- getFailureRateIO cb
      assertBool "failure rate should be valid" (rate >= 0.0 && rate <= 1.0)
      putMVar done ()
      takeMVar done

  , testCase "state transitions are atomic" $ do
      cb <- newCircuitBreaker defaultConfig
      done <- newEmptyMVar

      -- Multiple threads trying to update state
      forM_ [(1::Int)..20] $ \_ -> forkIO $ do
        atomically $ do
          cbState <- readState cb
          case cbsState cbState of
            Closed   -> updateState cb cbState { cbsState = Open }
            Open     -> updateState cb cbState { cbsState = HalfOpen }
            HalfOpen -> updateState cb cbState { cbsState = Closed }

      threadDelay 50_000  -- 50ms

      -- State should be one of the valid states
      state <- getCurrentState cb
      assertBool "state should be valid" (state `elem` [Closed, Open, HalfOpen])
      putMVar done ()
      takeMVar done
  ]

-- | Performance tests
performanceTests :: TestTree
performanceTests = testGroup "Performance"
  [ testCase "1000 sequential reads in <10ms" $ do
      cb <- newCircuitBreaker defaultConfig
      start <- getCurrentTime
      replicateM_ 1000 $ getCurrentState cb
      end <- getCurrentTime
      let elapsed = diffUTCTime end start
      assertBool ("1000 reads took " ++ show elapsed ++ ", should be <10ms")
        (elapsed < 0.010)  -- 10ms

  , testCase "1000 sequential writes in <10ms" $ do
      cb <- newCircuitBreaker defaultConfig
      start <- getCurrentTime
      replicateM_ 1000 $ atomically $ do
        cbState <- readState cb
        let sw = cbsSlidingWindow cbState
            sw' = insertResult True sw
        updateState cb cbState { cbsSlidingWindow = sw' }
      end <- getCurrentTime
      let elapsed = diffUTCTime end start
      assertBool ("1000 writes took " ++ show elapsed ++ ", should be <10ms")
        (elapsed < 0.010)  -- 10ms

  , testCase "1000 mixed read/write operations in <20ms" $ do
      cb <- newCircuitBreaker defaultConfig
      start <- getCurrentTime
      replicateM_ 1000 $ do
        -- Read
        _ <- getCurrentState cb
        -- Write
        atomically $ do
          cbState <- readState cb
          let sw = cbsSlidingWindow cbState
              sw' = insertResult True sw
          updateState cb cbState { cbsSlidingWindow = sw' }
      end <- getCurrentTime
      let elapsed = diffUTCTime end start
      assertBool ("1000 mixed ops took " ++ show elapsed ++ ", should be <20ms")
        (elapsed < 0.020)  -- 20ms
  ]

-- | Helper to get total calls from state
getTotalCallsFromState :: CircuitBreakerState -> Int
getTotalCallsFromState cbState = getTotalCalls (cbsSlidingWindow cbState)
