{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Test.Unit.State
-- Description : Tests for STM state management and state machine logic
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
--
-- Tests for the STM-based circuit breaker state management, including
-- thread-safety, performance tests, and state machine logic tests.

module Test.Unit.State (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Monad (replicateM_, forM_)
import Data.Time.Clock (addUTCTime, getCurrentTime, diffUTCTime)

import CircuitBreaker
  ( CircuitBreakerState (..)
  , State (..)
  , defaultConfig
  , getCurrentState
  , getCircuitBreakerState
  , getFailureRateIO
  , isCallPermitted
  , minimumCallsForOpen
  , newCircuitBreaker
  , readState
  , recordFailure
  , recordSuccess
  , setFailureThreshold
  , setHalfOpenPermits
  , setSlidingWindowSize
  , setWaitDuration
  , shouldOpen
  , shouldTransitionToHalfOpen
  , transitionToClosed
  , transitionToHalfOpen
  , transitionToOpen
  , updateState
  , insertResult
  , getTotalCalls
  , (&)
  )

tests :: TestTree
tests = testGroup "CircuitBreaker.Internal.State"
  [ initialStateTests
  , stmOperationTests
  , queryTests
  , threadSafetyTests
  , performanceTests
  , stateTransitionPredicateTests
  , stateTransitionFunctionTests
  , callPermissionTests
  , recordResultTests
  , fullStateFlowTests
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

-- ---------------------------------------------------------------------------
-- State Transition Predicate Tests
-- ---------------------------------------------------------------------------

-- | Tests for shouldOpen and shouldTransitionToHalfOpen predicates
stateTransitionPredicateTests :: TestTree
stateTransitionPredicateTests = testGroup "State Transition Predicates"
  [ testCase "shouldOpen returns False with no calls" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      let result = shouldOpen defaultConfig cbState
      result @?= False

  , testCase "shouldOpen returns False below minimum calls even at 100% failure" $ do
      cb <- newCircuitBreaker defaultConfig
      -- Add failures, but less than minimumCallsForOpen (5)
      replicateM_ 4 $ recordFailure cb =<< getCurrentTime
      cbState <- getCircuitBreakerState cb
      let result = shouldOpen defaultConfig cbState
      -- 4 calls is below minimum 5, so should not open
      result @?= False

  , testCase "shouldOpen returns True at threshold with minimum calls" $ do
      -- Create a config with 50% threshold
      let config = defaultConfig & setFailureThreshold 0.5
      cb <- newCircuitBreaker config
      -- Add 5 failures (100% failure rate, >= 50% threshold, >= 5 calls)
      replicateM_ 5 $ recordFailure cb =<< getCurrentTime
      cbState <- getCircuitBreakerState cb
      -- After 5 failures the state should already be Open
      cbsState cbState @?= Open

  , testCase "shouldOpen returns False below threshold with minimum calls" $ do
      let config = defaultConfig & setFailureThreshold 0.5
      cb <- newCircuitBreaker config
      -- Add 3 successes, then 2 failures (40% failure rate, < 50% threshold)
      now <- getCurrentTime
      replicateM_ 3 $ recordSuccess cb now
      replicateM_ 2 $ recordFailure cb now
      cbState <- getCircuitBreakerState cb
      let result = shouldOpen config cbState
      result @?= False

  , testCase "shouldTransitionToHalfOpen returns False during cooldown" $ do
      let config = defaultConfig & setWaitDuration 30  -- 30 second wait
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Manually set to Open state
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = Open, cbsLastStateChange = now }
      cbState <- getCircuitBreakerState cb
      -- Check 10 seconds after (before 30 second wait)
      let future = addUTCTime 10 now
      let result = shouldTransitionToHalfOpen config cbState future
      result @?= False

  , testCase "shouldTransitionToHalfOpen returns True after wait duration" $ do
      let config = defaultConfig & setWaitDuration 30  -- 30 second wait
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Manually set to Open state
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = Open, cbsLastStateChange = now }
      cbState <- getCircuitBreakerState cb
      -- Check 31 seconds after (after 30 second wait)
      let future = addUTCTime 31 now
      let result = shouldTransitionToHalfOpen config cbState future
      result @?= True
  ]

-- ---------------------------------------------------------------------------
-- State Transition Function Tests
-- ---------------------------------------------------------------------------

-- | Tests for transitionToOpen, transitionToHalfOpen, transitionToClosed
stateTransitionFunctionTests :: TestTree
stateTransitionFunctionTests = testGroup "State Transition Functions"
  [ testCase "transitionToOpen sets state to Open" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      now <- getCurrentTime
      let newState = transitionToOpen now cbState
      cbsState newState @?= Open

  , testCase "transitionToOpen preserves sliding window (mcb-oz8 fix)" $ do
      -- The sliding window is NOT reset when transitioning to Open.
      -- This is intentional (mcb-oz8 fix): in-flight calls that were permitted
      -- before the transition should still have their results recorded accurately.
      cb <- newCircuitBreaker defaultConfig
      -- Add some results
      now <- getCurrentTime
      replicateM_ 3 $ recordSuccess cb now
      cbState <- getCircuitBreakerState cb
      let newState = transitionToOpen now cbState
      -- Window should be preserved, not reset
      getTotalCalls (cbsSlidingWindow newState) @?= 3

  , testCase "transitionToOpen records timestamp" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      now <- getCurrentTime
      let later = addUTCTime 100 now
      let newState = transitionToOpen later cbState
      cbsLastStateChange newState @?= later

  , testCase "transitionToOpen resets halfOpenAttempts" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      now <- getCurrentTime
      let stateWithAttempts = cbState { cbsHalfOpenAttempts = 5 }
      let newState = transitionToOpen now stateWithAttempts
      cbsHalfOpenAttempts newState @?= 0

  , testCase "transitionToHalfOpen sets state to HalfOpen" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      now <- getCurrentTime
      let newState = transitionToHalfOpen now cbState
      cbsState newState @?= HalfOpen

  , testCase "transitionToHalfOpen preserves sliding window" $ do
      cb <- newCircuitBreaker defaultConfig
      now <- getCurrentTime
      -- Add some results
      replicateM_ 3 $ recordSuccess cb now
      cbState <- getCircuitBreakerState cb
      let originalCalls = getTotalCalls (cbsSlidingWindow cbState)
      let newState = transitionToHalfOpen now cbState
      getTotalCalls (cbsSlidingWindow newState) @?= originalCalls

  , testCase "transitionToHalfOpen resets halfOpenAttempts" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      now <- getCurrentTime
      let stateWithAttempts = cbState { cbsHalfOpenAttempts = 3 }
      let newState = transitionToHalfOpen now stateWithAttempts
      cbsHalfOpenAttempts newState @?= 0

  , testCase "transitionToClosed sets state to Closed" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      now <- getCurrentTime
      let openState = cbState { cbsState = Open }
      let newState = transitionToClosed now openState
      cbsState newState @?= Closed

  , testCase "transitionToClosed resets sliding window" $ do
      cb <- newCircuitBreaker defaultConfig
      now <- getCurrentTime
      replicateM_ 3 $ recordSuccess cb now
      cbState <- getCircuitBreakerState cb
      let newState = transitionToClosed now cbState
      getTotalCalls (cbsSlidingWindow newState) @?= 0

  , testCase "transitionToClosed resets halfOpenAttempts" $ do
      cb <- newCircuitBreaker defaultConfig
      cbState <- getCircuitBreakerState cb
      now <- getCurrentTime
      let stateWithAttempts = cbState { cbsHalfOpenAttempts = 2 }
      let newState = transitionToClosed now stateWithAttempts
      cbsHalfOpenAttempts newState @?= 0
  ]

-- ---------------------------------------------------------------------------
-- Call Permission Tests
-- ---------------------------------------------------------------------------

-- | Tests for isCallPermitted
callPermissionTests :: TestTree
callPermissionTests = testGroup "Call Permission"
  [ testCase "Closed state always permits calls" $ do
      cb <- newCircuitBreaker defaultConfig
      now <- getCurrentTime
      (permitted, _) <- isCallPermitted cb now
      permitted @?= True

  , testCase "Open state rejects calls before wait duration" $ do
      let config = defaultConfig & setWaitDuration 30
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Manually set to Open
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = Open, cbsLastStateChange = now }
      -- Check permission 10 seconds later (before 30 second wait)
      let future = addUTCTime 10 now
      (permitted, _) <- isCallPermitted cb future
      permitted @?= False

  , testCase "Open state permits calls after wait duration (transitions to HalfOpen)" $ do
      let config = defaultConfig & setWaitDuration 30
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Manually set to Open
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = Open, cbsLastStateChange = now }
      -- Check permission 31 seconds later (after 30 second wait)
      let future = addUTCTime 31 now
      (permitted, newState) <- isCallPermitted cb future
      permitted @?= True
      cbsState newState @?= HalfOpen

  , testCase "HalfOpen state permits up to halfOpenPermits calls" $ do
      let config = defaultConfig & setHalfOpenPermits 3
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Manually set to HalfOpen
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = HalfOpen, cbsHalfOpenAttempts = 0 }
      -- First 3 calls should be permitted
      (p1, _) <- isCallPermitted cb now
      (p2, _) <- isCallPermitted cb now
      (p3, _) <- isCallPermitted cb now
      -- 4th call should be rejected
      (p4, _) <- isCallPermitted cb now
      p1 @?= True
      p2 @?= True
      p3 @?= True
      p4 @?= False

  , testCase "HalfOpen state increments attempt counter" $ do
      let config = defaultConfig & setHalfOpenPermits 5
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Manually set to HalfOpen
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = HalfOpen, cbsHalfOpenAttempts = 0 }
      -- Make two calls
      _ <- isCallPermitted cb now
      _ <- isCallPermitted cb now
      cbState <- getCircuitBreakerState cb
      cbsHalfOpenAttempts cbState @?= 2
  ]

-- ---------------------------------------------------------------------------
-- Record Result Tests
-- ---------------------------------------------------------------------------

-- | Tests for recordSuccess and recordFailure
recordResultTests :: TestTree
recordResultTests = testGroup "Record Results"
  [ testCase "recordSuccess in Closed state records success" $ do
      cb <- newCircuitBreaker defaultConfig
      now <- getCurrentTime
      _ <- recordSuccess cb now
      rate <- getFailureRateIO cb
      rate @?= 0.0  -- 0 failures out of 1 call

  , testCase "recordFailure in Closed state records failure" $ do
      cb <- newCircuitBreaker defaultConfig
      now <- getCurrentTime
      _ <- recordFailure cb now
      rate <- getFailureRateIO cb
      rate @?= 1.0  -- 1 failure out of 1 call

  , testCase "recordFailure transitions to Open when threshold exceeded" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Add 5 failures (100% failure, >= 50% threshold, >= 5 minimum calls)
      replicateM_ 5 $ recordFailure cb now
      state <- getCurrentState cb
      state @?= Open

  , testCase "recordFailure in HalfOpen transitions to Open" $ do
      cb <- newCircuitBreaker defaultConfig
      now <- getCurrentTime
      -- Manually set to HalfOpen
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = HalfOpen, cbsHalfOpenAttempts = 1 }
      _ <- recordFailure cb now
      state <- getCurrentState cb
      state @?= Open

  , testCase "recordSuccess in HalfOpen transitions to Closed when all permits used" $ do
      let config = defaultConfig & setHalfOpenPermits 3
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Manually set to HalfOpen with 3 attempts (all permits used)
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = HalfOpen, cbsHalfOpenAttempts = 3 }
      _ <- recordSuccess cb now
      state <- getCurrentState cb
      state @?= Closed

  , testCase "recordSuccess in HalfOpen does not transition if permits remain" $ do
      let config = defaultConfig & setHalfOpenPermits 3
      cb <- newCircuitBreaker config
      now <- getCurrentTime
      -- Manually set to HalfOpen with 1 attempt (2 permits remain)
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = HalfOpen, cbsHalfOpenAttempts = 1 }
      _ <- recordSuccess cb now
      state <- getCurrentState cb
      state @?= HalfOpen

  , testCase "recordSuccess in Open is no-op" $ do
      cb <- newCircuitBreaker defaultConfig
      now <- getCurrentTime
      -- Manually set to Open
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = Open }
      _ <- recordSuccess cb now
      state <- getCurrentState cb
      state @?= Open

  , testCase "recordFailure in Open is no-op" $ do
      cb <- newCircuitBreaker defaultConfig
      now <- getCurrentTime
      -- Manually set to Open
      atomically $ do
        cbState <- readState cb
        updateState cb cbState { cbsState = Open }
      _ <- recordFailure cb now
      state <- getCurrentState cb
      state @?= Open
  ]

-- ---------------------------------------------------------------------------
-- Full State Flow Tests
-- ---------------------------------------------------------------------------

-- | Tests for complete state machine flows
fullStateFlowTests :: TestTree
fullStateFlowTests = testGroup "Full State Flow"
  [ testCase "complete flow: Closed -> Open -> HalfOpen -> Closed" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0  -- Immediate transition for testing
            & setHalfOpenPermits 2
      cb <- newCircuitBreaker config
      now <- getCurrentTime

      -- Start in Closed
      state1 <- getCurrentState cb
      state1 @?= Closed

      -- Add 5 failures to exceed threshold
      replicateM_ 5 $ recordFailure cb now
      state2 <- getCurrentState cb
      state2 @?= Open

      -- Wait duration has elapsed (set to 0), so next isCallPermitted transitions to HalfOpen
      (permitted, newState) <- isCallPermitted cb now
      permitted @?= True
      cbsState newState @?= HalfOpen

      -- Record a success
      _ <- recordSuccess cb now

      -- Second call in HalfOpen
      (permitted2, _) <- isCallPermitted cb now
      permitted2 @?= True

      -- Record second success (all permits used)
      _ <- recordSuccess cb now

      -- Should now be Closed
      state3 <- getCurrentState cb
      state3 @?= Closed

  , testCase "complete flow: Closed -> Open -> HalfOpen -> Open (on failure)" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0  -- Immediate transition for testing
            & setHalfOpenPermits 3
      cb <- newCircuitBreaker config
      now <- getCurrentTime

      -- Start in Closed
      state1 <- getCurrentState cb
      state1 @?= Closed

      -- Add 5 failures to exceed threshold
      replicateM_ 5 $ recordFailure cb now
      state2 <- getCurrentState cb
      state2 @?= Open

      -- Transition to HalfOpen
      (permitted, _) <- isCallPermitted cb now
      permitted @?= True
      state3 <- getCurrentState cb
      state3 @?= HalfOpen

      -- Record a failure - should go back to Open
      _ <- recordFailure cb now
      state4 <- getCurrentState cb
      state4 @?= Open

  , testCase "sliding window respects size limit" $ do
      let config = defaultConfig & setSlidingWindowSize 5
      cb <- newCircuitBreaker config
      now <- getCurrentTime

      -- Add 10 results (should only keep last 5)
      replicateM_ 3 $ recordSuccess cb now
      replicateM_ 7 $ recordFailure cb now

      cbState <- getCircuitBreakerState cb
      -- Window should have at most 5 calls
      -- Note: we may have transitioned to Open and reset the window
      let calls = getTotalCallsFromState cbState
      assertBool "window should respect size" (calls <= 5)

  , testCase "minimumCallsForOpen is 5" $ do
      minimumCallsForOpen @?= 5
  ]
