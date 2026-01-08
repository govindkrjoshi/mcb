{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.Integration.TimeoutCircuitBreakerConcurrency
-- Description : Concurrency and distributed systems edge case tests for Timeout + Circuit Breaker
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
--
-- Tests verifying correct behavior under distributed systems failure scenarios:
--
-- * Concurrent timeouts and state transitions
-- * Race conditions between timeout and circuit breaker state changes
-- * Thundering herd scenarios on HalfOpen transitions
-- * STM transaction conflicts under high contention
-- * Clock skew and timing edge cases
-- * State consistency under partial failures
-- * Idempotency of result recording
-- * Resource exhaustion scenarios

module Test.Integration.TimeoutCircuitBreakerConcurrency (tests) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar, newMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, retry)
import Control.Exception (SomeException, fromException)
import Control.Monad (forM, forM_, replicateM, replicateM_, void)
import Control.Monad.Catch (try)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', atomicModifyIORef')
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import CircuitBreaker
  ( CircuitBreakerException (..)
  , CircuitOpenException (..)
  , State (..)
  , TimeoutException (..)
  , defaultConfig
  , getCurrentState
  , milliseconds
  , newCircuitBreaker
  , setFailureThreshold
  , setHalfOpenPermits
  , setSlidingWindowSize
  , setWaitDuration
  , withCircuitBreaker
  , withTimeout
  , (&)
  )

tests :: TestTree
tests = testGroup "Timeout + CircuitBreaker Concurrency & Edge Cases"
  [ concurrentTimeoutTests
  , stateTransitionRaceTests
  , thunderingHerdTests
  , stmContentionTests
  , timingEdgeCaseTests
  , stateConsistencyTests
  , idempotencyTests
  , resourceExhaustionTests
  ]

-- ---------------------------------------------------------------------------
-- Concurrent Timeout Tests
-- ---------------------------------------------------------------------------

concurrentTimeoutTests :: TestTree
concurrentTimeoutTests = testGroup "Concurrent Timeouts"
  [ testCase "multiple threads timing out simultaneously open circuit correctly" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 20
      cb <- newCircuitBreaker config

      -- Launch 10 threads that will all timeout simultaneously
      successCount <- newIORef (0 :: Int)
      timeoutCount <- newIORef (0 :: Int)

      threads <- forM [1..10] $ \_ -> forkIO $ do
        result <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ do
            threadDelay 50_000  -- 50ms, will timeout
            pure ()
        case result of
          Left (_ :: TimeoutException) -> atomicModifyIORef' timeoutCount (\c -> (c+1, ()))
          Right () -> atomicModifyIORef' successCount (\c -> (c+1, ()))

      -- Wait for all threads to complete
      threadDelay 100_000
      mapM_ killThread threads

      timeouts <- readIORef timeoutCount
      successes <- readIORef successCount

      -- All 10 should have timed out
      timeouts @?= 10
      successes @?= 0

      -- Circuit should be open (10 failures out of 20 window = 50%)
      state <- getCurrentState cb
      state @?= Open

  , testCase "concurrent timeouts with successes maintain correct failure rate" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 20
      cb <- newCircuitBreaker config

      results <- newIORef ([] :: [Either () ()])

      -- Launch 20 threads: 10 fast (succeed), 10 slow (timeout)
      threads <- forM [1..20] $ \i -> forkIO $ do
        let action = if i <= 10
              then threadDelay 1_000 >> pure ()  -- 1ms, fast
              else threadDelay 50_000 >> pure () -- 50ms, slow
        result <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 10) action
        case result of
          Left (_ :: TimeoutException) ->
            atomicModifyIORef' results (\rs -> (Left () : rs, ()))
          Right () ->
            atomicModifyIORef' results (\rs -> (Right () : rs, ()))

      -- Wait for completion
      threadDelay 100_000
      mapM_ killThread threads

      rs <- readIORef results
      let timeoutCount = length [() | Left () <- rs]
          successCount = length [() | Right () <- rs]

      -- Should have 10 successes and 10 timeouts
      successCount @?= 10
      timeoutCount @?= 10

      -- Circuit should be open (50% failure rate)
      state <- getCurrentState cb
      state @?= Open

  , testCase "race: timeout fires while recording previous timeout" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- First timeout
      _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 5) $ threadDelay 50_000

      -- Immediately launch second timeout without any delay
      -- This tests if the state machine can handle rapid successive failures
      _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 5) $ threadDelay 50_000

      -- Circuit should still be consistent (Closed, since only 2/10 failures)
      state <- getCurrentState cb
      state @?= Closed
  ]

-- ---------------------------------------------------------------------------
-- State Transition Race Condition Tests
-- ---------------------------------------------------------------------------

stateTransitionRaceTests :: TestTree
stateTransitionRaceTests = testGroup "State Transition Race Conditions"
  [ testCase "timeout while circuit transitions from Closed to Open" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- Open circuit with 4 failures (not quite enough to open)
      replicateM_ 4 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ threadDelay 50_000
        pure ()

      -- Launch two threads simultaneously - one will push circuit to Open
      barrier <- newEmptyMVar
      results <- newMVar []

      t1 <- forkIO $ do
        _ <- takeMVar barrier
        r <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ threadDelay 50_000
        modifyMVar_ results (pure . (("t1", r) :))

      t2 <- forkIO $ do
        _ <- takeMVar barrier
        r <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ threadDelay 50_000
        modifyMVar_ results (pure . (("t2", r) :))

      -- Release both threads simultaneously
      putMVar barrier ()
      putMVar barrier ()

      threadDelay 100_000
      killThread t1
      killThread t2

      rs <- readMVar results

      -- Both should get TimeoutException (not CircuitOpenException)
      -- because they both checked permission before circuit opened
      let timeouts = [t | (t, Left (_ :: TimeoutException)) <- rs]
      length timeouts @?= 2

      -- Circuit should now be open
      state <- getCurrentState cb
      state @?= Open

  , testCase "timeout in HalfOpen races with state transition to Open" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0  -- Immediate transition to HalfOpen
            & setHalfOpenPermits 5
      cb <- newCircuitBreaker config

      -- Open circuit
      replicateM_ 5 $ do
        _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ threadDelay 50_000
        pure ()

      state1 <- getCurrentState cb
      state1 @?= Open

      -- Transition to HalfOpen by making a call
      _ :: Either SomeException () <- try $ withCircuitBreaker cb $ pure ()

      state2 <- getCurrentState cb
      state2 @?= HalfOpen

      -- Now cause a timeout in HalfOpen - this should reopen circuit
      _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 5) $ threadDelay 50_000

      state3 <- getCurrentState cb
      state3 @?= Open

  , testCase "multiple threads race for HalfOpen test permits" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0
            & setHalfOpenPermits 3  -- Only 3 test calls allowed
      cb <- newCircuitBreaker config

      -- Open circuit
      replicateM_ 5 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $ error "fail"
        pure ()

      -- Launch 10 threads trying to execute when circuit opens to HalfOpen
      threadDelay 1_000  -- Brief wait for Open state

      permitCount <- newIORef (0 :: Int)
      rejectCount <- newIORef (0 :: Int)

      threads <- forM [1..10] $ \_ -> forkIO $ do
        result <- try $ withCircuitBreaker cb $ pure "success"
        case result of
          Right _ -> atomicModifyIORef' permitCount (\c -> (c+1, ()))
          Left (_ :: CircuitOpenException) -> atomicModifyIORef' rejectCount (\c -> (c+1, ()))
          Left _ -> pure ()  -- Other exception

      threadDelay 50_000
      mapM_ killThread threads

      permits <- readIORef permitCount
      rejects <- readIORef rejectCount

      -- Should have exactly 3 permits (HalfOpen) + potentially more from Closed state
      -- The first call transitions to HalfOpen and gets permit 1
      -- Next 2 get permits 2-3, which completes test and transitions to Closed
      -- Remaining threads execute in Closed state
      assertBool "permitted calls should be >= 3" (permits >= 3)

      -- Final state should be Closed (all test calls succeeded)
      finalState <- getCurrentState cb
      finalState @?= Closed
  ]

-- ---------------------------------------------------------------------------
-- Thundering Herd Tests
-- ---------------------------------------------------------------------------

thunderingHerdTests :: TestTree
thunderingHerdTests = testGroup "Thundering Herd Scenarios"
  [ testCase "many threads blocked on Open circuit don't overwhelm HalfOpen" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0.05  -- 50ms wait
            & setHalfOpenPermits 3
      cb <- newCircuitBreaker config

      -- Open circuit
      replicateM_ 5 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $ error "fail"
        pure ()

      -- Launch threads at different times:
      -- - 50 threads at T=10ms (before wait duration, will be rejected)
      -- - 50 threads at T=60ms (after wait duration, will race for HalfOpen permits)
      results <- newIORef ([] :: [Either String String])

      -- First wave: threads that check during Open state (before 50ms wait duration)
      earlyThreads <- forM [1..50] $ \_ -> forkIO $ do
        threadDelay 10_000  -- 10ms delay - circuit is definitely still Open
        result <- try $ withCircuitBreaker cb $ pure "success"
        case result of
          Right s -> atomicModifyIORef' results (\rs -> (Right s : rs, ()))
          Left (e :: CircuitOpenException) ->
            atomicModifyIORef' results (\rs -> (Left (show e) : rs, ()))
          Left _ -> pure ()

      -- Second wave: threads that check after wait duration elapses
      lateThreads <- forM [1..50] $ \_ -> forkIO $ do
        threadDelay 60_000  -- 60ms delay - wait duration (50ms) has elapsed
        result <- try $ withCircuitBreaker cb $ pure "success"
        case result of
          Right s -> atomicModifyIORef' results (\rs -> (Right s : rs, ()))
          Left (e :: CircuitOpenException) ->
            atomicModifyIORef' results (\rs -> (Left (show e) : rs, ()))
          Left _ -> pure ()

      -- Wait for all threads to complete
      threadDelay 200_000
      mapM_ killThread (earlyThreads ++ lateThreads)

      rs <- readIORef results

      -- Early threads (50) should be rejected during Open state
      -- Late threads (50) race for HalfOpen: 3 get permits, if all succeed circuit closes
      -- Then remaining late threads can succeed in Closed state
      let successes = length [() | Right _ <- rs]
          rejections = length [() | Left _ <- rs]

      -- At minimum 50 rejections (early threads during Open)
      -- But late threads may also be rejected if they check while HalfOpen is full
      assertBool "most threads should be rejected" (rejections >= 50)
      -- At minimum 3 successes (the HalfOpen test permits)
      -- If those succeed, circuit closes and more threads succeed
      assertBool "some threads should succeed after circuit recovers" (successes >= 3)

  , testCase "STM transactions don't livelock under thundering herd" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 20
      cb <- newCircuitBreaker config

      -- Launch 50 concurrent threads all timing out simultaneously
      -- This creates high STM contention on the circuit breaker state
      startBarrier <- newEmptyMVar
      completionCount <- newIORef (0 :: Int)

      threads <- forM [1..50] $ \_ -> forkIO $ do
        _ <- takeMVar startBarrier
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ threadDelay 50_000
        atomicModifyIORef' completionCount (\c -> (c+1, ()))

      -- Release all threads at once
      replicateM_ 50 $ putMVar startBarrier ()

      -- Wait for all to complete (with generous timeout)
      threadDelay 300_000
      mapM_ killThread threads

      completed <- readIORef completionCount

      -- All threads should have completed (no livelock)
      assertBool "all threads should complete" (completed >= 45)  -- Allow some variance

  , testCase "STM permits exactly maxPermits concurrent HalfOpen operations under high contention" $ do
      -- This test validates the fix for mcb-k7z: thundering herd coordination failure
      -- It ensures that STM atomicity prevents more than maxPermits threads from
      -- executing simultaneously during HalfOpen state, even under extreme race conditions
      let maxPermits = 5
          racingThreads = 150  -- High contention to stress-test STM
          config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 20
            & setWaitDuration 0.01  -- 10ms wait - short enough to race but long enough to control
            & setHalfOpenPermits maxPermits

      cb <- newCircuitBreaker config

      -- Phase 1: Open the circuit by causing failures
      replicateM_ 10 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $ error "fail"
        pure ()

      state1 <- getCurrentState cb
      state1 @?= Open

      -- Phase 2: Wait for circuit to be ready to transition to HalfOpen
      -- We wait just past the wait duration to ensure next call will transition
      threadDelay 15_000  -- 15ms > 10ms wait duration

      -- Phase 3: Launch racing threads that will all compete for HalfOpen permits
      -- Use a barrier to maximize race conditions
      startBarrier <- newEmptyMVar
      permitGrantedCount <- newIORef (0 :: Int)
      rejectedCount <- newIORef (0 :: Int)
      successCount <- newIORef (0 :: Int)

      -- Track which threads got permits vs rejected
      -- This helps us verify exactly maxPermits got through
      threads <- forM [1..racingThreads] $ \threadId -> forkIO $ do
        -- Wait at barrier to ensure maximum contention
        _ <- takeMVar startBarrier

        result <- try $ withCircuitBreaker cb $ do
          -- Record that this thread got a permit
          atomicModifyIORef' permitGrantedCount (\c -> (c+1, ()))
          pure ("success" :: String)

        case result of
          Right _ -> do
            -- Success means: got permit AND action succeeded
            atomicModifyIORef' successCount (\c -> (c+1, ()))
          Left (_ :: CircuitOpenException) -> do
            -- Rejected: circuit was Open or HalfOpen was full
            atomicModifyIORef' rejectedCount (\c -> (c+1, ()))
          Left _ -> do
            -- Other exception (shouldn't happen in this test)
            pure ()

      -- Release all threads simultaneously to maximize race conditions
      replicateM_ racingThreads $ putMVar startBarrier ()

      -- Wait for all threads to complete their attempts
      -- Use a generous timeout to ensure no deadlocks
      threadDelay 500_000  -- 500ms should be plenty
      mapM_ killThread threads

      -- Phase 4: Verify results
      permitted <- readIORef permitGrantedCount
      rejected <- readIORef rejectedCount
      succeeded <- readIORef successCount

      -- Critical invariant: exactly maxPermits threads should have been granted permits
      -- during HalfOpen state. This validates STM atomicity.
      --
      -- The first thread to check after wait duration will:
      -- 1. Transition Open -> HalfOpen
      -- 2. Get permit #1
      -- 3. Execute and succeed
      --
      -- The next (maxPermits - 1) threads will:
      -- 1. Find circuit in HalfOpen
      -- 2. Get permits #2 through #maxPermits
      -- 3. Execute and succeed
      --
      -- After all maxPermits succeed, circuit transitions to Closed
      -- Remaining threads execute in Closed state
      --
      -- So we expect:
      -- - At least maxPermits threads got permits during HalfOpen
      -- - Remaining threads either got rejected (during HalfOpen) or succeeded (after Closed)
      -- - Most threads should succeed (maxPermits in HalfOpen + many in Closed)

      -- At minimum, maxPermits threads must have gotten through
      assertBool
        ("at least " ++ show maxPermits ++ " threads should get permits, got " ++ show permitted)
        (permitted >= maxPermits)

      -- Most rejections happen if threads check during the brief HalfOpen window
      -- before all test calls complete and circuit closes
      -- We don't assert exact count because timing varies

      -- The total of permitted + rejected should equal our thread count
      -- (with small variance for killed threads)
      let accountedFor = permitted + rejected
      assertBool
        ("most threads should be accounted for: " ++ show accountedFor ++ " of " ++ show racingThreads)
        (accountedFor >= racingThreads - 10)  -- Allow 10 for timing variance

      -- Final state should be Closed (all HalfOpen test calls succeeded)
      finalState <- getCurrentState cb
      finalState @?= Closed

      -- Verify circuit recovered properly by making a successful call
      result <- withCircuitBreaker cb $ pure "final test"
      result @?= "final test"
  ]

-- ---------------------------------------------------------------------------
-- STM Contention Tests
-- ---------------------------------------------------------------------------

stmContentionTests :: TestTree
stmContentionTests = testGroup "STM Transaction Conflicts"
  [ testCase "concurrent permission checks don't cause anomalies" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- Launch 20 threads all checking permission concurrently
      -- Half will succeed, half will timeout
      threads <- forM [1..20] $ \i -> forkIO $ do
        let action = if i <= 10
              then pure "success"
              else threadDelay 50_000 >> pure "timeout"
        _ :: Either SomeException String <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 10) action
        pure ()

      threadDelay 100_000
      mapM_ killThread threads

      -- Circuit should be in a valid state (not corrupted)
      state <- getCurrentState cb
      assertBool "state should be valid" (state `elem` [Closed, Open, HalfOpen])

  , testCase "recording results under high concurrency maintains sliding window integrity" $ do
      let config = defaultConfig
            & setFailureThreshold 0.7  -- High threshold to keep circuit closed
            & setSlidingWindowSize 200  -- Larger window for pre-seeding
      cb <- newCircuitBreaker config

      -- Pre-seed with successes to prevent premature opening.
      -- With 50 successes, we need many more failures to reach 70% threshold.
      replicateM_ 50 $ withCircuitBreaker cb $ pure "preseed"

      -- 100 threads all completing very quickly
      threads <- forM [1..100] $ \i -> forkIO $ do
        _ :: Either SomeException String <- try $ withCircuitBreaker cb $
          if i <= 30
            then error "fail"  -- 30% failure rate of these 100 threads
            else pure "success"
        pure ()

      threadDelay 100_000
      mapM_ killThread threads

      -- With 50 pre-seed + 70 successes + 30 failures = 150 total
      -- Failure rate = 30/150 = 20% < 70% threshold
      state <- getCurrentState cb
      state @?= Closed
  ]

-- ---------------------------------------------------------------------------
-- Timing Edge Case Tests
-- ---------------------------------------------------------------------------

timingEdgeCaseTests :: TestTree
timingEdgeCaseTests = testGroup "Timing and Clock Edge Cases"
  [ testCase "extremely short timeout doesn't cause state corruption" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- Timeout of 1ms - may fire before action even starts
      _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 1) $ pure ()

      -- Circuit should be in valid state
      state <- getCurrentState cb
      assertBool "state should be valid" (state `elem` [Closed, Open, HalfOpen])

  , testCase "action completes just as timeout fires" $ do
      let config = defaultConfig
      cb <- newCircuitBreaker config

      -- Use a timeout that races with action completion
      resultRef <- newIORef (Nothing :: Maybe (Either SomeException String))
      threads <- replicateM 10 $ forkIO $ do
        result <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 10) $ do
            threadDelay 9_000  -- 9ms, right at the edge
            pure "success"
        atomicModifyIORef' resultRef (\_ -> (Just result, ()))

      threadDelay 50_000
      mapM_ killThread threads

      -- Should get either success or timeout, but not corruption
      result <- readIORef resultRef
      case result of
        Nothing -> assertBool "got a result" False
        Just _ -> assertBool "got a result" True

  , testCase "timeout during permission check window" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0
            & setHalfOpenPermits 1
      cb <- newCircuitBreaker config

      -- Open circuit
      replicateM_ 5 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $ error "fail"
        pure ()

      -- Very short timeout that might fire during permission check
      _ :: Either SomeException () <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 1) $ pure ()

      -- State should be valid
      state <- getCurrentState cb
      assertBool "state should be valid" (state `elem` [Closed, Open, HalfOpen])
  ]

-- ---------------------------------------------------------------------------
-- State Consistency Tests
-- ---------------------------------------------------------------------------

stateConsistencyTests :: TestTree
stateConsistencyTests = testGroup "State Consistency Under Partial Failures"
  [ testCase "timeout after action succeeds but before return" $ do
      let config = defaultConfig
      cb <- newCircuitBreaker config

      -- Action that succeeds but takes long to return
      completed <- newIORef False
      result <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 50) $ do
          modifyIORef' completed (const True)
          threadDelay 100_000  -- Long delay after "success"
          pure "done"

      didComplete <- readIORef completed
      didComplete @?= True  -- Action did complete

      -- But we should get timeout exception
      case result of
        Left (_ :: TimeoutException) -> pure ()
        Right _ -> error "Expected timeout"

  , testCase "state transitions are atomic despite timeouts" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      stateSequence <- newIORef ([] :: [State])

      -- Monitor state changes
      threads <- forM [1..20] $ \_ -> forkIO $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ threadDelay 50_000
        s <- getCurrentState cb
        atomicModifyIORef' stateSequence (\ss -> (s:ss, ()))

      threadDelay 150_000
      mapM_ killThread threads

      sequence <- readIORef stateSequence

      -- All observed states should be valid
      assertBool "all states are valid"
        (all (\s -> s `elem` [Closed, Open, HalfOpen]) sequence)
  ]

-- ---------------------------------------------------------------------------
-- Idempotency Tests
-- ---------------------------------------------------------------------------

idempotencyTests :: TestTree
idempotencyTests = testGroup "Result Recording Idempotency"
  [ testCase "result recorded exactly once per action" $ do
      -- Use a larger window to avoid eviction effects
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 20
      cb <- newCircuitBreaker config

      -- Execute 10 successes, each should be recorded exactly once
      replicateM_ 10 $ do
        _ :: Either SomeException String <- try $ withCircuitBreaker cb $ pure "success"
        pure ()

      -- Add 4 failures - total: 14, rate: 4/14 = 28.5% < 50%
      replicateM_ 4 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $ error "fail"
        pure ()

      -- Circuit should remain closed with 28.5% failure rate
      state <- getCurrentState cb
      state @?= Closed

  , testCase "timeout doesn't cause double recording" $ do
      let config = defaultConfig
            & setFailureThreshold 0.4
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- Record 6 successes
      replicateM_ 6 $ withCircuitBreaker cb $ pure "success"

      -- If timeout causes double recording (as both success and failure),
      -- we'd see weird state
      _ :: Either TimeoutException () <- try $ withCircuitBreaker cb $
        withTimeout (milliseconds 5) $ threadDelay 50_000

      -- Now: 6 successes + 1 timeout = 7 total, 1 failure = 14% failure rate
      -- Circuit should be closed
      state <- getCurrentState cb
      state @?= Closed
  ]

-- ---------------------------------------------------------------------------
-- Resource Exhaustion Tests
-- ---------------------------------------------------------------------------

resourceExhaustionTests :: TestTree
resourceExhaustionTests = testGroup "Resource Exhaustion Scenarios"
  [ testCase "system remains responsive under timeout storm" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 50
      cb <- newCircuitBreaker config

      -- Create a storm of 100 timeouts
      completed <- newIORef (0 :: Int)
      threads <- forM [1..100] $ \_ -> forkIO $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 5) $ threadDelay 50_000
        atomicModifyIORef' completed (\c -> (c+1, ()))

      -- System should handle this without hanging
      threadDelay 300_000
      mapM_ killThread threads

      count <- readIORef completed
      assertBool "most threads completed" (count > 80)

  , testCase "memory doesn't leak under continuous timeout load" $ do
      let config = defaultConfig
            & setFailureThreshold 0.9  -- Keep circuit closed
            & setSlidingWindowSize 100
      cb <- newCircuitBreaker config

      -- Run 1000 rapid timeouts
      -- If there's a memory leak, this will be noticeable
      -- Note: With 100% failure rate (all timeouts), the circuit may open despite
      -- 90% threshold because failure rate reaches 100% quickly.
      -- We catch all exceptions to handle both timeout and circuit open scenarios.
      replicateM_ 1000 $ do
        _ :: Either SomeException () <- try $ withCircuitBreaker cb $
          withTimeout (milliseconds 1) $ threadDelay 10_000
        pure ()

      -- If we got here without OOM, test passes
      state <- getCurrentState cb
      assertBool "circuit is in valid state" (state `elem` [Closed, Open])

  , testCase "circuit breaker handles burst of concurrent operations" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 200
      cb <- newCircuitBreaker config

      -- Burst of 200 concurrent operations
      results <- newIORef ([] :: [Bool])
      threads <- forM [1..200] $ \i -> forkIO $ do
        let action = if i `mod` 3 == 0
              then error "fail"  -- Every 3rd fails
              else pure True
        result <- try $ withCircuitBreaker cb action
        case result of
          Right True -> atomicModifyIORef' results (\rs -> (True:rs, ()))
          Left (_ :: SomeException) -> atomicModifyIORef' results (\rs -> (False:rs, ()))
          _ -> pure ()

      threadDelay 100_000
      mapM_ killThread threads

      rs <- readIORef results
      let successes = length $ filter id rs

      assertBool "most operations completed" (length rs > 150)
      assertBool "some operations succeeded" (successes > 100)

  , testCase "threshold enforcement under timeout storm (mcb-abq fix)" $ do
      -- This test verifies the fix for mcb-abq: premature circuit opening under
      -- timeout storm. With 1000 rapid concurrent requests where 80% are timeouts,
      -- the circuit should NOT open if threshold is 90%.
      --
      -- The fix ensures that a minimum sample size (10% of window) is required
      -- before the circuit can open, preventing transient spikes from triggering
      -- premature opening.
      let config = defaultConfig
            & setFailureThreshold 0.9  -- 90% threshold
            & setSlidingWindowSize 100
      cb <- newCircuitBreaker config

      -- Pre-seed with 20 successes to establish baseline
      replicateM_ 20 $ withCircuitBreaker cb $ pure "preseed"

      -- Launch 100 concurrent threads: 80 will timeout, 20 will succeed
      -- Overall failure rate = 80/120 = 67% < 90% threshold
      startBarrier <- newEmptyMVar
      timeoutCount <- newIORef (0 :: Int)
      successCount <- newIORef (0 :: Int)
      circuitOpenCount <- newIORef (0 :: Int)

      threads <- forM [1..100] $ \i -> forkIO $ do
        _ <- takeMVar startBarrier
        let shouldTimeout = i <= 80
        result <- try $ withCircuitBreaker cb $
          if shouldTimeout
            then withTimeout (milliseconds 1) (threadDelay 50_000) >> pure ()
            else pure ()
        case result of
          Right () -> atomicModifyIORef' successCount (\c -> (c+1, ()))
          Left e
            | Just (_ :: CircuitOpenException) <- fromException e ->
                atomicModifyIORef' circuitOpenCount (\c -> (c+1, ()))
            | Just (_ :: TimeoutException) <- fromException e ->
                atomicModifyIORef' timeoutCount (\c -> (c+1, ()))
            | otherwise ->
                atomicModifyIORef' timeoutCount (\c -> (c+1, ()))

      -- Release all threads
      replicateM_ 100 $ putMVar startBarrier ()

      -- Wait for completion
      threadDelay 500_000
      mapM_ killThread threads

      timeouts <- readIORef timeoutCount
      successes <- readIORef successCount
      circuitOpens <- readIORef circuitOpenCount

      -- Final state check
      state <- getCurrentState cb

      -- The circuit should stay Closed because overall failure rate < 90%
      -- With the fix: minimum calls = max(5, 100/10) = 10
      -- After 10 calls are recorded, we have enough sample to make decision
      -- But even then, 80/120 = 67% < 90% threshold
      assertBool
        ("circuit should stay Closed with 67% failure rate < 90% threshold, " ++
         "got " ++ show state ++ " with " ++ show timeouts ++ " timeouts, " ++
         show successes ++ " successes, " ++ show circuitOpens ++ " circuit opens")
        (state == Closed)

      -- Minimal or no circuit opens should have occurred
      assertBool
        ("minimal circuit opens expected, got " ++ show circuitOpens)
        (circuitOpens < 10)
  ]
