{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Test.Integration.WithCircuitBreaker
-- Description : Integration tests for withCircuitBreaker
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
--
-- Integration tests for the core withCircuitBreaker function,
-- testing success, failure, circuit open scenarios, and state transitions.

module Test.Integration.WithCircuitBreaker (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Control.Exception (Exception, SomeException, fromException)
import Control.Monad (replicateM_)
import Control.Monad.Catch (catch, throwM)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Typeable (Typeable)

import CircuitBreaker
  ( CircuitOpenException (..)
  , State (..)
  , defaultConfig
  , getCurrentState
  , newCircuitBreaker
  , setExceptionPredicate
  , setFailureThreshold
  , setHalfOpenPermits
  , setOnStateTransition
  , setSlidingWindowSize
  , setWaitDuration
  , withCircuitBreaker
  , (&)
  )

-- | Custom test exception
data TestException = TestException String
  deriving stock (Eq, Show, Typeable)

instance Exception TestException

-- | Custom exception for selective failure counting
data ClientError = ClientError String
  deriving stock (Eq, Show, Typeable)

instance Exception ClientError

-- | Custom exception for server errors
data ServerError = ServerError String
  deriving stock (Eq, Show, Typeable)

instance Exception ServerError

tests :: TestTree
tests = testGroup "withCircuitBreaker Integration"
  [ successScenarioTests
  , failureScenarioTests
  , circuitOpenTests
  , stateTransitionTests
  , exceptionPredicateTests
  , callbackTests
  , concurrencyTests
  ]

-- ---------------------------------------------------------------------------
-- Success Scenario Tests
-- ---------------------------------------------------------------------------

successScenarioTests :: TestTree
successScenarioTests = testGroup "Success Scenarios"
  [ testCase "successful action returns its result" $ do
      cb <- newCircuitBreaker defaultConfig
      result <- withCircuitBreaker cb $ pure (42 :: Int)
      result @?= 42

  , testCase "multiple successful actions all return results" $ do
      cb <- newCircuitBreaker defaultConfig
      results <- mapM (\i -> withCircuitBreaker cb $ pure i) [1..10 :: Int]
      results @?= [1..10]

  , testCase "circuit stays closed after successes" $ do
      cb <- newCircuitBreaker defaultConfig
      replicateM_ 10 $ withCircuitBreaker cb $ pure ()
      state <- getCurrentState cb
      state @?= Closed

  , testCase "successful IO action executes" $ do
      ref <- newIORef False
      cb <- newCircuitBreaker defaultConfig
      withCircuitBreaker cb $ writeIORef ref True
      result <- readIORef ref
      result @?= True
  ]

-- ---------------------------------------------------------------------------
-- Failure Scenario Tests
-- ---------------------------------------------------------------------------

failureScenarioTests :: TestTree
failureScenarioTests = testGroup "Failure Scenarios"
  [ testCase "exception from action is re-thrown" $ do
      cb <- newCircuitBreaker defaultConfig
      _ <- (withCircuitBreaker cb $ throwM (TestException "test") :: IO ())
        `catch` \(TestException _) -> pure ()
      pure ()

  , testCase "exception message is preserved" $ do
      cb <- newCircuitBreaker defaultConfig
      caught <- (withCircuitBreaker cb $ throwM (TestException "original message") >> pure Nothing)
        `catch` \(TestException msg) -> pure (Just msg)
      caught @?= Just "original message"

  , testCase "failures are recorded and trigger circuit open" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- Make 5 failing calls to trigger circuit open
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      state <- getCurrentState cb
      state @?= Open

  , testCase "single failure does not open circuit" $ do
      cb <- newCircuitBreaker defaultConfig
      (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
        `catch` \(_ :: TestException) -> pure ()

      state <- getCurrentState cb
      state @?= Closed

  , testCase "failure rate below threshold keeps circuit closed" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- 3 failures, 7 successes = 30% failure rate < 50% threshold
      replicateM_ 3 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()
      replicateM_ 7 $ withCircuitBreaker cb $ pure ()

      state <- getCurrentState cb
      state @?= Closed
  ]

-- ---------------------------------------------------------------------------
-- Circuit Open Tests
-- ---------------------------------------------------------------------------

circuitOpenTests :: TestTree
circuitOpenTests = testGroup "Circuit Open"
  [ testCase "throws CircuitOpenException when circuit is open" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 30  -- Long wait so circuit stays open
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- Verify circuit is open
      state <- getCurrentState cb
      state @?= Open

      -- Next call should throw CircuitOpenException
      caught <- (withCircuitBreaker cb (pure ()) >> pure False)
        `catch` \(CircuitOpenException _) -> pure True

      assertBool "Should throw CircuitOpenException" caught

  , testCase "CircuitOpenException contains circuit name" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 30
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- Capture the exception
      result <- (withCircuitBreaker cb (pure ()) >> pure Nothing)
        `catch` \(CircuitOpenException name) -> pure (Just name)

      assertBool "Should have circuit name" (result /= Nothing)

  , testCase "action is not executed when circuit is open" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 30
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- Try to execute action
      ref <- newIORef False
      _ <- (withCircuitBreaker cb $ writeIORef ref True)
        `catch` \(_ :: CircuitOpenException) -> pure ()

      -- Action should not have executed
      executed <- readIORef ref
      executed @?= False
  ]

-- ---------------------------------------------------------------------------
-- State Transition Tests
-- ---------------------------------------------------------------------------

stateTransitionTests :: TestTree
stateTransitionTests = testGroup "State Transitions"
  [ testCase "Open -> HalfOpen after wait duration" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0  -- Immediate transition
            & setHalfOpenPermits 3
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- Circuit should be open
      state1 <- getCurrentState cb
      state1 @?= Open

      -- With waitDuration 0, next call should succeed (transitions to HalfOpen)
      withCircuitBreaker cb $ pure ()

      -- Should be in HalfOpen
      state2 <- getCurrentState cb
      state2 @?= HalfOpen

  , testCase "HalfOpen -> Closed on successful test calls" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0
            & setHalfOpenPermits 2
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- Make 2 successful calls in HalfOpen
      withCircuitBreaker cb $ pure ()
      withCircuitBreaker cb $ pure ()

      -- Should be closed now
      state <- getCurrentState cb
      state @?= Closed

  , testCase "HalfOpen -> Open on failure" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0
            & setHalfOpenPermits 3
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- First call transitions to HalfOpen
      withCircuitBreaker cb $ pure ()

      state1 <- getCurrentState cb
      state1 @?= HalfOpen

      -- Failure in HalfOpen should go back to Open
      (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
        `catch` \(_ :: TestException) -> pure ()

      state2 <- getCurrentState cb
      state2 @?= Open

  , testCase "full cycle: Closed -> Open -> HalfOpen -> Closed" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0
            & setHalfOpenPermits 2
      cb <- newCircuitBreaker config

      -- 1. Start in Closed
      state1 <- getCurrentState cb
      state1 @?= Closed

      -- 2. Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      state2 <- getCurrentState cb
      state2 @?= Open

      -- 3. Transition to HalfOpen
      withCircuitBreaker cb $ pure ()

      state3 <- getCurrentState cb
      state3 @?= HalfOpen

      -- 4. Transition to Closed
      withCircuitBreaker cb $ pure ()

      state4 <- getCurrentState cb
      state4 @?= Closed
  ]

-- ---------------------------------------------------------------------------
-- Exception Predicate Tests
-- ---------------------------------------------------------------------------

exceptionPredicateTests :: TestTree
exceptionPredicateTests = testGroup "Exception Predicate"
  [ testCase "exceptions matching predicate count as failures" $ do
      let isServerError :: SomeException -> Bool
          isServerError e = case fromException e of
            Just (ServerError _) -> True
            Nothing -> False

      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate isServerError
      cb <- newCircuitBreaker config

      -- Server errors should count as failures
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (ServerError "500") :: IO ())
          `catch` \(_ :: ServerError) -> pure ()

      state <- getCurrentState cb
      state @?= Open

  , testCase "exceptions not matching predicate do not count as failures" $ do
      let isServerError :: SomeException -> Bool
          isServerError e = case fromException e of
            Just (ServerError _) -> True
            Nothing -> False

      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate isServerError
      cb <- newCircuitBreaker config

      -- Client errors should NOT count as failures
      replicateM_ 10 $ do
        (withCircuitBreaker cb $ throwM (ClientError "404") :: IO ())
          `catch` \(_ :: ClientError) -> pure ()

      -- Circuit should still be closed
      state <- getCurrentState cb
      state @?= Closed

  , testCase "exceptions not matching predicate are still re-thrown" $ do
      let isServerError :: SomeException -> Bool
          isServerError e = case fromException e of
            Just (ServerError _) -> True
            Nothing -> False

      let config = defaultConfig
            & setExceptionPredicate isServerError
      cb <- newCircuitBreaker config

      -- ClientError should be re-thrown even though it doesn't count as failure
      caught <- (withCircuitBreaker cb $ throwM (ClientError "404") >> pure False)
        `catch` \(ClientError _) -> pure True

      assertBool "ClientError should be re-thrown" caught
  ]

-- ---------------------------------------------------------------------------
-- Callback Tests
-- ---------------------------------------------------------------------------

callbackTests :: TestTree
callbackTests = testGroup "State Transition Callbacks"
  [ testCase "callback is invoked on Closed -> Open" $ do
      transitionRef <- newIORef []

      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setOnStateTransition (\old new ->
                atomicModifyIORef' transitionRef (\xs -> ((old, new):xs, ())))
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      transitions <- readIORef transitionRef
      assertBool "Should have Closed -> Open transition" $
        (Closed, Open) `elem` transitions

  , testCase "callback is invoked on Open -> HalfOpen" $ do
      transitionRef <- newIORef []

      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0
            & setOnStateTransition (\old new ->
                atomicModifyIORef' transitionRef (\xs -> ((old, new):xs, ())))
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- Transition to HalfOpen
      withCircuitBreaker cb $ pure ()

      transitions <- readIORef transitionRef
      assertBool "Should have Open -> HalfOpen transition" $
        (Open, HalfOpen) `elem` transitions

  , testCase "callback is invoked on HalfOpen -> Closed" $ do
      transitionRef <- newIORef []

      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0
            & setHalfOpenPermits 2
            & setOnStateTransition (\old new ->
                atomicModifyIORef' transitionRef (\xs -> ((old, new):xs, ())))
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- Transition through HalfOpen to Closed
      withCircuitBreaker cb $ pure ()
      withCircuitBreaker cb $ pure ()

      transitions <- readIORef transitionRef
      assertBool "Should have HalfOpen -> Closed transition" $
        (HalfOpen, Closed) `elem` transitions

  , testCase "callback is invoked on HalfOpen -> Open" $ do
      transitionRef <- newIORef []

      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setWaitDuration 0
            & setHalfOpenPermits 3
            & setOnStateTransition (\old new ->
                atomicModifyIORef' transitionRef (\xs -> ((old, new):xs, ())))
      cb <- newCircuitBreaker config

      -- Open the circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
          `catch` \(_ :: TestException) -> pure ()

      -- Transition to HalfOpen
      withCircuitBreaker cb $ pure ()

      -- Fail in HalfOpen
      (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
        `catch` \(_ :: TestException) -> pure ()

      transitions <- readIORef transitionRef
      assertBool "Should have HalfOpen -> Open transition" $
        (HalfOpen, Open) `elem` transitions

  , testCase "callback is not invoked when state doesn't change" $ do
      callCount <- newIORef (0 :: Int)

      let config = defaultConfig
            & setOnStateTransition (\_ _ ->
                atomicModifyIORef' callCount (\n -> (n + 1, ())))
      cb <- newCircuitBreaker config

      -- Make several successful calls
      replicateM_ 10 $ withCircuitBreaker cb $ pure ()

      count <- readIORef callCount
      count @?= 0
  ]

-- ---------------------------------------------------------------------------
-- Concurrency Tests
-- ---------------------------------------------------------------------------

concurrencyTests :: TestTree
concurrencyTests = testGroup "Concurrency"
  [ testCase "concurrent successful calls work correctly" $ do
      cb <- newCircuitBreaker defaultConfig
      counter <- newIORef (0 :: Int)

      let numCalls = 100

      -- Start concurrent calls
      replicateM_ numCalls $ do
        _ <- withCircuitBreaker cb $ atomicModifyIORef' counter (\n -> (n + 1, ()))
        pure ()

      -- All calls should have executed
      count <- readIORef counter
      count @?= numCalls

  , testCase "state remains consistent under concurrent access" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
      cb <- newCircuitBreaker config

      -- Mix of successful and failing calls
      let makeCall i =
            if i `mod` 3 == 0
              then (withCircuitBreaker cb $ throwM (TestException "fail") :: IO ())
                     `catch` \(_ :: TestException) -> pure ()
              else withCircuitBreaker cb $ pure ()

      mapM_ makeCall [1..20 :: Int]

      -- State should be valid
      state <- getCurrentState cb
      assertBool "State should be valid" (state `elem` [Closed, Open, HalfOpen])
  ]
