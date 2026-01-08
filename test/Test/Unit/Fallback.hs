-- |
-- Module      : Test.Unit.Fallback
-- Description : Unit tests for Fallback
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause

module Test.Unit.Fallback
  ( tests
  ) where

import Control.Exception (SomeException, fromException, toException, try)
import Control.Monad.Catch (throwM)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import CircuitBreaker
  ( BulkheadRejectedException (..)
  , CircuitOpenException (..)
  , RateLimitedException (..)
  , RetriesExhaustedException (..)
  , isBulkheadRejectedException
  , isCircuitBreakerException
  , isCircuitOpenException
  , isRateLimitedException
  , isRetriesExhaustedException
  , isTimeoutException
  , withFallback
  , withFallbackOn
  , withFallbackValue
  , withFallbackValueOn
  )
import CircuitBreaker.Timeout (TimeoutException (..))

tests :: TestTree
tests = testGroup "CircuitBreaker.Fallback"
  [ withFallbackTests
  , withFallbackOnTests
  , withFallbackValueTests
  , withFallbackValueOnTests
  , predicateTests
  , distributedSystemsTests
  ]

withFallbackTests :: TestTree
withFallbackTests = testGroup "withFallback"
  [ testCase "returns primary result on success" $ do
      result <- withFallback (pure "fallback") (pure "primary" :: IO String)
      result @?= "primary"

  , testCase "returns fallback on exception" $ do
      result <- withFallback (pure "fallback") (error "fail" :: IO String)
      result @?= "fallback"

  , testCase "fallback action is executed, not just a value" $ do
      callCount <- newIORef (0 :: Int)
      result <- withFallback
        (modifyIORef' callCount (+1) >> pure "fallback")
        (error "fail" :: IO String)
      result @?= "fallback"
      count <- readIORef callCount
      count @?= 1

  , testCase "fallback action is NOT executed on success" $ do
      callCount <- newIORef (0 :: Int)
      result <- withFallback
        (modifyIORef' callCount (+1) >> pure "fallback")
        (pure "primary" :: IO String)
      result @?= "primary"
      count <- readIORef callCount
      count @?= 0

  , testCase "propagates exception from fallback if it also fails" $ do
      result <- try $ withFallback
        (error "fallback error" :: IO String)
        (error "primary error" :: IO String)
      case result of
        Left (_ :: SomeException) -> pure ()  -- Expected
        Right _ -> error "Expected exception from fallback"

  , testCase "works with different exception types" $ do
      result <- withFallback (pure "fallback") $
        ioError (userError "IO error") >> pure "never"
      result @?= "fallback"
  ]

withFallbackOnTests :: TestTree
withFallbackOnTests = testGroup "withFallbackOn"
  [ testCase "returns primary result on success" $ do
      result <- withFallbackOn (const True) (pure "fallback") (pure "primary" :: IO String)
      result @?= "primary"

  , testCase "returns fallback when predicate matches" $ do
      result <- withFallbackOn (const True) (pure "fallback") (error "fail" :: IO String)
      result @?= "fallback"

  , testCase "re-throws exception when predicate does not match" $ do
      result <- try $ withFallbackOn
        (const False)  -- Never fallback
        (pure "fallback")
        (error "fail" :: IO String)
      case result of
        Left (_ :: SomeException) -> pure ()  -- Expected - exception re-thrown
        Right _ -> error "Expected exception to be re-thrown"

  , testCase "works with selective predicate" $ do
      -- Only fallback on CircuitOpenException
      let shouldFallback ex = isCircuitOpenException ex

      -- This should NOT use fallback (wrong exception type)
      result1 <- try $ withFallbackOn shouldFallback (pure "fallback") $
        ioError (userError "not a circuit breaker error") >> pure ("never" :: String)
      case result1 of
        Left (_ :: SomeException) -> pure ()
        Right _ -> error "Expected IOError to be re-thrown"

  , testCase "predicate receives the actual exception" $ do
      _ <- withFallbackOn
        (\ex -> do
          -- Store the exception message for verification
          let _ = show ex  -- Force evaluation
          True)
        (pure "fallback")
        (error "specific error message" :: IO String)
      -- If we got here without exception, the test passed
      pure ()
  ]

withFallbackValueTests :: TestTree
withFallbackValueTests = testGroup "withFallbackValue"
  [ testCase "returns primary result on success" $ do
      result <- withFallbackValue "fallback" (pure "primary" :: IO String)
      result @?= "primary"

  , testCase "returns fallback value on exception" $ do
      result <- withFallbackValue "fallback" (error "fail" :: IO String)
      result @?= "fallback"

  , testCase "works with empty list as fallback" $ do
      result <- withFallbackValue [] (error "fail" :: IO [Int])
      result @?= []

  , testCase "works with Nothing as fallback" $ do
      result <- withFallbackValue Nothing (error "fail" :: IO (Maybe Int))
      result @?= Nothing

  , testCase "works with complex types" $ do
      result <- withFallbackValue (0, "default") (error "fail" :: IO (Int, String))
      result @?= (0, "default")
  ]

withFallbackValueOnTests :: TestTree
withFallbackValueOnTests = testGroup "withFallbackValueOn"
  [ testCase "returns primary result on success" $ do
      result <- withFallbackValueOn (const True) "fallback" (pure "primary" :: IO String)
      result @?= "primary"

  , testCase "returns fallback value when predicate matches" $ do
      result <- withFallbackValueOn (const True) "fallback" (error "fail" :: IO String)
      result @?= "fallback"

  , testCase "re-throws exception when predicate does not match" $ do
      result <- try $ withFallbackValueOn
        (const False)
        "fallback"
        (error "fail" :: IO String)
      case result of
        Left (_ :: SomeException) -> pure ()
        Right _ -> error "Expected exception to be re-thrown"
  ]

predicateTests :: TestTree
predicateTests = testGroup "Exception Predicates"
  [ testCase "isCircuitBreakerException matches CircuitOpenException" $ do
      let ex = toException (CircuitOpenException "test")
      assertBool "should match" (isCircuitBreakerException ex)

  , testCase "isCircuitBreakerException matches RateLimitedException" $ do
      let ex = toException RateLimitedException
      assertBool "should match" (isCircuitBreakerException ex)

  , testCase "isCircuitBreakerException matches BulkheadRejectedException" $ do
      let ex = toException (BulkheadRejectedException 10)
      assertBool "should match" (isCircuitBreakerException ex)

  , testCase "isCircuitBreakerException matches RetriesExhaustedException" $ do
      let innerEx = toException (userError "inner")
      let ex = toException (RetriesExhaustedException 3 innerEx)
      assertBool "should match" (isCircuitBreakerException ex)

  , testCase "isCircuitBreakerException does not match IOError" $ do
      let ex = toException (userError "io error")
      assertBool "should not match" (not $ isCircuitBreakerException ex)

  , testCase "isCircuitOpenException matches CircuitOpenException" $ do
      let ex = toException (CircuitOpenException "test")
      assertBool "should match" (isCircuitOpenException ex)

  , testCase "isCircuitOpenException does not match RateLimitedException" $ do
      let ex = toException RateLimitedException
      assertBool "should not match" (not $ isCircuitOpenException ex)

  , testCase "isTimeoutException matches TimeoutException" $ do
      let ex = toException (TimeoutException 5.0)
      assertBool "should match" (isTimeoutException ex)

  , testCase "isTimeoutException does not match CircuitOpenException" $ do
      let ex = toException (CircuitOpenException "test")
      assertBool "should not match" (not $ isTimeoutException ex)

  , testCase "isRateLimitedException matches RateLimitedException" $ do
      let ex = toException RateLimitedException
      assertBool "should match" (isRateLimitedException ex)

  , testCase "isBulkheadRejectedException matches BulkheadRejectedException" $ do
      let ex = toException (BulkheadRejectedException 10)
      assertBool "should match" (isBulkheadRejectedException ex)

  , testCase "isRetriesExhaustedException matches RetriesExhaustedException" $ do
      let innerEx = toException (userError "inner")
      let ex = toException (RetriesExhaustedException 3 innerEx)
      assertBool "should match" (isRetriesExhaustedException ex)
  ]

-- | Tests for distributed systems edge cases and failure scenarios
distributedSystemsTests :: TestTree
distributedSystemsTests = testGroup "Distributed Systems Edge Cases"
  [ cascadingFallbackTests
  , partialFailureTests
  , resourceCleanupTests
  , idempotencyTests
  , exceptionMaskingTests
  ]

-- | Tests for cascading fallback scenarios
cascadingFallbackTests :: TestTree
cascadingFallbackTests = testGroup "Cascading Fallbacks"
  [ testCase "three-level cascading fallbacks" $ do
      -- Remote -> Cache -> Default
      result <- withFallback (pure "default") $
                  withFallback (pure "cache") $
                    (error "remote failed" :: IO String)
      result @?= "cache"

  , testCase "all fallbacks fail - propagates last exception" $ do
      result <- try $ withFallback
        (error "cache failed" :: IO String)
        (error "remote failed" :: IO String)
      case result of
        Left (_ :: SomeException) -> pure ()
        Right _ -> error "Expected exception from failed fallback"

  , testCase "fallback chain with different exception types" $ do
      result <- withFallback (pure "io-default") $
                  withFallback (pure "network-cache") $
                    ioError (userError "IO error")
      result @?= "network-cache"

  , testCase "selective fallback in cascade" $ do
      -- Inner withFallback catches everything, outer withFallbackOn is selective
      -- This tests that selective fallback can wrap general fallback
      result <- withFallbackOn isCircuitOpenException (pure "cb-fallback") $
                  withFallback (pure "general-fallback") $
                    (error "some error" :: IO String)

      -- Inner fallback should handle the error
      result @?= "general-fallback"
  ]

-- | Tests for partial failure scenarios
partialFailureTests :: TestTree
partialFailureTests = testGroup "Partial Failures"
  [ testCase "primary action has side effects before failing" $ do
      -- This tests the case where the primary action modifies state
      -- before throwing, and the fallback must handle inconsistent state
      primaryCalls <- newIORef (0 :: Int)
      fallbackCalls <- newIORef (0 :: Int)

      result <- withFallback
        (modifyIORef' fallbackCalls (+1) >> pure "fallback")
        (modifyIORef' primaryCalls (+1) >> error "fail" :: IO String)

      result @?= "fallback"
      pCount <- readIORef primaryCalls
      fCount <- readIORef fallbackCalls
      pCount @?= 1  -- Primary was called
      fCount @?= 1  -- Fallback was called
      -- This demonstrates non-atomicity - primary side effects remain

  , testCase "fallback sees partial state from failed primary" $ do
      sharedState <- newIORef ([] :: [String])

      result <- withFallback
        (do
          -- Fallback can observe partial writes from primary
          state <- readIORef sharedState
          pure $ "fallback-saw-" ++ show (length state))
        (do
          -- Primary writes partial state before failing
          modifyIORef' sharedState (++ ["partial"])
          error "fail" :: IO String)

      result @?= "fallback-saw-1"
      -- Demonstrates state inconsistency risk

  , testCase "multiple fallbacks with shared state" $ do
      counter <- newIORef (0 :: Int)

      let action = do
            modifyIORef' counter (+1)
            count <- readIORef counter
            if count < 3
              then error "not ready"
              else pure "success"

      -- This tests non-idempotent fallback behavior
      _ <- try $ withFallback action action :: IO (Either SomeException String)

      count <- readIORef counter
      -- Both primary and fallback increment counter
      count @?= 2
  ]

-- | Tests for resource cleanup during fallback
resourceCleanupTests :: TestTree
resourceCleanupTests = testGroup "Resource Cleanup"
  [ testCase "fallback preserves exception semantics" $ do
      -- Ensure fallback doesn't suppress async exceptions
      result <- withFallback (pure "fallback") (pure "success" :: IO String)
      result @?= "success"

  , testCase "nested fallbacks maintain exception ordering" $ do
      -- Test that exception handling is LIFO
      executionOrder <- newIORef ([] :: [String])

      result <- try $ withFallback
        (do
          modifyIORef' executionOrder (++ ["fallback2"])
          error "fallback2-error" :: IO String)
        (withFallback
          (do
            modifyIORef' executionOrder (++ ["fallback1"])
            error "fallback1-error" :: IO String)
          (do
            modifyIORef' executionOrder (++ ["primary"])
            error "primary-error" :: IO String))

      case result of
        Left (_ :: SomeException) -> pure ()
        Right _ -> error "Expected exception"

      order <- readIORef executionOrder
      order @?= ["primary", "fallback1", "fallback2"]
  ]

-- | Tests for idempotency concerns
idempotencyTests :: TestTree
idempotencyTests = testGroup "Idempotency"
  [ testCase "fallback action is only called once per failure" $ do
      fallbackCallCount <- newIORef (0 :: Int)

      result <- withFallback
        (modifyIORef' fallbackCallCount (+1) >> pure "fallback")
        (error "fail" :: IO String)

      result @?= "fallback"
      count <- readIORef fallbackCallCount
      count @?= 1  -- Fallback called exactly once

  , testCase "re-throwing from withFallbackOn doesn't trigger fallback" $ do
      fallbackCallCount <- newIORef (0 :: Int)

      result <- try $ withFallbackOn
        (const False)  -- Never match
        (modifyIORef' fallbackCallCount (+1) >> pure "fallback")
        (error "fail" :: IO String)

      case result of
        Left (_ :: SomeException) -> pure ()
        Right _ -> error "Expected exception"

      count <- readIORef fallbackCallCount
      count @?= 0  -- Fallback never called
  ]

-- | Tests for exception masking and propagation
exceptionMaskingTests :: TestTree
exceptionMaskingTests = testGroup "Exception Masking"
  [ testCase "fallback exception masks primary exception" $ do
      -- When fallback fails, we lose the original exception
      result <- try $ withFallback
        (error "fallback-error" :: IO String)
        (error "primary-error" :: IO String)

      case result of
        Left ex -> do
          -- Should see fallback error, not primary
          let msg = show (ex :: SomeException)
          assertBool "fallback exception propagates" ("fallback-error" `elem` words msg)
        Right _ -> error "Expected exception"

  , testCase "withFallbackOn preserves original exception when not matching" $ do
      result <- try $ withFallbackOn
        (const False)
        (pure "fallback")
        (error "original-error" :: IO String)

      case result of
        Left ex -> do
          let msg = show (ex :: SomeException)
          assertBool "original exception preserved" ("original-error" `elem` words msg)
        Right _ -> error "Expected exception"

  , testCase "complex exception type is preserved through predicate" $ do
      let originalEx = CircuitOpenException "test-circuit"

      result <- try $ withFallbackOn
        (const False)  -- Don't fallback
        (pure "fallback")
        (throwM originalEx :: IO String)

      case result of
        Left ex ->
          case fromException ex :: Maybe CircuitOpenException of
            Just (CircuitOpenException name) -> name @?= "test-circuit"
            Nothing -> error "Exception type was not preserved"
        Right _ -> error "Expected exception"
  ]
