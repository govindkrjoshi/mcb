module Test.Unit.Exceptions
  ( tests
  ) where

import Control.Exception
    ( Exception(..)
    , SomeException
    , catch
    , throwIO
    )
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

import CircuitBreaker.Exceptions
    ( CircuitBreakerException(..)
    , CircuitOpenException(..)
    )
import CircuitBreaker.Timeout (TimeoutException(..))

tests :: TestTree
tests = testGroup "CircuitBreaker.Exceptions"
  [ circuitOpenExceptionTests
  , circuitBreakerExceptionHierarchyTests
  , timeoutExceptionHierarchyTests
  ]

circuitOpenExceptionTests :: TestTree
circuitOpenExceptionTests = testGroup "CircuitOpenException"
  [ testCase "has correct Show instance" $
      show (CircuitOpenException "test-cb") @?= "CircuitOpenException \"test-cb\""

  , testCase "has correct displayException message format" $ do
      let ex = CircuitOpenException "payment-service"
      displayException ex @?= "Circuit breaker 'payment-service' is OPEN"

  , testCase "stores circuit breaker name correctly" $
      circuitBreakerName (CircuitOpenException "my-service") @?= "my-service"

  , testCase "Eq instance works" $ do
      CircuitOpenException "a" @?= CircuitOpenException "a"
      assertBool "different names not equal"
        (CircuitOpenException "a" /= CircuitOpenException "b")

  , testCase "can be thrown and caught specifically" $ do
      result <- (do
          _ <- throwIO (CircuitOpenException "test-cb")
          pure Nothing
        ) `catch` \(CircuitOpenException name) -> pure (Just name)
      result @?= Just "test-cb"

  , testCase "can be caught as SomeException" $ do
      result <- (do
          _ <- throwIO (CircuitOpenException "test-cb")
          pure Nothing
        ) `catch` \(e :: SomeException) -> pure (Just (displayException e))
      case result of
        Just msg -> assertBool "message contains circuit name" ("test-cb" `isInfixOf` msg)
        Nothing -> assertFailure "Expected exception to be caught"
  ]

circuitBreakerExceptionHierarchyTests :: TestTree
circuitBreakerExceptionHierarchyTests = testGroup "CircuitBreakerException hierarchy"
  [ testCase "CircuitOpenException can be caught as CircuitBreakerException" $ do
      result <- (do
          _ <- throwIO (CircuitOpenException "test-cb")
          pure Nothing
        ) `catch` \(CircuitBreakerException e) -> pure (Just (displayException e))
      case result of
        Just msg -> assertBool "message contains circuit name" ("test-cb" `isInfixOf` msg)
        Nothing -> assertFailure "Expected exception to be caught"

  , testCase "CircuitBreakerException Show delegates to wrapped exception" $ do
      let cbe = CircuitBreakerException (CircuitOpenException "my-cb")
      assertBool "show contains inner exception info" ("my-cb" `isInfixOf` show cbe)

  , testCase "CircuitBreakerException displayException delegates to wrapped" $ do
      let cbe = CircuitBreakerException (CircuitOpenException "my-cb")
      let msg = displayException cbe
      assertBool "displayException contains circuit name" ("my-cb" `isInfixOf` msg)

  , testCase "catch hierarchy order - specific before general" $ do
      -- When catching CircuitOpenException specifically, it should match first
      result <- (do
          _ <- throwIO (CircuitOpenException "test-cb")
          pure "not caught"
        ) `catch` \(CircuitOpenException name) -> pure ("specific: " ++ name)
          `catch` \(CircuitBreakerException _) -> pure "general"
      result @?= "specific: test-cb"

  , testCase "catch general when specific doesn't match" $ do
      -- TimeoutException should be caught by CircuitBreakerException handler
      result <- (do
          _ <- throwIO (TimeoutException 5)
          pure "not caught"
        ) `catch` \(CircuitBreakerException e) -> pure ("general: " ++ take 20 (displayException e))
      assertBool "caught by general handler" ("general:" `isInfixOf` result)
  ]

timeoutExceptionHierarchyTests :: TestTree
timeoutExceptionHierarchyTests = testGroup "TimeoutException in hierarchy"
  [ testCase "TimeoutException can be caught specifically" $ do
      result <- (do
          _ <- throwIO (TimeoutException 5)
          pure Nothing
        ) `catch` \(TimeoutException d) -> pure (Just d)
      result @?= Just 5

  , testCase "TimeoutException can be caught as CircuitBreakerException" $ do
      result <- (do
          _ <- throwIO (TimeoutException 10)
          pure Nothing
        ) `catch` \(CircuitBreakerException e) -> pure (Just (displayException e))
      case result of
        Just msg -> assertBool "message mentions timeout" ("timed out" `isInfixOf` msg)
        Nothing -> assertFailure "Expected exception to be caught"

  , testCase "TimeoutException can be caught as SomeException" $ do
      result <- (do
          _ <- throwIO (TimeoutException 3)
          pure Nothing
        ) `catch` \(e :: SomeException) -> pure (Just (displayException e))
      case result of
        Just msg -> assertBool "message mentions timeout" ("timed out" `isInfixOf` msg)
        Nothing -> assertFailure "Expected exception to be caught"
  ]
