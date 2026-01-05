module Test.Unit.Timeout
  ( tests
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO)
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

import CircuitBreaker.Timeout

tests :: TestTree
tests = testGroup "CircuitBreaker.Timeout"
  [ timeoutExceptionTests
  , withTimeoutTests
  , durationHelperTests
  ]

timeoutExceptionTests :: TestTree
timeoutExceptionTests = testGroup "TimeoutException"
  [ testCase "has correct Show instance" $
      show (TimeoutException 5) @?= "TimeoutException 5s"
  , testCase "has correct displayException" $ do
      let ex = TimeoutException 5
      -- displayException returns a String via the Exception class
      let msg = show ex
      assertBool "show contains duration" ("5s" `elem` words msg || "5" `elem` words msg)
  , testCase "stores duration correctly" $
      timeoutDuration (TimeoutException 10) @?= 10
  , testCase "Eq instance works" $ do
      TimeoutException 5 @?= TimeoutException 5
      assertBool "different durations not equal" (TimeoutException 5 /= TimeoutException 10)
  ]

withTimeoutTests :: TestTree
withTimeoutTests = testGroup "withTimeout"
  [ testCase "action completing before timeout returns result" $ do
      result <- withTimeout (milliseconds 100) $ do
        threadDelay 10000  -- 10ms
        pure "success"
      result @?= "success"

  , testCase "action exceeding timeout throws TimeoutException" $ do
      result <- (do
          _ <- withTimeout (milliseconds 50) $ do
            threadDelay 200000  -- 200ms
            pure "should not reach"
          pure Nothing
        ) `catch` \(TimeoutException d) -> pure (Just d)
      case result of
        Just d -> assertBool "timeout duration is captured" (d > 0)
        Nothing -> assertFailure "Expected TimeoutException to be thrown"

  , testCase "very short timeout (1ms) times out slow action" $ do
      result <- (do
          _ <- withTimeout (milliseconds 1) $ do
            threadDelay 100000  -- 100ms
            pure "should not reach"
          pure False
        ) `catch` \(TimeoutException _) -> pure True
      result @?= True

  , testCase "nested timeouts - inner timeout fires first" $ do
      result <- withTimeout (milliseconds 500) $ do
        (do
            _ <- withTimeout (milliseconds 10) $ do
              threadDelay 100000  -- 100ms
              pure "inner should not reach"
            pure "outer should not reach"
          ) `catch` \(TimeoutException _) -> pure "inner caught"
      result @?= "inner caught"

  , testCase "nested timeouts - outer timeout fires first" $ do
      result <- (do
          _ <- withTimeout (milliseconds 10) $ do
            -- Inner timeout is longer than outer
            withTimeout (milliseconds 500) $ do
              threadDelay 100000  -- 100ms
              pure "should not reach"
          pure Nothing
        ) `catch` \(TimeoutException _) -> pure (Just "outer caught")
      result @?= Just "outer caught"

  , testCase "exception from action propagates" $ do
      let customError = userError "custom error"
      result <- (do
          _ <- withTimeout (milliseconds 100) $ throwIO customError
          pure "should not reach"
        ) `catch` \e -> pure (show (e :: IOError))
      assertBool "custom error propagated" ("custom" `isInfixOf` result && "error" `isInfixOf` result)

  , testCase "timeout returns correct value type" $ do
      intResult <- withTimeout (seconds 1) $ pure (42 :: Int)
      intResult @?= 42
      stringResult <- withTimeout (seconds 1) $ pure "hello"
      stringResult @?= "hello"
      listResult <- withTimeout (seconds 1) $ pure [1,2,3 :: Int]
      listResult @?= [1,2,3]
  ]

durationHelperTests :: TestTree
durationHelperTests = testGroup "Duration Helpers"
  [ testCase "seconds converts correctly" $ do
      seconds 1 @?= 1
      seconds 5 @?= 5
      seconds 0.5 @?= 0.5

  , testCase "milliseconds converts correctly" $ do
      milliseconds 1000 @?= 1
      milliseconds 500 @?= 0.5
      milliseconds 100 @?= 0.1

  , testCase "minutes converts correctly" $ do
      minutes 1 @?= 60
      minutes 2 @?= 120
      minutes 0.5 @?= 30

  , testCase "helpers compose correctly" $ do
      -- 1 second + 500ms = 1.5 seconds
      let duration = seconds 1 + milliseconds 500
      duration @?= 1.5

  , testCase "helpers work with withTimeout" $ do
      -- Just verify they compile and work together
      result <- withTimeout (seconds 1) $ pure "ok"
      result @?= "ok"
      result2 <- withTimeout (milliseconds 100) $ pure "ok"
      result2 @?= "ok"
      result3 <- withTimeout (minutes 1) $ pure "ok"  -- won't actually wait
      result3 @?= "ok"
  ]
