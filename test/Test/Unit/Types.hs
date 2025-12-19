module Test.Unit.Types
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import CircuitBreaker.Types

tests :: TestTree
tests = testGroup "CircuitBreaker.Types"
  [ stateTests
  , failureThresholdTests
  , slidingWindowSizeTests
  , waitDurationTests
  , halfOpenPermitsTests
  , defaultConfigTests
  , builderTests
  ]

stateTests :: TestTree
stateTests = testGroup "State"
  [ testCase "Closed, Open, HalfOpen are distinct" $ do
      assertBool "Closed /= Open" (Closed /= Open)
      assertBool "Open /= HalfOpen" (Open /= HalfOpen)
      assertBool "Closed /= HalfOpen" (Closed /= HalfOpen)
  , testCase "State has Show instance" $ do
      show Closed @?= "Closed"
      show Open @?= "Open"
      show HalfOpen @?= "HalfOpen"
  ]

failureThresholdTests :: TestTree
failureThresholdTests = testGroup "FailureThreshold"
  [ testCase "valid threshold 0.5" $
      isRight (mkFailureThreshold 0.5) @?= True
  , testCase "valid threshold 1.0" $
      isRight (mkFailureThreshold 1.0) @?= True
  , testCase "valid threshold 0.01" $
      isRight (mkFailureThreshold 0.01) @?= True
  , testCase "invalid threshold 0" $
      isLeft (mkFailureThreshold 0) @?= True
  , testCase "invalid threshold -0.5" $
      isLeft (mkFailureThreshold (-0.5)) @?= True
  , testCase "invalid threshold 1.5" $
      isLeft (mkFailureThreshold 1.5) @?= True
  , testCase "unwrap valid threshold" $ do
      let Right ft = mkFailureThreshold 0.5
      unFailureThreshold ft @?= 0.5
  ]

slidingWindowSizeTests :: TestTree
slidingWindowSizeTests = testGroup "SlidingWindowSize"
  [ testCase "valid size 10" $
      isRight (mkSlidingWindowSize 10) @?= True
  , testCase "valid size 1" $
      isRight (mkSlidingWindowSize 1) @?= True
  , testCase "invalid size 0" $
      isLeft (mkSlidingWindowSize 0) @?= True
  , testCase "invalid size -1" $
      isLeft (mkSlidingWindowSize (-1)) @?= True
  , testCase "unwrap valid size" $ do
      let Right sw = mkSlidingWindowSize 10
      unSlidingWindowSize sw @?= 10
  ]

waitDurationTests :: TestTree
waitDurationTests = testGroup "WaitDuration"
  [ testCase "valid duration 30" $
      isRight (mkWaitDuration 30) @?= True
  , testCase "valid duration 0" $
      isRight (mkWaitDuration 0) @?= True
  , testCase "invalid duration -1" $
      isLeft (mkWaitDuration (-1)) @?= True
  , testCase "unwrap valid duration" $ do
      let Right wd = mkWaitDuration 30
      unWaitDuration wd @?= 30
  ]

halfOpenPermitsTests :: TestTree
halfOpenPermitsTests = testGroup "HalfOpenPermits"
  [ testCase "valid permits 3" $
      isRight (mkHalfOpenPermits 3) @?= True
  , testCase "valid permits 1" $
      isRight (mkHalfOpenPermits 1) @?= True
  , testCase "invalid permits 0" $
      isLeft (mkHalfOpenPermits 0) @?= True
  , testCase "invalid permits -1" $
      isLeft (mkHalfOpenPermits (-1)) @?= True
  , testCase "unwrap valid permits" $ do
      let Right hp = mkHalfOpenPermits 3
      unHalfOpenPermits hp @?= 3
  ]

defaultConfigTests :: TestTree
defaultConfigTests = testGroup "defaultConfig"
  [ testCase "has valid failure threshold" $
      unFailureThreshold (failureThreshold defaultConfig) @?= 0.5
  , testCase "has valid sliding window size" $
      unSlidingWindowSize (slidingWindowSize defaultConfig) @?= 10
  , testCase "has valid wait duration" $
      unWaitDuration (waitDuration defaultConfig) @?= 30
  , testCase "has valid half-open permits" $
      unHalfOpenPermits (halfOpenPermits defaultConfig) @?= 3
  ]

builderTests :: TestTree
builderTests = testGroup "Builder Pattern"
  [ testCase "setFailureThreshold updates config" $ do
      let cfg = defaultConfig & setFailureThreshold 0.7
      unFailureThreshold (failureThreshold cfg) @?= 0.7
  , testCase "setSlidingWindowSize updates config" $ do
      let cfg = defaultConfig & setSlidingWindowSize 20
      unSlidingWindowSize (slidingWindowSize cfg) @?= 20
  , testCase "setWaitDuration updates config" $ do
      let cfg = defaultConfig & setWaitDuration 60
      unWaitDuration (waitDuration cfg) @?= 60
  , testCase "setHalfOpenPermits updates config" $ do
      let cfg = defaultConfig & setHalfOpenPermits 5
      unHalfOpenPermits (halfOpenPermits cfg) @?= 5
  , testCase "chained builders work" $ do
      let cfg = defaultConfig
            & setFailureThreshold 0.8
            & setSlidingWindowSize 15
            & setWaitDuration 45
            & setHalfOpenPermits 2
      unFailureThreshold (failureThreshold cfg) @?= 0.8
      unSlidingWindowSize (slidingWindowSize cfg) @?= 15
      unWaitDuration (waitDuration cfg) @?= 45
      unHalfOpenPermits (halfOpenPermits cfg) @?= 2
  ]

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
