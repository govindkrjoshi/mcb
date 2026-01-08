module Test.Unit.SlidingWindow
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import CircuitBreaker.SlidingWindow

tests :: TestTree
tests = testGroup "CircuitBreaker.SlidingWindow"
  [ emptyWindowTests
  , fullSuccessTests
  , fullFailureTests
  , mixedResultsTests
  , evictionTests
  , resetTests
  , boundaryTests
  ]

-- | Tests for empty window behavior
emptyWindowTests :: TestTree
emptyWindowTests = testGroup "Empty Window"
  [ testCase "empty window has zero failure rate" $
      getFailureRate (newSlidingWindow 10) @?= 0.0
  , testCase "empty window has zero total calls" $
      getTotalCalls (newSlidingWindow 10) @?= 0
  , testCase "empty window has zero failure count" $
      getFailureCount (newSlidingWindow 10) @?= 0
  , testCase "empty window isEmpty is True" $
      isEmpty (newSlidingWindow 10) @?= True
  , testCase "empty window isFull is False" $
      isFull (newSlidingWindow 10) @?= False
  , testCase "empty window preserves size" $
      getWindowSize (newSlidingWindow 10) @?= 10
  ]

-- | Tests for window full of successes
fullSuccessTests :: TestTree
fullSuccessTests = testGroup "Full Successes (0.0 rate)"
  [ testCase "all successes has 0.0 failure rate" $
      let w = insertMany [True, True, True, True, True] (newSlidingWindow 5)
      in getFailureRate w @?= 0.0
  , testCase "all successes has zero failure count" $
      let w = insertMany [True, True, True, True, True] (newSlidingWindow 5)
      in getFailureCount w @?= 0
  , testCase "all successes is full" $
      let w = insertMany [True, True, True, True, True] (newSlidingWindow 5)
      in isFull w @?= True
  , testCase "all successes is not empty" $
      let w = insertMany [True, True, True, True, True] (newSlidingWindow 5)
      in isEmpty w @?= False
  ]

-- | Tests for window full of failures
fullFailureTests :: TestTree
fullFailureTests = testGroup "Full Failures (1.0 rate)"
  [ testCase "all failures has 1.0 failure rate" $
      let w = insertMany [False, False, False, False, False] (newSlidingWindow 5)
      in getFailureRate w @?= 1.0
  , testCase "all failures has correct failure count" $
      let w = insertMany [False, False, False, False, False] (newSlidingWindow 5)
      in getFailureCount w @?= 5
  , testCase "all failures is full" $
      let w = insertMany [False, False, False, False, False] (newSlidingWindow 5)
      in isFull w @?= True
  , testCase "all failures total calls equals window size" $
      let w = insertMany [False, False, False, False, False] (newSlidingWindow 5)
      in getTotalCalls w @?= 5
  ]

-- | Tests for mixed success/failure results
mixedResultsTests :: TestTree
mixedResultsTests = testGroup "Mixed Results"
  [ testCase "50% failure rate" $
      let w = insertMany [True, False, True, False] (newSlidingWindow 4)
      in getFailureRate w @?= 0.5
  , testCase "25% failure rate" $
      let w = insertMany [True, True, True, False] (newSlidingWindow 4)
      in getFailureRate w @?= 0.25
  , testCase "75% failure rate" $
      let w = insertMany [True, False, False, False] (newSlidingWindow 4)
      in getFailureRate w @?= 0.75
  , testCase "one failure in ten is 10%" $
      let w = insertMany [True, True, True, True, True, True, True, True, True, False]
                        (newSlidingWindow 10)
      in getFailureRate w @?= 0.1
  , testCase "failure count matches failures inserted" $
      let w = insertMany [True, False, True, False, False] (newSlidingWindow 5)
      in getFailureCount w @?= 3
  ]

-- | Tests for FIFO eviction behavior
evictionTests :: TestTree
evictionTests = testGroup "FIFO Eviction"
  [ testCase "evicting success keeps failure count correct" $ do
      -- Window size 3: insert T, F, T, then insert F
      -- Before: [T, F, T] (1 failure)
      -- After:  [F, T, F] (2 failures) -- evicted T, added F
      let w1 = insertMany [True, False, True] (newSlidingWindow 3)
          w2 = insertResult False w1
      getFailureCount w1 @?= 1
      getFailureCount w2 @?= 2
      getFailureRate w2 @?= (2.0 / 3.0)

  , testCase "evicting failure decreases failure count" $ do
      -- Window size 3: insert F, T, T, then insert T
      -- Before: [F, T, T] (1 failure)
      -- After:  [T, T, T] (0 failures) -- evicted F, added T
      let w1 = insertMany [False, True, True] (newSlidingWindow 3)
          w2 = insertResult True w1
      getFailureCount w1 @?= 1
      getFailureCount w2 @?= 0
      getFailureRate w2 @?= 0.0

  , testCase "continuous eviction maintains correct count" $ do
      -- Window size 2, insert: F, F, T, T, F
      -- Step by step:
      -- [F]     -> 1 failure
      -- [F, F]  -> 2 failures (full)
      -- [F, T]  -> 1 failure (evicted F)
      -- [T, T]  -> 0 failures (evicted F)
      -- [T, F]  -> 1 failure (evicted T)
      let w = insertMany [False, False, True, True, False] (newSlidingWindow 2)
      getFailureCount w @?= 1
      getFailureRate w @?= 0.5

  , testCase "eviction preserves window size" $ do
      let w = insertMany (replicate 100 True) (newSlidingWindow 10)
      getTotalCalls w @?= 10
      getWindowSize w @?= 10

  , testCase "many evictions maintain integrity" $ do
      -- Insert 50 failures then 50 successes into window of 20
      -- Final window should have 20 successes
      let w = insertMany (replicate 50 False ++ replicate 50 True) (newSlidingWindow 20)
      getTotalCalls w @?= 20
      getFailureCount w @?= 0
      getFailureRate w @?= 0.0
  ]

-- | Tests for reset behavior
resetTests :: TestTree
resetTests = testGroup "Reset"
  [ testCase "reset clears total calls" $ do
      let w = insertMany [True, False, True] (newSlidingWindow 10)
          r = reset w
      getTotalCalls r @?= 0

  , testCase "reset clears failure count" $ do
      let w = insertMany [False, False, False] (newSlidingWindow 10)
          r = reset w
      getFailureCount r @?= 0

  , testCase "reset sets failure rate to 0" $ do
      let w = insertMany [False, False, False] (newSlidingWindow 10)
          r = reset w
      getFailureRate r @?= 0.0

  , testCase "reset makes isEmpty True" $ do
      let w = insertMany [True, False] (newSlidingWindow 10)
          r = reset w
      isEmpty r @?= True

  , testCase "reset preserves window size" $ do
      let w = insertMany [True, False] (newSlidingWindow 42)
          r = reset w
      getWindowSize r @?= 42

  , testCase "can insert after reset" $ do
      let w = insertMany [False, False] (newSlidingWindow 5)
          r = reset w
          w' = insertResult True r
      getTotalCalls w' @?= 1
      getFailureCount w' @?= 0
  ]

-- | Tests for boundary conditions
boundaryTests :: TestTree
boundaryTests = testGroup "Boundary Conditions"
  [ testCase "window size 1 works correctly" $ do
      let w = newSlidingWindow 1
      getWindowSize w @?= 1
      isEmpty w @?= True

      let w1 = insertResult True w
      isFull w1 @?= True
      getFailureRate w1 @?= 0.0

      let w2 = insertResult False w1
      getFailureRate w2 @?= 1.0
      getTotalCalls w2 @?= 1

  , testCase "window size 1 evicts immediately" $ do
      let w = insertMany [True, False, True] (newSlidingWindow 1)
      getTotalCalls w @?= 1
      -- Last inserted was True
      getFailureRate w @?= 0.0

  , testCase "single result partial fill" $ do
      let w = insertResult False (newSlidingWindow 100)
      getTotalCalls w @?= 1
      getFailureRate w @?= 1.0
      isFull w @?= False
      isEmpty w @?= False

  , testCase "non-positive size is clamped to 1" $ do
      let w0 = newSlidingWindow 0
          wNeg = newSlidingWindow (-5)
      getWindowSize w0 @?= 1
      getWindowSize wNeg @?= 1

  , testCase "large window partially filled" $ do
      let w = insertMany [True, False] (newSlidingWindow 1000)
      getTotalCalls w @?= 2
      getFailureCount w @?= 1
      getFailureRate w @?= 0.5
      isFull w @?= False

  , testCase "alternating pattern eviction" $ do
      -- Window size 4, insert alternating T, F, T, F, T, F, T, F
      -- Final: F, T, F, T (in order of insertion)
      -- Failures: 2
      let w = insertMany [True, False, True, False, True, False, True, False]
                        (newSlidingWindow 4)
      getTotalCalls w @?= 4
      getFailureCount w @?= 2
      getFailureRate w @?= 0.5
  ]

-- | Helper to insert multiple results
insertMany :: [Bool] -> SlidingWindow -> SlidingWindow
insertMany results w = foldl (flip insertResult) w results
