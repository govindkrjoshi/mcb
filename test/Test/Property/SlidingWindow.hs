module Test.Property.SlidingWindow
  ( tests
  ) where

import Hedgehog (Property, Gen, property, (===), forAll, assert, annotate, annotateShow, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import CircuitBreaker.SlidingWindow

tests :: TestTree
tests = testGroup "CircuitBreaker.SlidingWindow Properties"
  [ testProperty "failure rate matches actual count (10k tests)" $
      withTests 10000 prop_failureRateMatchesActualCount
  , testProperty "window never exceeds size (10k tests)" $
      withTests 10000 prop_windowNeverExceedsSize
  , testProperty "FIFO ordering maintained (10k tests)" $
      withTests 10000 prop_fifoOrderingMaintained
  , testProperty "failure count is always non-negative" $
      withTests 10000 prop_failureCountNonNegative
  , testProperty "failure rate is between 0 and 1" $
      withTests 10000 prop_failureRateInRange
  , testProperty "reset clears all results" $
      prop_resetClearsAll
  , testProperty "empty window has zero failure rate" $
      prop_emptyWindowZeroRate
  , testProperty "total calls matches expected" $
      withTests 10000 prop_totalCallsMatchesExpected
  , testProperty "isFull agrees with total calls" $
      prop_isFullConsistent
  , testProperty "isEmpty agrees with total calls" $
      prop_isEmptyConsistent
  ]

-- | Generate a sliding window with random operations applied
genSlidingWindow :: Gen (SlidingWindow, [Bool])
genSlidingWindow = do
  size <- Gen.int (Range.linear 1 100)
  numOps <- Gen.int (Range.linear 0 200)
  results <- Gen.list (Range.singleton numOps) Gen.bool
  let window = foldl' (flip insertResult) (newSlidingWindow size) results
  pure (window, results)

-- | Property: The failure rate computed from cached count matches
-- the actual ratio of failures in the window.
prop_failureRateMatchesActualCount :: Property
prop_failureRateMatchesActualCount = property $ do
  (window, results) <- forAll genSlidingWindow
  let size = getWindowSize window
      -- Take only the last 'size' results (what should be in the window)
      windowContents = takeLast size results
      actualFailures = length $ filter not windowContents
      total = length windowContents
      expectedRate =
        if total == 0
          then 0.0
          else fromIntegral actualFailures / fromIntegral total

  annotate $ "Window size: " ++ show size
  annotate $ "Total calls: " ++ show total
  annotate $ "Actual failures: " ++ show actualFailures
  annotate $ "Expected rate: " ++ show expectedRate
  annotate $ "Computed rate: " ++ show (getFailureRate window)

  -- Allow for floating point imprecision
  assert $ abs (getFailureRate window - expectedRate) < 1e-10

-- | Property: The window never contains more results than its size
prop_windowNeverExceedsSize :: Property
prop_windowNeverExceedsSize = property $ do
  (window, _) <- forAll genSlidingWindow
  let size = getWindowSize window
      total = getTotalCalls window
  annotateShow size
  annotateShow total
  assert $ total <= size

-- | Property: FIFO ordering is maintained - oldest elements are evicted first
-- We verify this by checking that if we insert windowSize + n elements,
-- the first n elements are gone and the remaining match.
prop_fifoOrderingMaintained :: Property
prop_fifoOrderingMaintained = property $ do
  size <- forAll $ Gen.int (Range.linear 1 50)
  results <- forAll $ Gen.list (Range.linear 1 100) Gen.bool

  let window = foldl' (flip insertResult) (newSlidingWindow size) results
      expectedContents = takeLast size results
      -- Compute actual failure count from expected contents
      expectedFailures = length $ filter not expectedContents

  annotate $ "Window size: " ++ show size
  annotate $ "Input results: " ++ show results
  annotate $ "Expected contents: " ++ show expectedContents

  -- The failure count should match what we expect from FIFO eviction
  getFailureCount window === expectedFailures
  getTotalCalls window === length expectedContents

-- | Property: Failure count is always non-negative
prop_failureCountNonNegative :: Property
prop_failureCountNonNegative = property $ do
  (window, _) <- forAll genSlidingWindow
  assert $ getFailureCount window >= 0

-- | Property: Failure rate is always between 0 and 1
prop_failureRateInRange :: Property
prop_failureRateInRange = property $ do
  (window, _) <- forAll genSlidingWindow
  let rate = getFailureRate window
  annotateShow rate
  assert $ rate >= 0.0
  assert $ rate <= 1.0

-- | Property: Reset clears all results and sets failure count to 0
prop_resetClearsAll :: Property
prop_resetClearsAll = property $ do
  (window, _) <- forAll genSlidingWindow
  let resetWindow = reset window
  getTotalCalls resetWindow === 0
  getFailureCount resetWindow === 0
  getFailureRate resetWindow === 0.0
  isEmpty resetWindow === True
  -- Window size should be preserved
  getWindowSize resetWindow === getWindowSize window

-- | Property: Empty window has zero failure rate
prop_emptyWindowZeroRate :: Property
prop_emptyWindowZeroRate = property $ do
  size <- forAll $ Gen.int (Range.linear 1 100)
  let window = newSlidingWindow size
  getFailureRate window === 0.0
  isEmpty window === True
  getTotalCalls window === 0
  getFailureCount window === 0

-- | Property: getTotalCalls matches the expected count
prop_totalCallsMatchesExpected :: Property
prop_totalCallsMatchesExpected = property $ do
  (window, results) <- forAll genSlidingWindow
  let size = getWindowSize window
      expectedTotal = min size (length results)
  getTotalCalls window === expectedTotal

-- | Property: isFull is consistent with total calls vs window size
prop_isFullConsistent :: Property
prop_isFullConsistent = property $ do
  (window, _) <- forAll genSlidingWindow
  isFull window === (getTotalCalls window >= getWindowSize window)

-- | Property: isEmpty is consistent with total calls
prop_isEmptyConsistent :: Property
prop_isEmptyConsistent = property $ do
  (window, _) <- forAll genSlidingWindow
  isEmpty window === (getTotalCalls window == 0)

-- | Helper: take the last n elements from a list
takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs
