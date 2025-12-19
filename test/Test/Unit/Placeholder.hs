module Test.Unit.Placeholder
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "Unit Tests"
  [ testCase "placeholder test" $ do
      (1 + 1) @?= (2 :: Int)
  ]
