module Test.Property.Placeholder
  ( tests
  ) where

import Hedgehog (property, (===), forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "Property Tests"
  [ testProperty "addition is commutative" $ property $ do
      x <- forAll $ Gen.int (Range.linear 0 1000)
      y <- forAll $ Gen.int (Range.linear 0 1000)
      x + y === y + x
  ]
