module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Unit.Types as Types
import qualified Test.Unit.Timeout as Timeout
import qualified Test.Property.Placeholder as Property
import qualified Test.Property.SlidingWindow as SlidingWindowProp

main :: IO ()
main = defaultMain $ testGroup "mcb"
  [ Types.tests
  , Timeout.tests
  , Property.tests
  , SlidingWindowProp.tests
  ]
