module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Unit.Types as Types
import qualified Test.Property.Placeholder as Property

main :: IO ()
main = defaultMain $ testGroup "mcb"
  [ Types.tests
  , Property.tests
  ]
