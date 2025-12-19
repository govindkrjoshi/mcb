module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Unit.Placeholder as Unit
import qualified Test.Property.Placeholder as Property

main :: IO ()
main = defaultMain $ testGroup "mcb"
  [ Unit.tests
  , Property.tests
  ]
