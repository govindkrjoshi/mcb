module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Unit.Exceptions as Exceptions
import qualified Test.Unit.Predicate as Predicate
import qualified Test.Unit.Types as Types
import qualified Test.Unit.Timeout as Timeout
import qualified Test.Unit.SlidingWindow as SlidingWindowUnit
import qualified Test.Unit.State as State
import qualified Test.Property.Placeholder as Property
import qualified Test.Property.SlidingWindow as SlidingWindowProp
import qualified Test.Property.State as StateProp
import qualified Test.Integration.WithCircuitBreaker as Integration
import qualified Test.Performance.WithCircuitBreaker as Performance

main :: IO ()
main = defaultMain $ testGroup "mcb"
  [ Types.tests
  , Timeout.tests
  , Exceptions.tests
  , Predicate.tests
  , SlidingWindowUnit.tests
  , State.tests
  , Property.tests
  , SlidingWindowProp.tests
  , StateProp.tests
  , Integration.tests
  , Performance.tests
  ]
