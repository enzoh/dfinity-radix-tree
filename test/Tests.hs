{-# OPTIONS -Wall #-}

import Test.Tasty

import Integrations as Integration
import Properties   as Property
import Units        as Unit

main :: IO ()
main = do
  units <- Unit.tests
  defaultMain $ testGroup "tests" [Integration.tests, Property.tests, units]
