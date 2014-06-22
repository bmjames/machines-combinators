module Tests where

import Distribution.TestSuite.QuickCheck

import Data.Machine
import qualified Data.Machine.Combinators as M


tests :: IO [Test]
tests = return
  [ testProperty "any" $ anyProp f
  , testProperty "all" $ allProp f
  ]
  where
    f = (> 10)

anyProp :: (Int -> Bool) -> [Int] -> Bool
anyProp f as = run (source as ~> M.any f) == [any f as]

allProp :: (Int -> Bool) -> [Int] -> Bool
allProp f as = run (source as ~> M.all f) == [all f as]
