module Main where

import Test.HUnit
import System.Exit
import BioInf.GenussFold

testHairpin1 = TestCase(assertEqual "Hairpin 1" ("-3 (((...)))") (viennaRNA 1 "AAACCCUUU") )
testHairpin2 = TestCase(assertEqual "Hairpin 1" ("-3 (((...)))") (viennaRNA 1 "AAACCCUUU") )

main :: IO ()
main = do
  counts <- runTestTT ( test [testHairpin1, testHairpin2 ])
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
