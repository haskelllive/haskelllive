-- | This is the documentation for the module Chess.
module Chess where

import Test.HUnit
import Test.QuickCheck



-- Tests

tests = TestList $ map TestCase
  [assertEqual "add tests here"  1 1
  ]

prop_empty c1 = (c1::Int) == c1

runTests = do
  runTestTT tests
  quickCheck prop_empty

-- | For now, main will run our tests.
main :: IO ()
main = runTests
