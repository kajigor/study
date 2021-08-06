module Test.Chapter2.Ex_2_1 where

import Test.Tasty.HUnit (Assertion, (@=?))
import Chapter2.Ex_2_1

unit_suffixes :: Assertion
unit_suffixes = do
  suffixes [] @=? ([[]] :: [[Int]])
  suffixes [1,2,3,4] @=? [[1,2,3,4], [2,3,4], [3,4], [4], []]
