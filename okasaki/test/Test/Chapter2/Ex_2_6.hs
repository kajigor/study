module Test.Chapter2.Ex_2_6 where

import Prelude hiding (lookup, empty)
import Test.Tasty.HUnit (Assertion, (@=?))
import Chapter2.Ex_2_6

import Debug.Trace

unit_map = do
  Nothing @=? (lookup 1 (empty :: Map Int String))
  Just "value" @=? (lookup 1 (bind 1 "value" empty :: Map Int String))
  Just "new"   @=? (lookup 1 (bind 1 "new" $ bind 1 "old" empty :: Map Int String))
  Just "old"   @=? (lookup 1 (bind 2 "new" $ bind 1 "old" empty :: Map Int String))
  Just "new"   @=? (lookup 2 (bind 2 "new" $ bind 1 "old" empty :: Map Int String))