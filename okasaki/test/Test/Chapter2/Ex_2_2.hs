module Test.Chapter2.Ex_2_2 where

import Test.Tasty.HUnit (Assertion, (@=?))
import Chapter2.Ex_2_2

treeE = Empty
tree0 = Node Empty 0 Empty
tree2 = Node Empty 2 Empty
tree1 = Node tree0 1 tree2

tree4 = Node Empty 4 Empty
tree6 = Node Empty 6 Empty
tree5 = Node tree4 5 tree6

tree3 = Node tree1 3 tree5

unit_member :: Assertion
unit_member = do
  member 0 treeE @=? False
  member 1 treeE @=? False
  member 2 treeE @=? False
  member 3 treeE @=? False
  member 0 tree0 @=? True
  member 1 tree0 @=? False
  member 2 tree0 @=? False
  member 3 tree0 @=? False
  member 0 tree2 @=? False
  member 1 tree2 @=? False
  member 2 tree2 @=? True
  member 3 tree2 @=? False
  member 0 tree1 @=? True
  member 1 tree1 @=? True
  member 2 tree1 @=? True
  member 3 tree1 @=? False
  member 0 tree3 @=? True
  member 1 tree3 @=? True
  member 2 tree3 @=? True
  member 3 tree3 @=? True
  member 4 tree3 @=? True
  member 5 tree3 @=? True
  member 6 tree3 @=? True
  member 7 tree3 @=? False