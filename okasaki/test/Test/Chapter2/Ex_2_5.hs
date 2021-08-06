{-# LANGUAGE ScopedTypeVariables #-}

module Test.Chapter2.Ex_2_5 where

import Test.Tasty.HUnit (Assertion, (@=?), assertBool)
import Chapter2.Ex_2_5

tree0 = Empty
tree1 = Node tree0 0 tree0
tree3 = Node tree1 0 tree1
tree7 = Node tree3 0 tree3
tree15 = Node tree7 0 tree7

tree2 = Node tree0 0 tree1
tree5 = Node tree2 0 tree2
tree6 = Node tree2 0 tree3

isBalanced Empty = True
isBalanced (Node l _ r) =
    abs (size l - size r) <= 1 && isBalanced l && isBalanced r
  where
    size Empty = 0
    size (Node l _ r) = 1 + size l + size r

unit_complete = do
  tree0  @=? complete 0 0
  tree1  @=? complete 0 1
  tree3  @=? complete 0 2
  tree7  @=? complete 0 3
  tree15 @=? complete 0 4

unit_balanced_powersOf2 = do
  tree0  @=? balanced 0 0
  tree1  @=? balanced 0 1
  tree3  @=? balanced 0 3
  tree7  @=? balanced 0 7
  tree15 @=? balanced 0 15

checkBalance size =
  let tree = balanced 0 size in
  assertBool ("Not balanced: " ++ show tree) $ isBalanced tree

unit_balanced_incomplete = do
  checkBalance 0
  checkBalance 1
  checkBalance 2
  checkBalance 3
  checkBalance 4
  checkBalance 5
  checkBalance 6
  checkBalance 7
  checkBalance 8
  checkBalance 9
  checkBalance 10
  checkBalance 11
  checkBalance 12
  checkBalance 13
  checkBalance 14
  checkBalance 15

  tree2 @=? balanced 0 2
  tree5 @=? balanced 0 5
  tree6 @=? balanced 0 6
