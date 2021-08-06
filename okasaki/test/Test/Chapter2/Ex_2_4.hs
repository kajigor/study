{-# LANGUAGE ScopedTypeVariables #-}

module Test.Chapter2.Ex_2_4 where

import Test.Tasty.HUnit (Assertion, (@=?))
import Chapter2.Ex_2_4

treeE = Empty
tree0 = Node Empty 0 Empty
tree2 = Node Empty 2 Empty
tree1 = Node tree0 1 tree2

tree4 = Node Empty 4 Empty
tree6 = Node Empty 6 Empty
tree5 = Node tree4 5 tree6

tree3 = Node tree1 3 tree5

unit_insertToTheRight :: Assertion
unit_insertToTheRight = do
  let t0 :: Tree Int = insert 0 Empty
  let t1 = insert 1 t0
  let t2 = insert 2 t1
  let t3 = insert 3 t2
  let t4 = insert 4 t3
  let t5 = insert 5 t4
  let t6 = insert 6 t5

  t6 @=? Node Empty 0 (Node Empty 1 (Node Empty 2 (Node Empty 3 (Node Empty 4 (Node Empty 5 (Node Empty 6 Empty))))))


unit_insertToTheLeft :: Assertion
unit_insertToTheLeft = do
  let t6 :: Tree Int = insert 6 Empty
  let t5 = insert 5 t6
  let t4 = insert 4 t5
  let t3 = insert 3 t4
  let t2 = insert 2 t3
  let t1 = insert 1 t2
  let t0 = insert 0 t1

  t0 @=? Node (Node (Node (Node (Node (Node (Node Empty 0 Empty) 1 Empty) 2 Empty) 3 Empty) 4 Empty) 5 Empty) 6 Empty

unit_insertBalanced :: Assertion
unit_insertBalanced = do
  let t3 = insert 0 $ insert 2 $ insert 1 $ insert 4 $ insert 6 $ insert 5 $ insert 3 Empty
  t3 @=? tree3


