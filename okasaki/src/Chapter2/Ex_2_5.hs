module Chapter2.Ex_2_5 where

import Data.Maybe (fromMaybe)

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Ord, Show)

complete :: a -> Int -> Tree a
complete _ 0 =
  Empty
complete elem d =
  let subtree = complete elem (d - 1) in
  Node subtree elem subtree

balanced :: a -> Int -> Tree a
balanced elem size =
    fst $ go elem size
  where
    go elem 0 = (Empty, Node Empty elem Empty)
    go elem size
      | odd size =
        let (subtree, _) = go elem (size `div` 2) in
        let res = Node subtree elem subtree in
        (res, res)
      | otherwise =
        let (l, r) = go elem (size `div` 2 - 1) in
        (Node l elem r, Node r elem r)

