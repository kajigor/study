module Chapter2.Ex_2_3 where

import Data.Maybe (fromMaybe)

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Ord, Show)

insert :: Ord a => a -> Tree a -> Tree a
insert elem tree =
    fromMaybe tree (go elem tree)
  where
    go :: Ord a => a -> Tree a -> Maybe (Tree a)
    go elem Empty = Just $ Node Empty elem Empty
    go elem (Node l x r)
      | elem < x = go elem l >>= \l -> Just $ Node l x r
      | elem > x = go elem r >>= \r -> Just $ Node l x r
      | otherwise = Nothing
