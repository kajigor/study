module Chapter2.Ex_2_4 where

import Data.Maybe (fromMaybe)

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Ord, Show)

insert :: Ord a => a -> Tree a -> Tree a
insert elem Empty =
    Node Empty elem Empty
insert elem tree@(Node _ x _) =
    fromMaybe tree (go elem tree x)
  where
    go elem Empty maybeEqual
      | elem == maybeEqual = Nothing
      | otherwise = Just $ Node Empty elem Empty
    go elem (Node l x r) maybeEqual
      | elem < x = go elem l maybeEqual >>= \l -> Just $ Node l x r
      | otherwise = go elem r x >>= \r -> Just $ Node l x r

