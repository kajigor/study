module Chapter2.Ex_2_2 where

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Ord, Show)

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member elem t@(Node _ x _) =
    go elem t x
  where
    go elem Empty x = x == elem
    go elem (Node l y r) x
      | y < elem = go elem r x
      | otherwise = go elem l y