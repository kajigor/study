{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter2.Ex_2_6 where

import Data.Maybe (fromMaybe)

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Ord, Show)

newtype Map k v = Map { getMap :: Tree (k, v) }
                deriving (Eq, Ord, Show)

class Ord k => FiniteMap m k v where
  empty :: m k v
  bind :: k -> v -> m k v -> m k v
  lookup :: k -> m k v -> Maybe v

instance (Ord k, Eq v) => FiniteMap Map k v where
  empty = Map Empty
  bind k v (Map map) =
      Map $ insert k v map
    where
      insert k v Empty = Node Empty (k, v) Empty
      insert k v tree@(Node l (k', v') r)
        | k < k' = Node (insert k v l) (k', v') r
        | k > k' = Node l (k', v') (insert k v r)
        | k == k' && v == v' = tree
        | otherwise = Node l (k, v) r


      -- insert k v Empty =
      --     Node Empty (k, v) Empty
      -- insert k v tree@(Node _ x _) =
      --     fromMaybe tree (go k v tree tree)
      --   where
      --     go _ _ _ Empty = Just Empty
      --     go k v Empty (Node l (k', v') r)
      --       | k == k' && v == v' = Nothing
      --       | k == k' = Just (Node l (k, v) r)
      --       | otherwise = Just $ Node Empty (k, v) Empty
      --     go k v t@(Node l x r) maybeEqual
      --       | k < fst x = go k v l maybeEqual >>= \l -> Just $ Node l x r
      --       | otherwise = go k v r t >>= \r -> Just $ Node l x r
  lookup k (Map map) =
      member k map
    where
      member k Empty = Nothing
      member k t@(Node _ x _) =
          go k t x
        where
          go k Empty x
            | fst x == k = Just $ snd x
            | otherwise = Nothing
          go k (Node l y r) x
            | fst y < k = go k r x
            | otherwise = go k l y
