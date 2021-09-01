module Subst where

import Syntax ( Term(..) )
import qualified Data.Map as Map
import Data.List ( intercalate )
import Data.Maybe ( fromMaybe )
import Text.Printf ( printf )

newtype Subst a = Subst { getSubst :: Map.Map a (Term a) }

instance Show a => Show (Subst a) where
  show (Subst subst) =
    printf "[%s]" $ intercalate ", " $ map (\(k, v) -> printf "%s -> %s" (show k) (show v)) $ Map.toList subst

singleton :: Ord a => a -> Term a -> Subst a
singleton v t = Subst $ Map.fromList [(v, t)]

empty :: Subst a
empty = Subst Map.empty

apply :: Ord a => Term a -> Subst a -> Term a
apply (App t1 t2) subst = App (apply t1 subst) (apply t2 subst)
apply t@(BoundVar x) (Subst subst) =
  fromMaybe t (Map.lookup x subst)
apply x _ = x

