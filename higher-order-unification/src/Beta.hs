module Beta where

import Syntax ( Term(..) )
import qualified Subst

beta :: Ord a => Term a -> Term a
beta (App (Abs v t) s) = Subst.apply t (Subst.singleton v s)
beta x = x