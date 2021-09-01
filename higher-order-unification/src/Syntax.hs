module Syntax where

import Text.Printf ( printf )

data Term a = FreeVar a
            | BoundVar a
            | Const a
            | Abs a (Term a)
            | App (Term a) (Term a)
            deriving (Eq)

instance Show a => Show (Term a) where
  show (FreeVar x) = printf "F_%s" $ show x
  show (BoundVar x) = printf "B_%s" $ show x
  show (Const x) = printf "C_%s" $ show x
  show (Abs x t) = printf "\\%s.%s" (show x) (show t)
  show (App t1 t2) = printf "(%s . %s)" (show t1) (show t2)
