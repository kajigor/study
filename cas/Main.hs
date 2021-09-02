{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List ( (\\) )
import Text.Printf ( printf )

data Term v =
    Var v
  | App (Term v) (Term v)
  | Abs v (Term v)
  deriving (Show)

type Subst v = v -> Term v

emptySubst :: Eq v => Subst v
emptySubst = \x -> (Var x)

updateSubst :: Eq v => Subst v -> v -> Term v -> Subst v
updateSubst s x t = \y ->
  if y == x then t else s y

singletonSubst :: Eq v => v -> Term v -> Subst v
singletonSubst = updateSubst emptySubst

vars :: Term v -> [v]
vars (Var x) = [x]
vars (App t u) = vars t ++ vars u
vars (Abs x t) = x : vars t

freeVars :: Eq v => Term v -> [v]
freeVars (Var x) = [x]
freeVars (App t u) = freeVars t ++ freeVars u
freeVars (Abs x t) = freeVars t \\ [x]

class Name v where
  genName :: v -> v

instance Name String where
  genName = ('_' :)

instance Name Int where
  genName = (+1)

freshName :: (Eq v, Name v) => v -> Term v -> v
freshName v t =
  let names = iterate genName v in
  let vs = vars t in
  head . filter (`notElem` vs) $ names

identity = Abs "x" (Var "x")
identity' = Abs "y" (Var "y")

first = Abs "x" $ Abs "y" $ Var "x"
second = Abs "x" $ Abs "y" $ Var "y"

replace :: Subst v -> Term v -> Term v
replace s (Var x) = s x
replace s (App t u) = App (replace s t) (replace s u)
replace s (Abs x t) = Abs x (replace s t)

instance (Name v, Eq v) => Eq (Term v) where
  Var x == Var y = x == y
  App t u == App t' u' = t == t' && u == u'
  Abs x t == Abs y u =
    let z = freshName x (App (Var y) (App t u)) in
    replace (singletonSubst x (Var z)) t ==
      replace (singletonSubst y (Var z)) u
  _ == _ = False

cas :: (Name v, Eq v) => v -> Term v -> Term v -> Term v
cas x t (Var y) | x == y = t
cas _ _ (Var y) = Var y
cas x t (App t1 t2) = App (cas x t t1) (cas x t t2)
cas x t' (Abs y t) | x == y = Abs y t
cas x t' (Abs y t) = Abs z (cas x t' (cas y (Var z) t))
  where
    z = freshName x (App t t')

printCas :: (Show v, Name v, Eq v) => v -> Term v -> Term v -> IO ()
printCas x t term =
  putStrLn $ printf "[ %s / %s ] %s =\n\t%s" (show t) (show x) (show term) (show $ cas x t term)

main :: IO ()
main = do
  print identity
  print identity'
  print (identity == identity')
  print (first == second)
  printCas "x" (Var "y") (Abs "x" (Var "y"))
  printCas "y" (Var "y") (Abs "x" (Var "y"))
  printCas "x" identity identity'
  printCas "y" identity identity'
  printCas "x" identity' identity
  printCas "y" identity' identity
  printCas "x" (Var "y") (Abs "z" (App (App (Var "f") (Var "x")) (Var "z")))
  printCas "x" (Var "y") (Abs "y" (App (App (Var "f") (Var "x")) (Var "y")))
  printCas "x" (Var "y") (App (App (Abs "y" (Var "x")) (Abs "x" (Var "x"))) (Var "x"))


