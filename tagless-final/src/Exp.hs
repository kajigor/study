{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module Exp where

import Text.Printf

-- Initial

data Exp = Lit Int
         | Neg Exp
         | Add Exp Exp
         deriving (Show, Eq, Ord)

t1 :: Exp
t1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

class ExpSYM repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

instance ExpSYM Int where
  lit n = n
  neg e = - e
  add e1 e2 = e1 + e2

class MulSYM repr where
  mul :: repr -> repr -> repr

instance MulSYM Int where
  mul e1 e2 = e1 * e2

instance MulSYM String where
  mul e1 e2 = printf "(%s * %s)" e1 e2

tf1 :: ExpSYM repr => repr
tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))

tfm1 :: (MulSYM repr, ExpSYM repr) => repr
tfm1 = add (mul tf1 (lit 13)) (neg (lit 42))

eval :: Int -> Int
eval = id

instance ExpSYM String where
  lit n = show n
  neg e = printf "(- %s)" e
  add e1 e2 = printf "(%s + %s)" e1 e2

view :: String -> String
view = id

run = do
  print $ eval tf1
  putStrLn $ view tf1
  print $ eval tfm1
  putStrLn $ view tfm1