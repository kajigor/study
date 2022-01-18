{-# LANGUAGE FlexibleInstances #-}
module PushNeg where

import Exp
import Data.Bits (Bits(xor))

-- Initial

pushNeg (Lit n) = Lit n
pushNeg (Neg (Lit n)) = Neg (Lit n)
pushNeg (Neg (Neg e)) = pushNeg e
pushNeg (Neg (Add e1 e2)) = Add (pushNeg $ Neg e1) (pushNeg $ Neg e2)
pushNeg (Add e1 e2) = Add (pushNeg e1) (pushNeg e2)

flatA (Lit n) = Lit n
flatA (Neg x) = Neg x
flatA (Add (Add e1 e2) e3) = flatA $  Add e1 (Add e2 e3)
flatA (Add e1 e2) = Add e1 (flatA e2)

simple = flatA . pushNeg

t2 :: Exp
t2 = Add (Lit 8) (Neg (Add (Neg $ Lit 1) (Lit 2)))



t3 :: Exp
t3 = Add (Lit 8) (Neg (Add (Neg $ (Add (Add (Lit 1) (Lit 2)) (Add (Lit 3) (Lit 4)))) (Lit 2)))


-- Final

data Ctx = P | N

instance ExpSYM repr => ExpSYM (Ctx -> repr) where
  lit n P = lit n
  lit n N = neg $ lit n
  neg e P = e N
  neg e N = e P
  add e1 e2 ctx = add (e1 ctx) (e2 ctx)

instance MulSYM repr => MulSYM (Ctx -> repr) where
  mul e1 e2 P = mul (e1 P) (e2 P)
  mul e1 e2 N = mul (e1 P) (e2 N)

pushNegF :: ExpSYM t => (Ctx -> t) -> t
pushNegF e = e P

data AddCtx e = RightChild e | Leftmost

instance ExpSYM repr => ExpSYM (AddCtx repr -> repr) where
  lit n Leftmost = lit n
  lit n (RightChild r) = add (lit n) r
  neg e Leftmost = neg (e Leftmost)
  neg e (RightChild r) = add (neg (e Leftmost)) r
  add e1 e2 ctx = e1 (RightChild (e2 ctx))

pushAddF :: ExpSYM e => (AddCtx e -> e) -> e
pushAddF e = e Leftmost

tf2 :: ExpSYM repr => repr
tf2 = add (lit 8) (neg (add (neg $ lit 1) (lit 2)))


tf3 :: ExpSYM repr => repr
tf3 = add (add (neg (add (lit 1) (lit 2))) (lit 3)) (add (lit 4) (lit 5))


tfm2 :: (ExpSYM repr, MulSYM repr) => repr
tfm2 = mul (neg $ neg $ neg (lit 1)) tf2

simpleF = pushAddF . pushNegF


doStuff :: Exp -> IO ()
doStuff e = do
  print e
  print $ pushNeg e
  print $ simple e
  putStrLn "=====================\n"


run = do
  doStuff t2
  doStuff t3
  putStrLn $ view tfm2
  putStrLn $ view $ pushNegF tfm2
  putStrLn $ view $ simpleF tf3
