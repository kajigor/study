module Final where

import Common
import Text.Printf
import Control.Monad.State hiding (fix)
import Prelude hiding (compare)

class Symantics repr where
  intConst :: Int -> repr Int
  boolConst :: Bool -> repr Bool

  intBin :: BinOp (Int -> Int -> Int) -> repr Int -> repr Int -> repr Int
  boolBin :: BinOp (Bool -> Bool -> Bool) -> repr Bool -> repr Bool -> repr Bool
  compare :: BinOp (Int -> Int -> Bool) -> repr Int -> repr Int -> repr Bool
  ifExpr :: repr Bool -> repr t -> repr t -> repr t

  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
  fix :: (repr a -> repr a) -> repr a


instance Symantics Env where
  intConst n = Env $ n
  boolConst b = Env b

  intBin (BinOp _ f) x y = Env $ f (unEnv x) (unEnv y)
  boolBin (BinOp _ f) x y = Env $ f (unEnv x) (unEnv y)
  compare (BinOp _ f) x y = Env $ f (unEnv x) (unEnv y)
  ifExpr cond thn els = Env $ if unEnv cond then unEnv thn else unEnv els

  lam f = Env $ \x -> unEnv (f (Env x))
  app f x = Env $ (unEnv f) (unEnv x)
  fix f = Env $ fx (unEnv . f . Env) where fx f = f (fx f)

eval :: Env a -> a
eval = unEnv

instance Symantics S where
  intConst n = S $ return $ show n
  boolConst b = S $ return $ show b

  intBin op x y = S $ do
    x <- unS x
    y <- unS y
    return $ formatBinOp op x y
  boolBin op x y = S $ do
    x <- unS x
    y <- unS y
    return $ formatBinOp op x y
  compare op x y = S $ do
    x <- unS x
    y <- unS y
    return $ formatBinOp op x y
  ifExpr cond thn els = S $ do
    cond <- unS cond
    thn <- unS thn
    els <- unS els
    return $ printf "if %s then %s else %s" cond thn els

  lam f = S $ do
    v <- newVar
    let var = printf "x.%s" (show v) :: String
    x <- unS (f (S $ return var))
    return $ printf "\\%s -> %s" var x

  app x y = S $ do
    x <- unS x
    y <- unS y
    return $ printf "(%s) (%s)" x y

  fix f = S $ do
    v <- newVar
    let self = printf "self.%s" (show v) :: String
    b <- unS (f (S $ return self))
    return $ printf "(fix %s . %s)" self b

view :: S a -> String
view x = evalState (unS x) (VarState 0)

andOp :: Symantics repr => repr Bool -> repr Bool -> repr Bool
andOp = boolBin (BinOp "&&" (&&))

mulOp :: Symantics repr => repr Int -> repr Int -> repr Int
mulOp = intBin (BinOp "*" (*))

addOp :: Symantics repr => repr Int -> repr Int -> repr Int
addOp = intBin (BinOp "+" (+))

leqOp :: Symantics repr => repr Int -> repr Int -> repr Bool
leqOp = compare (BinOp "<=" (<=))

tipow :: Symantics repr => repr (Int -> Int -> Int)
tipow  = lam (\x -> fix (\self -> lam (\n ->
                        ifExpr  (leqOp n (intConst 0))
                                (intConst 1)
                                (mulOp x (app self (addOp n (intConst (-1))))))))

tipowApplied :: Symantics repr => Int -> Int -> repr Int
tipowApplied x y = app (app tipow (intConst x)) (intConst y)

