{-# LANGUAGE GADTs #-}

module Main where

data Expr a where
  Num :: Int -> Expr Int
  Bool :: Bool -> Expr Bool
  Plus :: Expr Int -> Expr Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  Equal :: Eq a => Expr a -> Expr a -> Expr Bool

instance Eq a => Eq (Expr a) where
  (==) =
      eqExpr
    where
      eqExpr :: Expr a -> Expr b -> Bool
      eqExpr (Num x) (Num y) = x == y
      eqExpr (Bool x) (Bool y) = x == y
      eqExpr (Plus x x') (Plus y y') = eqExpr x y && eqExpr x' y'
      eqExpr (If c t e) (If c' t' e') = eqExpr c c' && eqExpr t t' && eqExpr e e'
      eqExpr (Equal x x') (Equal y y') = eqExpr x y && eqExpr x' y'
      eqExpr _ _ = False

-- -- Won't compile because it cannot determine that x and y in Expr case have the same type.
-- instance Eq a => Eq (Expr a) where
--   Num x == Num y = x == y
--   Bool x == Bool y = x == y
--   Equal x x' == Equal y y' = x == y && x' == y'
--   Plus x x' == Plus y y' = x == y && x' == y'
--   If c t e == If c' t' e' = c == c' && t == t' && e == e'
--   _ == _ = False


eval :: Expr a -> a
eval (Num x) = x
eval (Bool x) = x
eval (Plus x y) = eval x + eval y
eval (If c t e) = if eval c then eval t else eval e
eval (Equal x y) = eval x == eval y

main :: IO ()
main = do
  let expr1 = If (Equal (Num 13) (Num 42)) (Bool True) (Bool False)
  let expr2 = If (Equal (Num 13) (Num 42)) (Num 42) (Num 777)
  print (eval expr1)
  print (eval expr2)
  print (expr1 == expr1)


-- -- Not a scalable solution

-- {-# LANGUAGE GADTs #-}

-- module Main where

-- data Expr a b where
--   Num :: Int -> Expr Int b
--   Bool :: Bool -> Expr Bool b
--   Plus :: Expr Int b -> Expr Int b -> Expr Int b
--   If :: Expr Bool b -> Expr a b -> Expr a b -> Expr a b
--   Equal :: Eq a => Expr a a -> Expr a a -> Expr Bool a

-- instance Eq a => Eq (Expr a b) where
--   Num x == Num y = x == y
--   Bool x == Bool y = x == y
--   Equal x x' == Equal y y' = x == y && x' == y'
--   Plus x x' == Plus y y' = x == y && x' == y'
--   If c t e == If c' t' e' = c == c' && t == t' && e == e'
--   _ == _ = False

-- eval :: Expr a b -> a
-- eval (Num x) = x
-- eval (Bool x) = x
-- eval (Equal x y) = eval x == eval y
-- eval (Plus x y) = eval x + eval y
-- eval (If c t e) = if eval c then eval t else eval e

-- main :: IO ()
-- main = do
--   let expr1 = If (Equal (Num 13) (Num 42)) (Bool True) (Bool False)
--   let expr2 = Plus (If (Equal (Num 13) (Num 42)) (Num 42) (Num 777)) (Num 1)
--   let expr3 = Equal (Plus (Num 1) (Num 2)) (If (Bool True) (Num 3) (Num 4))
--   print (eval expr1)
--   print (eval expr2)
--   print (expr1 == expr1)




-- more details see in the question https://stackoverflow.com/questions/68110623/equality-for-gadts-which-erase-type-parameter