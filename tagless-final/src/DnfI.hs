module DnfI where

import PushNeg (simpleF)
import Text.Printf (printf)

data Expr = Lit Int
          | Neg Expr
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Eq)

instance Show Expr where
  show (Lit n) = show n
  show (Neg e) = printf "(- %s)" $ show e
  show (Add x y) = printf "(%s + %s)" (show x) (show y)
  show (Mul x y) = printf "(%s * %s)" (show x) (show y)

pushNeg (Lit n) = Lit n
pushNeg (Neg (Lit n)) = Neg (Lit n)
pushNeg (Neg (Neg e)) = pushNeg e
pushNeg (Neg (Add x y)) = Add (pushNeg $ Neg x) (pushNeg $ Neg y)
pushNeg (Neg (Mul x y)) = Mul (pushNeg x) (pushNeg $ Neg y)
pushNeg (Add x y) = Add (pushNeg x) (pushNeg y)
pushNeg (Mul x y) = Mul (pushNeg x) (pushNeg y)


bdist e =
    go [] e
  where
    go ctx (Lit n) = foldl Mul (Lit n) ctx
    go ctx (Neg e) = foldl Mul (Neg e) ctx
    go ctx (Add x y) = flat $ Add (go ctx x) (go ctx y)
    go ctx (Mul x y) =
      case go ctx x of
        Add x' y' -> flat $ Add (go [x'] y) (go [] (Mul y' y))
        e -> go [e] y

flat (Lit n) = Lit n
flat (Neg x) = Neg x
flat (Add (Add x y) z) = flat $ Add x (Add y z)
flat (Add x y) = Add (flatMul x) (flat y)
flat (Mul x y) = flatMul $ Mul (flat x) (flat y)

flatMul (Mul (Mul x y) z) = flatMul $ Mul x (Mul y z)
flatMul (Mul x y) = Mul (flat x) (flat y)
flatMul (Add x y) = Add (flatMul x) (flatMul y)
flatMul x = x

norm = bdist . flat . pushNeg

t0 x y z t = Mul (Add (Lit x) (Lit y)) (Add (Lit z) (Lit t))
t1 x y z t = Neg (Mul (Add (Neg (Lit x)) (Lit y)) (Neg (Add (Lit z) (Lit t))))
t' = Mul (Add (Mul (Add (Lit 1) (Lit 2)) (Lit 3)) (Lit 4)) (Lit 5)

doStuff e = do
  print e
  print $ pushNeg e
  print $ flat $ pushNeg e
  let n = norm e
  print $ n
  print $ eval e == eval n
  print $ isNorm n

isNorm (Add x y) = isMult x && isNorm y
isNorm x = isMult x

isMult (Mul x y) = isTrivial x && isMult y
isMult x = isTrivial x

isTrivial (Lit _) = True
isTrivial (Neg _) = True
isTrivial _ = False



eval (Lit n) = n
eval (Neg e) = (-1) * eval e
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

run = do
  doStuff t'
  doStuff (Mul (t0 1 2 3 4) (t0 5 6 7 8))
  doStuff (Mul (Add (Lit 2) (Lit 3)) (Add (Mul (Add (Lit 2) (Lit 3)) (Add (Lit 2) (Lit 3))) (Lit 3)))

  doStuff (Mul (Mul (Lit 0) (Lit 0)) (Lit 0))

