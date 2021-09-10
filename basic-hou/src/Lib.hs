module Lib where

import Prelude hiding ( abs, zip )
import Control.Monad ( foldM )
import Control.Monad.Trans.State ( get, modify, State, execState, evalState )

someFunc :: IO ()
someFunc = putStrLn "someFunc"


assoc :: Eq a => a -> [(a, b)] -> Maybe b
assoc = lookup

mem :: Eq a => a -> [a] -> Bool
mem = elem

subset :: Eq a => [a] -> [a] -> Bool
subset (x:xs) ys = x `mem` ys && xs `subset` ys
subset [] ys = True

(/\) :: Eq a => [a] -> [a] -> [a]
(x:xs) /\ ys | x `mem` ys = x : (xs /\ ys)
             | otherwise = xs /\ ys
[] /\ _ = []


data Term = V String        --  free var
          | B String        --  bound var
          | C String        --  constant
          | App Term Term   --  application
          | Abs String Term --  abstraction
          deriving (Eq, Show)

newName :: String -> State [String] String
newName pat = do
  existing <- get
  if pat `elem` existing
  then return pat
  else do
    let newPat = '_' : pat
    modify (pat :)
    newName newPat


strip :: Term -> (Term, [Term])
strip t =
    go t []
  where
    go (App s t) ts = go s (t : ts)
    go t ts = (t, ts)

abs :: [Term] -> Term -> Term
abs xs t = foldr (Abs . b1) t xs

b1 :: Term -> String
b1 (B s) = s

hnf :: [Term] -> Term -> [Term] -> Term
hnf xs f ss = abs xs (foldl App f ss)

occ :: String -> [(String, Term)] -> Term -> Bool
occ f s (V g) =
  f == g ||
  case assoc g s of
    Just s' -> occ f s s'
    Nothing -> False
occ f s (App t' t) = occ f s t' || occ f s t
occ f s (Abs _ t) = occ f s t
occ _ _ _ = False

subst :: String -> String -> Term -> State [String] Term
subst y x =
    go
  where
    go b@(B z) | z == x = return $ B y
               | otherwise = return b
    go (App s1 s2) = App <$> go s1 <*> go s2
    go t@(Abs z s) | z == x = return t
                   | z /= y = Abs z <$> go s
                   | otherwise = do
                        z' <- newName z
                        s' <- subst z' z s
                        Abs z' <$> go s'
    go s = return s

red :: Term -> [Term] -> State [String] Term
red (Abs x s) (y : ys) = do
  t <- subst (b1 y) x s
  red t ys
red s (y : ys) = red (App s y) ys
red s [] = return s

devar :: [(String, Term)] -> Term -> State [String] Term
devar s t =
  case strip t of
    (V f, ys) ->
      case assoc f s of
        Just t' -> do
          r <- red t' ys
          devar s r
        Nothing -> return t
    _ -> return t

proj w s' s = do
  term <- devar s' s
  case strip term of
    (Abs x t, _) -> proj (x : w) s' t
    (C _, ss) -> foldM (proj w) s' ss
    (B x, ss) | x `mem` w -> foldM (proj w) s' ss
              | otherwise -> error "Bound variable not in subst"
    (V f, ss) | map b1 ss `subset` w -> return s'
              | otherwise -> do
                v' <- newName f
                return $ (f, hnf ss (V v') (ss /\ map B w)) : s'
    _ -> undefined

eqs (x : xs) (y : ys) | x == y = x : eqs xs ys
                      | otherwise = eqs xs ys
eqs [] [] = []
eqs _ _ = error "Cannot compare lists of different lengths"

flexFlex1 _ ym zn s | ym == zn = return s
flexFlex1 f ym zn s = do
  v' <- newName f
  return $ (f, hnf ym (V v') (eqs ym zn)) : s

flexFlex2 f ym g zn s | ym `subset` zn = return $ (g, hnf zn (V f) ym) : s
                      | zn `subset` ym = return $ (f, hnf ym (V g) zn) : s
                      | otherwise = do
                        let xk = ym /\ zn
                        h <- V <$> newName f
                        return $ (f, hnf ym h xk) : (g, hnf zn h xk) : s

flexFlex f ym g zn s | f == g = flexFlex1 f ym zn s
                     | otherwise = flexFlex2 f ym g zn s

flexRigid f ym t s | occ f s t = error "FlexRigid error"
                   | otherwise = proj (map b1 ym) ((f, abs ym t) : s) t

zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip [] [] = []
zip _ _ = error "Zipping lists of different lengths"

toplevelUnif s t = evalState (unif [] s t) []

unif sub s t = do
  s' <- devar sub s
  t' <- devar sub t
  case (s', t') of
    (Abs x s, Abs y t) | x == y -> unif sub s t
                       | otherwise -> do t' <- subst x y t
                                         unif sub s t'
    (Abs x s, t) -> unif sub s (App t (B x))
    (s, Abs x t) -> unif sub (App s (B x)) t
    (s, t) -> cases sub s t

cases sub s t =
  case (strip s, strip t) of
    ((V f, ym), (V g, zn)) -> flexFlex f ym g zn sub
    ((V f, ym), _) -> flexRigid f ym t sub
    (_, (V f, ym)) -> flexRigid f ym s sub
    ((a, sm), (b, tn)) -> rigidRigid a sm b tn sub

rigidRigid a ss b ts sub | a /= b = error "Unification is impossible for different constants"
                         | otherwise = foldM (\sub (s, t) -> unif sub s t) sub (zip ss ts)

