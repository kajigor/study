module Eval where

import Parser.Parser (parser)
import L
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad ( when )
import Options.Applicative.Help (yellow)

evalOp :: Op -> (Int -> Int -> Int)
evalOp Plus  = (+)
evalOp Minus = (-)
evalOp Mult  = (*)
evalOp Div   = div
evalOp Pow   = (^)
evalOp Eq    = transformCompare (==)
evalOp Neq   = transformCompare (/=)
evalOp Lt    = transformCompare (<)
evalOp Le    = transformCompare (<=)
evalOp Gt    = transformCompare (>)
evalOp Ge    = transformCompare (>=)
evalOp And   = transform (&&)
evalOp Or    = transform (||)

transformCompare :: (Int -> Int -> Bool) -> Int -> Int -> Int
transformCompare f x y = boolToInt $ f x y

transform :: (Bool -> Bool -> Bool) -> Int -> Int -> Int
transform f x y = boolToInt $ intToBool x `f` intToBool y

intToBool :: Int -> Bool
intToBool x = x /= 0

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

evalExpr :: Expr -> StateT VarState Maybe Int
evalExpr (Lit x) = return x
evalExpr (Var v) = do
  state <- get
  case lookup v state of
    Just x -> return x
    Nothing -> lift Nothing
evalExpr (BinOp Div l r) = do
  r <- evalExpr r
  if r == 0
  then
    fail "Division by zero"
  else do
    l <- evalExpr l
    return (evalOp Div l r)
evalExpr (BinOp op l r) = do
  l <- evalExpr l
  r <- evalExpr r
  return (evalOp op l r)

evalStmt :: Stmt -> StateT ProgramState Maybe ()
evalStmt (Read v) = do
  (state, x : input, output) <- get
  put ((v, x) : state, input, output)
  return ()
evalStmt (Write e) = do
  (state, input, output) <- get
  let r = evalStateT (evalExpr e) state
  case r of
    Just x -> do
      put (state, input, x : output)
      return ()
    Nothing ->
      lift Nothing
evalStmt (Assign v e) = do
  (state, input, output) <- get
  let r = evalStateT (evalExpr e) state
  case r of
    Just x -> do
      put ((v, x) : state, input, output)
      return ()
    Nothing ->
      lift Nothing
evalStmt (If c thn els) = do
  (state, _, _) <- get
  case evalStateT (evalExpr c) state of
    Just cond ->
      if intToBool cond
      then mapM_ evalStmt thn
      else mapM_ evalStmt els
    Nothing -> lift Nothing
evalStmt w@(While c body) = do
  (state, _, _) <- get
  case evalStateT (evalExpr c) state of
    Just cond ->
      when
        (intToBool cond)
        (do
          mapM_ evalStmt body
          evalStmt (While c body))
    Nothing -> lift Nothing

evalL :: L -> Input -> Maybe Output
evalL program inputs =
  let res = execStateT (mapM evalStmt program) ([], inputs, []) in
  case res of
    Just (_, _, output) -> Just $ reverse output
    Nothing -> Nothing

run :: Input -> String -> Either String Output
run inputs string =
  case parser string of
    Left err -> Left err
    Right program ->
      case evalL program inputs of
        Nothing -> Left "failed to evaluate"
        Just output -> Right output