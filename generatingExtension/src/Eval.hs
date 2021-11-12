module Eval where

import Parser.Parser (parser)
import L
import Control.Monad.Trans.State
import Control.Monad.Trans.Class


evalOp :: Op -> (Int -> Int -> Int)
evalOp Plus = (+)
evalOp Mult = (*)

evalExpr :: Expr -> StateT VarState Maybe Int
evalExpr (Lit x) = return x
evalExpr (Var v) = do
  state <- get
  case lookup v state of
    Just x -> return x
    Nothing -> lift Nothing
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