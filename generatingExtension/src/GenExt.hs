module GenExt where

import L
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Text.Printf (printf)

type HS = String

preamble :: HS
preamble =
  "{-# LANGUAGE ScopedTypeVariables #-}\n\
  \module Output where\n\n\
  \import Control.Monad.Trans.State\n\
  \import Control.Monad.Trans.Class\n\n\
  \"

-- mainDef :: HS -> HS
-- mainDef =
--   printf
--     "main :: IO ()\n\
--     \main = do \n\
--     \  let res = evalStateT (%s) ([(\"x\", 5)])\n\
--     \  print $ case res of\n\
--     \    Just output -> Just $ output\n\
--     \    Nothing -> Nothing\n\
--     \"

-- evalLGE :: Expr -> HS
-- evalLGE e =
--   printf "%s%s" preamble (mainDef $ evalExprGE e)


mainDef :: HS -> HS
mainDef =
  printf
    "main :: IO ()\n\
    \main = do \n\
    \  let compiled = %s\n\
    \  let res = execStateT compiled ([(\"x\", 5)], [13], [])\n\
    \  print $ case res of\n\
    \    Just (_, _, output :: [Int]) -> Just $ output\n\
    \    Nothing -> Nothing\n\
    \"

evalLGE :: Stmt -> HS
evalLGE e =
  printf "%s%s" preamble (mainDef $ evalStmtGE e)


evalOpGE :: Op -> HS
evalOpGE Plus = "(+)"
evalOpGE Mult = "(*)"

evalExprGE :: Expr -> HS
evalExprGE (Lit x) = printf "(return %s)" (show x)
evalExprGE (Var v) = printf "do {\
  \ state <- get; \
  \ case lookup %s state of {\
  \   Just x -> return x; \
  \    Nothing -> lift Nothing}}"
  (show v)
evalExprGE (BinOp op l r) = printf "do {\
  \ l <- (%s); \
  \ r <- (%s); \
  \ return (%s l r)}"
  (evalExprGE l)
  (evalExprGE r)
  (evalOpGE op)

evalStmtGE :: Stmt -> HS
evalStmtGE (Read v) = printf "do {\
  \(state, x : input, output) <- get; \
  \put ((%s, x) : state, input, output); \
  \return ()}\
  \"
  (show v)
evalStmtGE (Write e) = printf "do { \
  \(state, input, output) <- get; \
  \let r = evalStateT (%s) state; \
  \case r of\
  \  {Just x -> do {\
  \    put (state, input, x : output); \
  \    return ()};\
  \  Nothing ->\
  \    lift Nothing}}\
  \"
  (evalExprGE e)
evalStmtGE (Assign v e) = printf "(do \
  \(state, input, output) <- get; \
  \let r = evalStateT (%s) state; \
  \case r of{\
  \  Just x -> do {\
  \    put ((%s, x) : state, input, output); \
  \    return ()}; \
  \  Nothing ->\
  \    lift Nothing})\
  \"
  (evalExprGE e)
  (show v)



-- evalL :: L -> Input -> Maybe Output
-- evalL program inputs =
--   let res = execStateT (mapM evalStmt program) ([], inputs, []) in
--   case res of
--     Just (_, _, output) -> Just $ reverse output
--     Nothing -> Nothing

-- run :: Input -> String -> Either String Output
-- run inputs string =
--   case parser string of
--     Left err -> Left err
--     Right program ->
--       case evalL program inputs of
--         Nothing -> Left "failed to evaluate"
--         Just output -> Right output
