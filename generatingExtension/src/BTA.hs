module BTA where

import L
import Control.Monad.Trans.State
import Data.Maybe (fromJust)
import Eval (evalOp)
import Parser.Parser (parser)

data Annotation = Static
                | Dynamic
                deriving (Eq, Show)

data ExprA a = LitA Int a
             | VarA Var a
             | BinOpA Op (ExprA a) (ExprA a) a
             deriving Show

data StmtA a  = AssignA Var (ExprA a) a
              | ReadA Var a
              | WriteA (ExprA a) a
              deriving Show

type AnnotatedL a = [StmtA a]

type AnnotatedVar = (Var, Annotation)

run :: String -> Either String L
run string =
  case parser string of
    Left err -> Left err
    Right program ->
      Right $ optimize program

optimize :: L -> L
optimize = eraseAnnotations . optimizeExpr . bta

bta :: L -> AnnotatedL Annotation
bta program =
    evalState (go program) []
  where
    go :: L -> State [AnnotatedVar] (AnnotatedL Annotation)
    go stmts =
      mapM goStmt stmts
    goStmt :: Stmt -> State [AnnotatedVar] (StmtA Annotation)
    goStmt (Read v) = do
      modify ((v, Dynamic):)
      return $ ReadA v Dynamic
    goStmt (Assign v expr) = do
      e <- goExpr expr
      let annotation = getAnnotation e
      modify ((v, annotation):)
      return $ AssignA v e annotation
    goStmt (Write expr) = do
      e <- goExpr expr
      let annotation = getAnnotation e
      return $ WriteA e annotation

    goExpr :: Expr -> State [AnnotatedVar] (ExprA Annotation)
    goExpr (Lit x) = return $ LitA x Static
    goExpr (Var v) = do
      annotations <- get
      case lookup v annotations of
        Just a -> return $ VarA v a
        Nothing -> return $ VarA v Dynamic
    goExpr (BinOp op l r) = do
      l <- goExpr l
      r <- goExpr r
      let la = getAnnotation l
      let ra = getAnnotation r
      case (la, ra) of
        (Static, Static) -> return $ BinOpA op l r Static
        _ -> return $ BinOpA op l r Dynamic

    getAnnotation (LitA _ a) = a
    getAnnotation (BinOpA _ _ _ a) = a
    getAnnotation (VarA _ a) = a

optimizeExpr :: AnnotatedL Annotation -> AnnotatedL Annotation
optimizeExpr program =
    evalState (mapM goStmt program) []
  where
    goStmt (AssignA v e a) = do
      e <- goExpr e
      return $ AssignA v e a
    goStmt (WriteA e a) = do
      e <- goExpr e
      return $ WriteA e a
    goStmt r = return r

    goExpr (VarA v Static) = do
      varState <- get
      return $ LitA (fromJust $ lookup v varState) Static
    goExpr v@(VarA _ Dynamic) = return v
    goExpr l@(LitA n _) = return l
    goExpr e@(BinOpA op l r a) = do
      l <- goExpr l
      r <- goExpr r
      return $ binOp op l r

    binOp op (LitA l _) (LitA r _) =
      LitA (evalOp op l r) Static
    binOp Plus (LitA 0 _) e = e
    binOp Plus e (LitA 0 _) = e
    binOp Mult (LitA 1 _) e = e
    binOp Mult e (LitA 1 _) = e
    binOp Mult (LitA 0 _) _ = LitA 0 Static
    binOp Mult _ (LitA 0 _) = LitA 0 Static
    binOp op l r =
      BinOpA op l r Dynamic

eraseAnnotations :: AnnotatedL a -> L
eraseAnnotations = map goStmt
  where
    goStmt (ReadA x _) = Read x
    goStmt (WriteA e _) = Write (goExpr e)
    goStmt (AssignA v e _) = Assign v (goExpr e)

    goExpr (LitA n _) = Lit n
    goExpr (VarA v _) = Var v
    goExpr (BinOpA op l r _) =
      BinOp op (goExpr l) (goExpr r)