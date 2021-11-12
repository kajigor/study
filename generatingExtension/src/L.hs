module L where

type Var = String

data Op = Plus
        | Mult
        deriving Show

data Expr = Lit Int
          | Var Var
          | BinOp Op Expr Expr
          deriving Show

data Stmt = Assign Var Expr
          | Read Var
          | Write Expr
          deriving Show

type L = [Stmt]

type VarState = [(Var, Int)]
type Input = [Int]
type Output = [Int]

type ProgramState = (VarState, Input, Output)
