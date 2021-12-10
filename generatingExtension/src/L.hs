module L where

type Var = String

data Op = Plus
        | Minus
        | Mult
        | Div
        | Pow
        | Eq
        | Neq
        | Lt
        | Le
        | Gt
        | Ge
        | And
        | Or
        deriving Show

data Expr = Lit Integer
          | Var Var
          | BinOp Op Expr Expr
          deriving Show

data Stmt = Assign Var Expr
          | Read Var
          | Write Expr
          | If Expr L L
          | While Expr L
          deriving Show

type L = [Stmt]

type VarState = [(Var, Integer)]
type Input = [Integer]
type Output = [Integer]

type ProgramState = (VarState, Input, Output)
