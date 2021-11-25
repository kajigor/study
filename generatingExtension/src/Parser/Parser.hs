module Parser.Parser (parser) where

import Text.Megaparsec
import Parser.Lexer
    ( sc, symbol, roundBr, curlyBr, identifier, colon, number, op )
import Parser.Data ( Parser )
import L
import Control.Monad.Combinators.Expr

parser :: String -> Either String L
parser =
    runBundlingParser parseL ""

runBundlingParser :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s b -> String -> s -> Either String b
runBundlingParser parser filePath =
    mapLeft errorBundlePretty . runParser parser filePath
  where
    mapLeft f (Left x) = Left $ f x
    mapLeft _ (Right x) = Right x

parseLit :: Parser Expr
parseLit = Lit <$> number <?> "literal"

parseVar :: Parser Expr
parseVar = Var <$> identifier <?> "variable"

parseExprBr :: Parser Expr
parseExprBr = roundBr parseExpr <?> "expression in brackets"

parseBaseExpr :: Parser Expr
parseBaseExpr =
      parseLit
  <|> parseVar
  <|> parseExprBr

table :: [[Operator Parser Expr]]
table = [ [ infixR (symbol "^") (BinOp Pow) ]
        , [ infixL (symbol "*") (BinOp Mult), infixL (symbol "/") (BinOp Div) ]
        , [ infixL (symbol "+") (BinOp Plus), infixL (symbol "-") (BinOp Minus) ]
        , [ infixN (op "<=") (BinOp Le)
          , infixN (op "<") (BinOp Lt)
          , infixN (op ">=") (BinOp Ge)
          , infixN (op ">") (BinOp Gt)
          , infixN (op "==") (BinOp Eq)
          , infixN (op "/=") (BinOp Neq)
          ]
        , [ infixR (symbol "&&") (BinOp And) ]
        , [ infixR (symbol "||") (BinOp Or) ]
        ]

infixL :: Parser String -> (a -> a -> a) -> Operator Parser a
infixL op f = InfixL (f <$ op)

infixR :: Parser String -> (a -> a -> a) -> Operator Parser a
infixR op f = InfixR (f <$ op)

infixN :: Parser String -> (a -> a -> a) -> Operator Parser a
infixN op f = InfixN (f <$ op)

parseExpr :: Parser Expr
parseExpr = makeExprParser parseBaseExpr table <?> "expression"

parseAssign :: Parser Stmt
parseAssign = do
  v <- identifier
  symbol ":="
  Assign v <$> parseExpr
  <?> "parseAssign"

parseRead :: Parser Stmt
parseRead = do
  symbol "read"
  Read <$> identifier
  <?> "parseRead"

parseWrite :: Parser Stmt
parseWrite = do
  symbol "write"
  Write <$> parseExpr
  <?> "parseWrite"

parseStmt :: Parser Stmt
parseStmt =
      parseRead <* colon
  <|> parseWrite <* colon
  <|> parseAssign <* colon
  <|> parseIf
  <|> parseWhile

parseIf :: Parser Stmt
parseIf = do
  symbol "if"
  cond <- parseExprBr
  thn <- curlyBr parseStmts
  symbol "else"
  els <- curlyBr parseStmts
  return $ If cond thn els

parseWhile :: Parser Stmt
parseWhile = do
  symbol "while"
  cond <- parseExprBr
  body <- curlyBr parseStmts
  return $ While cond body

parseStmts :: Parser L
parseStmts = many parseStmt

parseL :: Parser L
parseL = parseStmts <* eof