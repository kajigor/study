module Parser.Parser (parser) where

import Text.Megaparsec
import Parser.Lexer
    ( sc, symbol, roundBr, identifier, colon, number )
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
table = [ [ binary  "*" (BinOp Mult) ]
        , [ binary  "+" (BinOp Plus) ] ]

binary :: String -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

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
      parseRead
  <|> parseWrite
  <|> parseAssign

parseL :: Parser L
parseL = sepBy1 parseStmt colon