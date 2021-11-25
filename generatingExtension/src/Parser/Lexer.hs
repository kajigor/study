module Parser.Lexer where

import Parser.Data ( Parser )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad ( void )
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, lowerChar, upperChar, spaceChar, digitChar, punctuationChar )
import Text.Megaparsec ( (<|>), between, many, some, MonadParsec(try), notFollowedBy )
import Text.Printf ( printf )

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

roundBr :: Parser a -> Parser a
roundBr = between (symbol "(") (symbol ")")

curlyBr :: Parser a -> Parser a
curlyBr = between (symbol "{") (symbol "}")

identLetters :: Parser Char
identLetters =
  char '_' <|> alphaNumChar <|> char '\''

reserved = ["read", "write", "if", "else", "while"]

notReserved :: (Monad m, MonadFail m) => [String] -> String -> m String
notReserved reserved x | x `elem` reserved = fail $ printf "%s is reserved" (show x)
notReserved reserved x = return x

identifier :: Parser String
identifier =
    (lexeme . try) (p >>= notReserved reserved)
  where
    p = (:) <$> letterChar <*> many identLetters

number :: Parser Int
number = read <$> L.lexeme sc (some digitChar)

colon :: Parser String
colon = symbol ";"

plus :: Parser String
plus = symbol "+"

mult :: Parser String
mult = symbol "*"

assign :: Parser String
assign = symbol ":="

op :: String -> Parser String
op n = (lexeme . try) (symbol n <* notFollowedBy punctuationChar)