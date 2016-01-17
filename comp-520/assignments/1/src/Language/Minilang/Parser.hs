module Language.Minilang.Parser where

import Control.Monad (mzero)
import Language.Minilang.Syntax (Program)
import qualified Language.Minilang.Syntax as S
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

spaceConsumer = L.space space mzero mzero
lexeme = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer

parens = between (symbol "(") (symbol ")")

semicolon = symbol ";"
equals = symbol "="

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

minilang :: Parser (Program ())
minilang = sequence_ (many stmt)

stmt :: Parser (Program ())
stmt
    = varStmt
    <|> assignStmt
    <|> whileStmt
    <|> ifStmt
    <|> printStmt
    <|> readStmt

identifier :: Parser String
identifier = do
    c <- letterChar
    s <- many alphaNumChar
    _ <- many space
    return (c:s)

varStmt :: Parser (Program ())
varStmt = do
    symbol "var"
    
assignStmt = undefined
whileStmt = undefined
ifStmt = undefined
printStmt = undefined
readStmt = undefined
