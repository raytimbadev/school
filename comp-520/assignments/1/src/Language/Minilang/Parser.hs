module Language.Minilang.Parser
( minilang
, Input
, stmt
, varStmt
, assignStmt
, whileStmt
, ifStmt
, printStmt
, readStmt
) where

import Language.Minilang.Lexer
import Language.Minilang.Parser.Expression ( expr )
import Language.Minilang.Syntax

import Text.Megaparsec

minilang :: Parser Program
minilang = many stmt

stmt :: Parser Statement
stmt
    = varStmt
    <|> whileStmt
    <|> ifStmt
    <|> assignStmt
    <|> printStmt
    <|> readStmt

varStmt :: Parser Statement
varStmt = do
    try $ tokVar
    ident <- identifier
    colon
    ty <- type_
    semicolon
    return $ Var ident ty
    
assignStmt :: Parser Statement
assignStmt = do
    ident <- try $ identifier <* equals
    e <- expr
    semicolon
    return $ Assign ident e

whileStmt :: Parser Statement
whileStmt = do
    try tokWhile
    e <- expr
    tokDo
    body <- minilang
    tokDone
    return $ While e body

ifStmt :: Parser Statement
ifStmt = do
    try tokIf
    e <- expr
    tokThen
    thenBody <- minilang
    elseBody <- option [] $ try tokElse *> minilang
    tokEnd
    return $ If e thenBody elseBody
        
printStmt :: Parser Statement
printStmt = do
    try tokPrint
    e <- expr
    semicolon
    return $ Print e

readStmt :: Parser Statement
readStmt = do
    try tokRead
    ident <- identifier
    semicolon
    return $ Read ident
