module Language.Minilang.Parser.Expression
( expr
) where

import Language.Minilang.Lexer
import Language.Minilang.Syntax

import Text.Megaparsec
import Text.Megaparsec.Expr

expr :: Parser Expr
expr = makeExprParser term table

term :: Parser Expr
term = fmap Literal literal
    <|> parens expr

literal :: Parser Literal
literal
    = fmap Real floatLiteral
    <|> fmap Int integerLiteral
    <|> fmap String stringLiteral
    <|> fmap Variable identifier

table :: [[Operator Parser Expr]]
table =
    [ [ Prefix $ tokMinus *> unaryMinus
      ]
    , [ InfixL $ tokTimes *> times
      , InfixL $ tokDivide *> divide
      ]
    , [ InfixL $ tokPlus *> plus
      , InfixL $ tokMinus *> minus
      ]
    ] where
        unaryMinus = pure (UnaryOp Negative)
        times = pure (BinaryOp Times)
        divide = pure (BinaryOp Divide)
        plus = pure (BinaryOp Plus)
        minus = pure (BinaryOp Minus)
