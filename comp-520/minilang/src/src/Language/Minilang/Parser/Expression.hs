module Language.Minilang.Parser.Expression
( expr
) where

import Language.Minilang.Lexer
import Language.Minilang.Syntax

import Data.Functor.Foldable
import Text.Megaparsec
import Text.Megaparsec.Expr

expr :: Parser Expr
expr = makeExprParser term table

term :: Parser Expr
term = fmap (Fix . Literal) literal
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
        unaryMinus = pure (Fix . UnaryOp Negative)
        bin op = pure (\e1 e2 -> Fix $ BinaryOp op e1 e2)
        times = bin Times
        divide = bin Divide
        plus = bin Plus
        minus = bin Minus
