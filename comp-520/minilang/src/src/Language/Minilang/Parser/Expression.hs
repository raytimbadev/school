module Language.Minilang.Parser.Expression
( expr
) where

import Language.Minilang.Annotation
import Language.Minilang.Lexer
import Language.Minilang.Syntax
import Language.Minilang.SrcAnn

import Data.Functor
import Data.Functor.Foldable
import Data.Functor.Identity
import Text.Megaparsec
import Text.Megaparsec.Expr

expr :: Parser SrcAnnExpr
expr = makeExprParser term table

term :: Parser SrcAnnExpr
term = l
    <|> parens expr where
        l :: Parser SrcAnnExpr
        l = do
            lit <- withSrcAnn' Literal literal
            pure (Fix lit)

literal :: Parser SrcAnnLiteral
literal = withSrcAnn' Identity unannotated where
    unannotated
        = fmap Real floatLiteral
        <|> fmap Int integerLiteral
        <|> fmap String stringLiteral
        <|> fmap Variable identifier

table :: [[Operator Parser SrcAnnExpr]]
table =
    [ [ Prefix unaryMinus
      ]
    , [ InfixL $ times
      , InfixL $ divide
      ]
    , [ InfixL $ plus
      , InfixL $ minus
      ]
    ] where
        unaryMinus :: Parser (SrcAnnExpr -> SrcAnnExpr)
        unaryMinus = do
            m <- withSrcAnn' Identity (tokMinus $> Negative)
            let (Ann p _) = m
            pure (\e@(Fix (Ann a _)) ->
                Fix $ Ann (SrcSpan (srcStart p) (srcEnd a))
                          (UnaryOp m e))

        bin op tok = do
            m <- withSrcAnn' Identity (tok $> op)
            pure (\e1@(Fix (Ann a1 _)) e2@(Fix (Ann a2 _)) ->
                Fix $ Ann (SrcSpan (srcStart a1) (srcEnd a2))
                          (BinaryOp m e1 e2))

        times :: Parser (SrcAnnExpr -> SrcAnnExpr -> SrcAnnExpr)
        times = bin Times tokTimes

        divide :: Parser (SrcAnnExpr -> SrcAnnExpr -> SrcAnnExpr)
        divide = bin Divide tokDivide

        plus :: Parser (SrcAnnExpr -> SrcAnnExpr -> SrcAnnExpr)
        plus = bin Plus tokPlus

        minus :: Parser (SrcAnnExpr -> SrcAnnExpr -> SrcAnnExpr)
        minus = bin Minus tokMinus
