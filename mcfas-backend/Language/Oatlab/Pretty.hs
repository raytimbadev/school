{-|
Module      : Language.Oatlab.Pretty
Description : Pretty-printing higher-order F-algebra
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental

This module defines the 'ppAlg' higher-order F-algebra for pretty-printing
unannotated Oatlab syntax trees. Any annotated syntax tree can be
systematically unannotated using the 'stripI' or 'stripH' functions from
"Data.Annotation", so annotations in no way make pretty-printing more
difficult.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Oatlab.Pretty
( ppAlg
, renderOatlab
) where

import Data.HFunctor ( K(..), (:~>) )
import Language.Oatlab.Syntax

import Text.PrettyPrint

joinLinesBy :: [Doc] -> Doc -> Doc
joinLinesBy docs s = foldr (\next done -> next $+$ s $+$ done) empty docs

joinLines :: [Doc] -> Doc
joinLines = foldr ($+$) empty

-- | Higher-order F-algebra for pretty-printing.
ppAlg :: OatlabAstF (K Doc) :~> K Doc
ppAlg node = case node of
  ProgramDecl (map unK -> topLevelDecls) -> K (topLevelDecls `joinLinesBy` "")
  FunctionDecl (K ident) (map unK -> params) (map unK -> body) ->
    K (
      "function" <+> ident <> parens (hcat $ punctuate ", " params) <+> "{" $+$
      nest 4 (joinLines body) $+$
      "}"
    )
  WhileLoop (K expr) (map unK -> body) ->
    K (
      "while" <+> parens expr <+> "{" $+$ nest 4 (joinLines body) $+$ "}"
    )
  ForLoop (K ident) (K expr) (map unK -> body) ->
    K (
      "for" <+> ident <+> "=" <+> expr <+> "{" $+$ nest 4 (joinLines body) $+$ "}"
    )
  Branch (K expr) (map unK -> body1) body2 ->
    K (
      "if" <+> parens expr <+> "{" $+$ nest 4 (joinLines body1) $+$ "}"
      $+$ maybe
        empty
        (\(map unK -> body') -> "else {" $+$ nest 4 (joinLines body') $+$ "}")
        body2
    )
  Return (K expr) -> K ("return" <+> expr <> ";")
  Assignment (K lhs) (K rhs) -> K (lhs <+> "=" <+> rhs <> ";")
  Expression (K expr) -> K (expr <> ";")
  Var (K ident) -> K ident
  BinaryOperation op (K opl) (K opr) ->
    K (
      parens (opl <+> ppOp op <+> opr)
    )
  Call (K expr) (map unK -> exprs) ->
    K (
      parens expr <> parens (hcat $ punctuate ", " exprs)
    )
  StringLiteral str -> K (doubleQuotes (text str))
  NumericLiteral num -> K (double num)
  Identifier name -> K (text name)
  VarDecl (K ident) -> K ident

-- | Pretty-print a binary operator.
ppOp :: BinaryOperator -> Doc
ppOp op = case op of
  Addition -> "+"
  Multiplication -> "*"
  Division -> "/"
  Subtraction -> "-"
  Modulo -> "%"
  LessThan -> "<"
  LessThanEqual -> "<="
  GreaterThan -> ">"
  GreaterThanEqual -> ">="
  Equal -> "=="
  NotEqual -> "!="
  RangeToFrom -> ":"
  IndexBy -> "@"

-- | Render a pretty-printed document into a string, so that you can do things
-- with it (like print it or save it to a file).
renderOatlab :: Doc -> String
renderOatlab
  = renderStyle
    Style
      { mode = PageMode
      , lineLength = maxBound
      , ribbonsPerLine = 1
      }
