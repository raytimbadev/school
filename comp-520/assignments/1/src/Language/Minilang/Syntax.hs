{-# LANGUAGE DeriveFunctor #-}

module Language.Minilang.Syntax where

import Prelude hiding ( print, read )
import Data.Text ( Text )

data Statement
    = Var Ident Type
    | Assign Ident Expr
    | While Expr Program
    | If Expr Program Program
    | Print Expr
    | Read Ident
    deriving (Eq, Read, Show)

data Expr
    = BinaryOp BinaryOp Expr Expr
    | UnaryOp UnaryOp Expr
    | Literal Literal
    deriving (Eq, Read, Show)

data BinaryOp
    = Plus | Minus | Times | Divide
    deriving (Eq, Read, Show)

data UnaryOp
    = Negative
    deriving (Eq, Read, Show)

data Literal
    = Variable Text
    | Int Int
    | Real Double
    | String Text
    deriving (Eq, Read, Show)

data Type 
    = TyReal
    | TyInt
    | TyString
    deriving (Eq, Read, Show)

type Program = [Statement]
type Ident = Text
