{-# LANGUAGE DeriveFunctor #-}

module Language.Minilang.Syntax where

import Prelude hiding ( print, read )
import Data.Text ( Text )

data Statement
    = Assign Ident Expr
    | While Expr [Statement]
    | If Expr [Statement] [Statement]
    | Print Expr
    | Read Ident
    deriving (Eq, Read, Show)

data Declaration
    = Var Ident Type
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

type Program = ([Declaration], [Statement])
type Ident = Text
