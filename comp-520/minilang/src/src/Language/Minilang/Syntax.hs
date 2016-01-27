{-# LANGUAGE DeriveFunctor #-}

module Language.Minilang.Syntax where

import Language.Minilang.Precedence
import Language.Minilang.Pretty

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

data Program
    = Program [Declaration] [Statement]
    deriving (Eq, Read, Show)

type Ident = Text

instance HasPrecedence BinaryOp where
    precedence e = case e of
        Plus -> 3
        Minus -> 3
        Times -> 4
        Divide -> 4

instance HasPrecedence UnaryOp where
    precedence e = case e of
        Negative -> 5

instance Pretty BinaryOp where
    pretty e = case e of
        Plus -> "+"
        Minus -> "-"
        Times -> "*"
        Divide -> "/"

instance Pretty UnaryOp where
    pretty e = case e of
        Negative -> "-"

instance Pretty Literal where
    pretty e = case e of
        Variable t -> pretty t
        Int n -> pretty n
        Real x -> pretty x
        String t -> pretty t

instance Pretty Type where
    pretty e = case e of
        TyReal -> "float"
        TyInt -> "int"
        TyString -> "string"

instance Pretty Expr where
    prettysPrec d e = case e of
        BinaryOp op l r ->
            showParen (precedence op < d) (prettyInfix op l r)
        UnaryOp op p ->
            showParen (precedence op < d) (prettyPrefix op p)
        Literal l ->
            prettys l

instance Pretty Statement where
    prettysPrec d e = case e of
        Assign i ex ->
            indent d.
            prettys i .
            prettySpace .
            prettyString "=" .
            prettySpace .
            prettys ex .
            prettyString ";\n"
        While ex body ->
            indent d . prettyString "while " . prettys ex . prettyString " do\n" .
            foldr (.) id (map (prettysPrec $ d + 1) body) .
            prettyString "end\n"
        If ex body1 body2 ->
            indent d . prettyString "if " . prettys ex . prettyString " then\n" .
            foldr (.) id (map (prettysPrec $ d + 1) body1) .
            (if (not . null) body2
                then
                    prettyString " else " .
                    foldr (.) id (map (prettysPrec $ d + 1) body2)
                else
                    id) .
            indent d . prettyString " endif\n"
        Print ex ->
            indent d .
            prettyString "print " .
            prettys ex .
            prettyString ";\n"
        Read ex ->
            indent d . prettyString "read " .
            prettys ex .
            prettyString ";\n"

instance Pretty Declaration where
    prettysPrec d e = case e of
        Var i t ->
            indent d .
            prettyString "var " .
            prettys i .
            prettyString " : " .
            prettys t .
            prettyString ";\n"

instance Pretty Program where
    prettysPrec _ e = case e of
        Program decls stmts ->
            prettyList decls .
            prettyString "\n" .
            prettyList stmts
