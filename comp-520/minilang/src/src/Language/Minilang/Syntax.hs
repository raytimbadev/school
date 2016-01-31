{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Minilang.Syntax where

import Language.Minilang.Precedence
import Language.Minilang.Pretty
import Language.Minilang.SrcAnn

import Data.Functor.Identity
import Data.Functor.Foldable
import Prelude hiding ( print, read )
import qualified Prelude as P
import Data.Text ( Text )
import Text.PrettyPrint

data StatementF ident expr f
    = Assign ident expr
    | While expr [f]
    | If expr [f] [f]
    | Print expr
    | Read ident
    deriving (Eq, Read, Show, Functor, P.Foldable, Traversable)

type SrcAnnStatementF
    = StatementF SrcAnnIdent SrcAnnExpr

type SrcAnnStatement
    = SrcAnnFix SrcAnnStatementF

type BasicStatementF
    = StatementF Ident BasicExpr

type BasicStatement
    = Fix BasicStatementF

unannotateStmt
    :: SrcAnnStatement
    -> BasicStatement
unannotateStmt fe = cata f fe where
    f :: Ann SrcSpan SrcAnnStatementF BasicStatement -> BasicStatement
    f (Ann _ ex) = Fix $ case ex of
        Assign i e -> Assign (runIdentity $ bare i) (unannotateExpr e)
        While e stmts -> While (unannotateExpr e) stmts
        If e thens elses -> If (unannotateExpr e) thens elses
        Print e -> Print (unannotateExpr e)
        Read i -> Read (runIdentity $ bare i)

data Declaration ident ty
    = Var ident ty
    deriving (Eq, Read, Show)

type BasicDeclaration = Declaration Ident Type

type SrcAnnDeclaration
    = SrcAnn Identity (Declaration SrcAnnIdent SrcAnnType)

unannotateDecl
    :: SrcAnnDeclaration
    -> BasicDeclaration
unannotateDecl de = case de of
    Ann _ e -> case runIdentity e of
        Var i t -> Var (runIdentity $ bare i) (runIdentity $ bare t)

data ExprF binaryOp unaryOp literal f
    = BinaryOp binaryOp f f
    | UnaryOp unaryOp f
    | Literal literal
    deriving (Eq, Read, Show, Functor, P.Foldable, Traversable)

type BasicExprF
    = ExprF BinaryOp UnaryOp Literal

type SrcAnnExprF
    = ExprF SrcAnnBinaryOp SrcAnnUnaryOp SrcAnnLiteral

type SrcAnnExpr
    = SrcAnnFix SrcAnnExprF

type BasicExpr
    = Fix BasicExprF

unannotateExpr :: SrcAnnExpr -> BasicExpr
unannotateExpr fe = cata f fe where
    f :: Ann SrcSpan SrcAnnExprF BasicExpr -> BasicExpr
    f (Ann _ e) = Fix $ case e of
        BinaryOp o e1 e2 -> BinaryOp (runIdentity $ bare o) e1 e2
        UnaryOp o e' -> UnaryOp (runIdentity $ bare o) e'
        Literal l -> Literal (runIdentity $ bare l)

data BinaryOp
    = Plus | Minus | Times | Divide
    deriving (Eq, Read, Show)

type SrcAnnBinaryOp
    = SrcAnn Identity BinaryOp

data UnaryOp
    = Negative
    deriving (Eq, Read, Show)

type SrcAnnUnaryOp
    = SrcAnn Identity UnaryOp

data Literal
    = Variable Ident
    | Int Int
    | Real Double
    | String Text
    deriving (Eq, Read, Show)

type SrcAnnLiteral
    = SrcAnn Identity Literal

data Type
    = TyReal
    | TyInt
    | TyString
    deriving (Eq, Read, Show)

type SrcAnnType = SrcAnn Identity Type

data Program decl stmt
    = Program [decl] [stmt]
    deriving (Eq, Read, Show)

type BasicProgram
    = Program BasicDeclaration BasicStatement

-- | A program with source code annotations all over the place.
type SrcAnnProgram
    = Program
        SrcAnnDeclaration
        SrcAnnStatement

type Ident = Text

type SrcAnnIdent = SrcAnn Identity Ident

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
    prettysPrec _ e = case e of
        Variable t -> prettys t
        Int n -> prettys n
        Real x -> prettys x
        String t -> prettyString "\"" . prettys t . prettyString "\""

instance Pretty Type where
    pretty e = case e of
        TyReal -> "float"
        TyInt -> "int"
        TyString -> "string"

instance
    ( Pretty bin
    , Pretty un
    , Pretty lit
    , HasPrecedence un
    , HasPrecedence bin
    ) => Pretty (Fix (ExprF bin un lit)) where

    prettysPrec _ fe = snd $ cata f fe where
        f e = case e of
            BinaryOp op (dl, l) (dr, r) ->
                let
                    p = precedence op
                    lb = showParen (dl < p) l
                    lr = showParen (dr < p) r
                    body = lb . prettySpace . prettys op . prettySpace . lr
                in
                    (p, body)

            UnaryOp op (dm, m) ->
                let
                    p = precedence op
                    lm = showParen (dm < p) m
                    body = prettys op . lm
                in
                    (p, body)

            Literal l ->
                (10, prettys l)

indentWidth :: Int
indentWidth = 4

instance
    ( Pretty i
    , Pretty e
    ) => Pretty (Fix (StatementF i e)) where

    pretty fe = render $ cata f fe where
        tp :: Pretty a => a -> Doc
        tp = text . pretty
        f e = case e of
            Assign i ex -> tp i <+> char '=' <+> tp ex <> semi
            While ex body ->
                text "while" <+> tp ex <+> text "do" $+$
                nest indentWidth (vcat body) $+$
                text "end"
            If ex body1 body2 ->
                text "if" <+> tp ex <+> text "then" $+$
                nest indentWidth (vcat body1) $+$
                (if (not . null) body2
                    then
                        text "else" $+$
                        nest indentWidth (vcat body2)
                    else
                        empty) $+$
                text "endif"
            Print ex ->
                text "print" <+> tp ex <> semi
            Read ex ->
                text "read" <+> tp ex <> semi

    prettyList ls
        = foldr (\a b -> a . showString "\n" . b) id (map prettys ls)

instance (Pretty ident, Pretty ty) => Pretty (Declaration ident ty) where
    prettysPrec d e = case e of
        Var i t ->
            indent d .
            prettyString "var " .
            prettys i .
            prettyString " : " .
            prettys t .
            prettyString ";\n"

instance (Pretty decl, Pretty stmt) => Pretty (Program decl stmt) where
    prettysPrec _ e = case e of
        Program decls stmts ->
            prettyList decls .
            prettyString "\n" .
            prettyList stmts
