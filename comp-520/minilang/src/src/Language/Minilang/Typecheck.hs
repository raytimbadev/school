{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Minilang.Typecheck
( -- * Symbol tables
  Env
, symbolTableTsv
, symbolTableFrom
  -- * Typechecking
, TySrcAnn
, TySrcAnnFix
  -- ** Programs
, TySrcAnnProgram
, typecheckProgram
  -- ** Statements
, TySrcAnnStatement
, typecheckStmt
, typecheckStmts
  -- ** Expressions
, TySrcAnnExpr
, typecheckExpr
, tyExprType
, tyExprSrcSpan
  -- ** Identifiers
, TySrcAnnIdent
, tyIdentType
, tyIdentName
, tyIdentSrcSpan
  -- * Error handling
, SemanticError(..)
) where

import Language.Minilang.Pretty
import Language.Minilang.SrcAnn
import Language.Minilang.Syntax

import Control.Monad.Identity
import Control.Monad.Except
import Data.Functor.Foldable
import Data.Function ( on )
import Data.List ( sortBy, groupBy )
import qualified Data.Map.Strict as M
import Data.Ord ( comparing )
import Text.PrettyPrint

-- | Symbol table.
type Env = M.Map Ident Type

-- | A "Type" and "SrcSpan"-annotated functor value.
type TySrcAnn = Ann (SrcSpan, Type)

-- | A "Type" and "SrcSpan"-annotated functor fixed point.
type TySrcAnnFix f = AnnFix (SrcSpan, Type) f

-- | A "Type" and "SrcSpan"-annotated identifier, wrapped in an "Identity"
-- functor.
type TySrcAnnIdent = TySrcAnn Identity Ident

-- | A "Type" and "SrcSpan"-annotated expression tree.
type TySrcAnnExpr = AnnFix (SrcSpan, Type) SrcAnnExprF

-- | A "Type" and "SrcSpan"-annotated statement tree.
--
-- Statements themselves do not have types, so the branches of this fixed point
-- are only annotated with source spans. The identifiers and expressions that
-- appear in the statement are however typechecked, so they are annotated with
-- both types and source spans.
type TySrcAnnStatement = SrcAnnFix (StatementF TySrcAnnIdent TySrcAnnExpr)

-- | Typechecked program, i.e. a program in which statements have been
-- typechecked using the program's declarations as a symbol table.
type TySrcAnnProgram = Program SrcAnnDeclaration TySrcAnnStatement

-- | Semantic errors that occur during typechecking.
data SemanticError a
    = TypeMismatch
        { gotType :: Type
        , expectedTypes :: [Type]
        }
    -- ^ The pair of types involved in a binary operator is wrong. This error
    -- occurs specifically when the operator supports the type of the first
    -- operand, but not in conjunction with the type of the second operand.
    --
    -- The type of the first operand determines what are the valid types of the
    -- second operand. The error data includes this information in order to
    -- provide better error messages.
    | UnsupportedType
        { gotType :: Type
        , supportedTypes :: [Type]
        }
    -- ^ The pair of types involved in a binary operator is wrong. This error
    -- occurs specifically when the operator does not support the type of the
    -- first operand at all, and so the type of the second operator is
    -- irrelevant.
    | FreeVariableError
        { freeVariable :: Ident
        }
    -- ^ A free variable is found in the expression, so its type cannot be
    -- determined.
    | RedeclaredVariable
        { firstOccurrence :: Ident
        , redundantOccurrences :: [SrcAnnIdent]
        , envAnyway :: Env
        }
    -- ^ A variable is declared twice.
    deriving (Functor)

deriving instance Show (SemanticError a)

instance Pretty (SemanticError a) where
    pretty e = case e of
        TypeMismatch { gotType = t, expectedTypes = ts } ->
            text "Type mismatch." $+$
            nest 4 (
                text "Got" <+> pretty t $+$
                if null ts
                    then text "But no type is valid here."
                    else text "Expected:" <+> sep (punctuate comma (map pretty ts))
            )
        UnsupportedType { gotType = t, supportedTypes = ts } ->
            text "Unsupported type in operator." $+$
            nest 4 (
                text "Got" <+> pretty t $+$
                if null ts
                    then text "But no types are supported by the operator."
                    else text "Supported types:" <+> sep (punctuate comma (map pretty ts))
            )
        FreeVariableError { freeVariable = v } ->
            text "Unbound variable." $+$
            nest 4 (
                text "The variable" <+> pretty v <+> text "has no declaration."
            )
        RedeclaredVariable { firstOccurrence = x, redundantOccurrences = xs } ->
            text "Redeclaration of variables." $+$
            nest 4 (
                text "The variable" <+> pretty x <+> text "is redeclared." $+$
                vcat (map (pretty . srcStart . ann) xs)
            )

instance Pretty (Ann SrcSpan SemanticError a) where
    pretty (Ann pos e) = hang (pretty $ srcStart pos) 4 (pretty e)

typecheckProgram :: MonadError (SrcAnn SemanticError a) m
                 => SrcAnnProgram -> m TySrcAnnProgram
typecheckProgram (Program decls stmts) = do
    -- build the symbol table
    env <- symbolTableFrom decls

    -- typecheck the statements
    stmts' <- typecheckStmts env stmts

    -- produce the typechecked program
    pure (Program decls stmts')

-- | Converts a symbol table to a tab-separated value string.
symbolTableTsv :: Env -> String
symbolTableTsv = invoke . foldr line id . M.toList where
    line (i, t) b = shows i . showString "\t" . shows t . showString "\n" . b
    invoke ss = ss ""

-- | Builds a symbol table from a list of declarations.
symbolTableFrom :: MonadError (SrcAnn SemanticError a) m
                => [SrcAnnDeclaration] -> m Env
symbolTableFrom decls = do
    let ident (bareId -> (Var (bareId -> i) _)) = i
    let annIdent (bareId -> (Var i _)) = i

    let decls' = sortBy (comparing ident) decls
    let decls'' = groupBy ((==) `on` ident) decls'

    let f (bareId -> (Var (bareId -> i) (bareId -> t))) = (i, t)
    let env = M.fromList (map f decls)

    -- check that there are no redeclarations
    forM_ decls'' $ \declGrp -> case declGrp of
        [] -> error "groupBy internal invariant broken"
        [_] -> pure () -- ok
        (x:xs) -> throwError
            (Ann
                (ann (annIdent x))
                (RedeclaredVariable
                    { firstOccurrence = ident x
                    , redundantOccurrences = annIdent <$> xs
                    , envAnyway = env
                    }))

    pure env

-- | Gets the top-level type of a typechecked expression.
tyExprType :: TySrcAnnExpr -> Type
tyExprType (Fix (Ann (_, ty) _)) = ty

-- | Gets the source span of a typechecked expression.
tyExprSrcSpan :: TySrcAnnExpr -> SrcSpan
tyExprSrcSpan (Fix (Ann (pos, _) _)) = pos

-- | Typechecks an identifier in a given environment, raising a
-- "FreeVariableError" if the variable cannot be found.
typecheckIdent :: MonadError (SrcAnn SemanticError a) m
               => Env -> SrcAnnIdent -> m TySrcAnnIdent
typecheckIdent env (Ann pos i) = case M.lookup (runIdentity i) env of
    Just t -> pure (Ann (pos, t) i)
    Nothing -> throwError
        (Ann
            pos
            (FreeVariableError
                { freeVariable = runIdentity i }))

-- | Extract the source span of a typechecked identifier.
tyIdentSrcSpan :: TySrcAnnIdent -> SrcSpan
tyIdentSrcSpan = fst . ann

-- | Extract the type of a typechecked identifier.
tyIdentType :: TySrcAnnIdent -> Type
tyIdentType = snd . ann

-- | Extract the name of a typechecked identifier.
tyIdentName :: TySrcAnnIdent -> Ident
tyIdentName = bareId

-- | Typecheck a statement, i.e. typecheck any expressions occurring in the
-- statement and typecheck any bare identifiers.
typecheckStmt
    :: MonadError (SrcAnn SemanticError a) m
    => Env -> SrcAnnStatement -> m TySrcAnnStatement
typecheckStmt env = cata f where
    f :: MonadError (SrcAnn SemanticError a) m
      => SrcAnn SrcAnnStatementF (m TySrcAnnStatement) -> m TySrcAnnStatement
    f (Ann pos me) = case me of -- me :: f (m TySrcAnnStatement)
        Assign i e -> do
            ity <- typecheckIdent env i -- typecheck the identifier
            ety <- typecheckExpr env e -- typecheck the expression

            -- a helper for valid statements
            let ok = pure (Fix (Ann pos (Assign ity ety)))

            -- a helper for invalid statements
            let die x = throwError (Ann (tyExprSrcSpan ety) x)

            -- check that the type of the identifier to assign to
            -- matches the computed type of the expression
            case (tyIdentType ity, tyExprType ety) of
                (TyInt, TyInt) -> ok

                (TyInt, TyReal) -> die $
                    TypeMismatch
                        { gotType = TyReal
                        , expectedTypes = [TyInt]
                        }

                (TyInt, TyString) -> die $
                    TypeMismatch
                        { gotType = TyString
                        , expectedTypes = [TyInt]
                        }

                (TyReal, TyInt) -> ok

                (TyReal, TyReal) -> ok

                (TyReal, TyString) -> die $
                    TypeMismatch
                        { gotType = TyString
                        , expectedTypes = [TyInt, TyReal]
                        }

                (TyString, TyInt) -> die $
                    TypeMismatch
                        { gotType = TyInt
                        , expectedTypes = [TyString]
                        }

                (TyString, TyReal) -> die $
                    TypeMismatch
                        { gotType = TyReal
                        , expectedTypes = [TyString]
                        }

                (TyString, TyString) -> ok

        While e ss -> do
            ety <- typecheckExpr env e
            let ty = tyExprType ety
            let epos = tyExprSrcSpan ety

            case ty of
                TyInt -> pure ()
                _ -> throwError
                    (Ann
                        epos
                        (UnsupportedType
                            { gotType = ty
                            , supportedTypes = [TyInt]
                            }))

            ss' <- sequence ss

            pure (Fix (Ann pos (While ety ss')))

        If e tb eb -> do
            e' <- typecheckExpr env e

            let (Fix (Ann (epos, ety) _)) = e'

            case ety of
                TyInt -> pure()
                _ -> throwError
                    (Ann
                        epos
                        (UnsupportedType
                            { gotType = ety
                            , supportedTypes = [TyInt]
                            }))

            tb' <- sequence tb
            eb' <- sequence eb

            pure (Fix (Ann pos (If e' tb' eb')))

        Print e -> do
            e' <- typecheckExpr env e

            pure (Fix (Ann pos (Print e')))

        Read i -> do
            i' <- typecheckIdent env i

            pure (Fix (Ann pos (Read i')))

-- | Typechecks a block of statements with a given symbol table.
typecheckStmts :: MonadError (SrcAnn SemanticError a) m
               => Env -> [SrcAnnStatement] -> m [TySrcAnnStatement]
typecheckStmts env = mapM (typecheckStmt env)

-- | Typechecks an expression tree, annotating every level with type
-- information. The typecheck proceeds inductively from the bottom-up, and is
-- performed in a monad supporting failure via the "SemanticError" type.
--
-- The type of literals is obvious or can be looked up in a symbol table. The
-- type of recursive expressions such as operators is determined according to
-- the types of the operands and the Minilang typing rules.
--
-- In Minilang, numeric types are liberal; integers are widened to floats when
-- a binary operators involves operands of both types. Strings are trickier.
-- The semantics of Minilang are that strings may be negated and added. This
-- leaves also an obvious interpretation of string subtraction. Strings cannot
-- be involved in any operation that also involves numeric types.
typecheckExpr :: (Monad m, MonadError (SrcAnn SemanticError a) m)
              => Env -> SrcAnnExpr -> m (AnnFix (SrcSpan, Type) SrcAnnExprF)
typecheckExpr env fe = cata f fe where
    -- f :: Ann SrcSpan SrcAnnExprF (m (AnnFix (SrcSpan, Type) SrcAnnExprF))
    --   -> m (Fix (Ann (SrcSpan, Type) SrcAnnExprF))
    f (Ann pos me) = case me of
        BinaryOp bin mt1 mt2 -> do
            let b = runIdentity (bare bin)
            ft1 <- mt1
            ft2 <- mt2

            let (Fix (Ann (pt1, t1) _)) = ft1
            let (Fix (Ann (pt2, t2) _)) = ft2

            -- define a helper function to construct the new branch in the tree
            -- with the added type annotation.
            let conclude :: Applicative m
                         => Type -> m (Fix (Ann (SrcSpan, Type) SrcAnnExprF))
                conclude rt = pure (Fix (Ann (pos, rt) (BinaryOp bin ft1 ft2)))

            case b of
                Plus -> case (t1, t2) of
                    (TyInt, TyInt) -> conclude TyInt
                    (TyInt, TyReal) -> conclude TyReal
                    (TyReal, TyInt) -> conclude TyReal
                    (TyReal, TyReal) -> conclude TyReal
                    (TyString, TyString) -> conclude TyString
                    (TyString, _) -> throwError
                        (Ann
                            pt2
                            (TypeMismatch
                                { gotType = t2, expectedTypes = [t1] }))
                    (_, TyString) -> throwError
                        (Ann
                            pt2
                            (TypeMismatch
                                { gotType = t2, expectedTypes = [TyInt, TyReal] }))
                Minus -> case (t1, t2) of
                    (TyInt, TyInt) -> conclude TyInt
                    (TyInt, TyReal) -> conclude TyReal
                    (TyReal, TyInt) -> conclude TyReal
                    (TyReal, TyReal) -> conclude TyReal
                    (TyString, TyString) -> conclude TyString
                    (TyString, _) -> throwError
                        (Ann
                            pt2
                            (TypeMismatch
                                { gotType = t2, expectedTypes = [t1] }))
                    (_, TyString) -> throwError
                        (Ann
                            pt2
                            (TypeMismatch
                                { gotType = t2, expectedTypes = [TyInt, TyReal] }))
                Times -> case (t1, t2) of
                    (TyInt, TyInt) -> conclude TyInt
                    (TyInt, TyReal) -> conclude TyReal
                    (TyReal, TyInt) -> conclude TyReal
                    (TyReal, TyReal) -> conclude TyReal
                    (TyString, _) -> throwError
                        (Ann
                            pt1
                            (UnsupportedType
                                { gotType = t2, supportedTypes = [] }))
                    (_, TyString) -> throwError
                        (Ann
                            pt2
                            (UnsupportedType
                                { gotType = t2, supportedTypes = [TyInt, TyReal] }))
                Divide -> case (t1, t2) of
                    (TyInt, TyInt) -> conclude TyInt
                    (TyInt, TyReal) -> conclude TyReal
                    (TyReal, TyInt) -> conclude TyReal
                    (TyReal, TyReal) -> conclude TyReal
                    (TyString, _) -> throwError
                        (Ann
                            pt1
                            (UnsupportedType
                                { gotType = t2, supportedTypes = [] }))
                    (_, TyString) -> throwError
                        (Ann
                            pt2
                            (UnsupportedType
                                { gotType = t2, supportedTypes = [TyInt, TyReal] }))

        Literal ml -> do
            let (bareId -> l) = ml

            let conclude rt = pure (Fix (Ann (pos, rt) (Literal ml)))

            case l of
                Variable i -> do
                    let rt = M.lookup i env
                    case rt of
                        Nothing -> throwError
                            (Ann
                                pos
                                (FreeVariableError
                                    { freeVariable = i }))
                        Just t -> conclude t
                Int _ -> conclude TyInt
                Real _ -> conclude TyReal
                String _ -> conclude TyString

        UnaryOp un mt -> do
            ft <- mt

            let (Fix (Ann (_, t) _)) = ft

            let conclude rt = pure (Fix (Ann (pos, rt) (UnaryOp un ft)))

            -- unary operators just preserve the underlying type
            conclude t
