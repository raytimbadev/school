{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Minilang.Typecheck where

import Language.Minilang.SrcAnn
import Language.Minilang.Syntax

import Control.Monad.Identity
import Control.Monad.Except
import Data.Functor.Foldable
import qualified Data.Map.Strict as M

type Env = M.Map Ident Type

data SemanticError a
    = TypeMismatch
        { gotType :: Type
        , expectedTypes :: [Type]
        }
    | UnsupportedType
        { gotType :: Type
        , supportedTypes :: [Type]
        }
    | FreeVariableError
        { freeVariable :: Ident
        }
    deriving (Functor)

deriving instance Show (SemanticError a)

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

            let (Fix (Ann (_, t1) _)) = ft1
            let (Fix (Ann (_, t2) _)) = ft2

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
                    _ -> throwError
                        (Ann
                            pos
                            (TypeMismatch
                                { gotType = t2, expectedTypes = [t1] }))
                Minus -> case (t1, t2) of
                    (TyInt, TyInt) -> conclude TyInt
                    (TyReal, TyReal) -> conclude TyReal
                    (TyString, TyString) -> conclude TyString
                    _ -> throwError
                        (Ann
                            pos
                            (TypeMismatch
                                { gotType = t2, expectedTypes = [t1] }))
                Times -> case (t1, t2) of
                    (TyInt, TyInt) -> conclude TyInt
                    (TyInt, TyReal) -> conclude TyReal
                    (TyReal, TyInt) -> conclude TyReal
                    (TyReal, TyReal) -> conclude TyReal
                    (TyString, _) -> throwError
                        (Ann
                            pos
                            (UnsupportedType
                                { gotType = t2, supportedTypes = [] }))
                    (_, TyString) -> throwError
                        (Ann
                            pos
                            (UnsupportedType
                                { gotType = t2, supportedTypes = [TyInt, TyReal] }))
                Divide -> case (t1, t2) of
                    (TyInt, TyInt) -> conclude TyInt
                    (TyInt, TyReal) -> conclude TyReal
                    (TyReal, TyInt) -> conclude TyReal
                    (TyReal, TyReal) -> conclude TyReal
                    (TyString, _) -> throwError
                        (Ann
                            pos
                            (UnsupportedType
                                { gotType = t2, supportedTypes = [] }))
                    (_, TyString) -> throwError
                        (Ann
                            pos
                            (UnsupportedType
                                { gotType = t2, supportedTypes = [TyInt, TyReal] }))

        Literal ml -> do
            let (runIdentity . bare -> l) = ml

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
