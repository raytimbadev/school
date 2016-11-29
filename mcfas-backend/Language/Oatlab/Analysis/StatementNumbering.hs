{-|
Module      : Language.Oatlab.Analysis.StatementNumbering
Description : A simple analysis that numbers all statements sequentially
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Oatlab.Analysis.StatementNumbering
( StatementNumber(..)
, StatementNumbering
, StatementNumberingP(..)
, numberStatementAlg
) where

import Data.Annotation
import Language.Oatlab.Syntax

import Control.Monad.State

-- | A number assigned to a statement. Used to uniquely identify statements.
newtype StatementNumber = StatementNumber { unStatementNumber :: Int }
  deriving (Eq, Ord)

-- | Node annotation index interpretation that annotates 'StatementNode' with
-- 'StatementNumber' and all other indices with the trivial annotation @()@.
type family StatementNumbering (node :: AstNode) :: * where
  StatementNumbering 'StatementNode = StatementNumber
  StatementNumbering _ = ()

-- | Newtype that simulates the action of the 'StatementNumbering' type family.
newtype StatementNumberingP (node :: AstNode)
  = StatementNumberingP { unStatementNumberingP :: StatementNumbering node }

-- | Increment the value in the state by one and return the value in the state.
nextInt :: MonadState Int m => m Int
nextInt = modify (+1) *> get

-- | Number statements increasingly.
--
-- /Monadic higher-order F-algebra/
numberStatementAlg
  :: (Monad m, MonadState Int m)
  => IAnn p OatlabAstF f a -> m (IAnn StatementNumberingP OatlabAstF f a)
numberStatementAlg = reannotateM phi where
  phi
    :: (Monad m, MonadState Int m)
    => OatlabAstF f a -> p a -> m (StatementNumberingP a)
  phi node _ = case collapseIndex node of
    StatementNodeS -> StatementNumberingP . StatementNumber <$> nextInt
    ProgramDeclNodeS -> pure $ StatementNumberingP ()
    TopLevelDeclNodeS -> pure $ StatementNumberingP ()
    VarDeclNodeS -> pure $ StatementNumberingP ()
    ExpressionNodeS -> pure $ StatementNumberingP ()
    IdentifierNodeS -> pure $ StatementNumberingP ()
