{-|
Module      : Language.Oatlab.Analysis.ReachingDefinitions
Description : Reaching definitions analysis using McFAS
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

module Language.Oatlab.Analysis.ReachingDefinitions where

import Data.Annotation
import Data.HFunctor
import Language.Common.Analysis
import Language.Oatlab.Analysis
import Language.Oatlab.Syntax

import Control.Monad.State
import qualified Data.Set as S

-- | A number assigned to a statement. Used to uniquely identify statements.
newtype StatementNumber
  = StatementNumber
    { unStatementNumber :: Int
    }
  deriving (Eq, Ord)

-- | The approximation used in the reaching definitions analysis.
type ReachingDefinitionsApprox = S.Set (StatementNumber, String)

-- | The annotation used in reaching definitions analysis.
data ReachingDefinitions
  = ReachingDefinitions
    { rdRemainingIterations :: Int
    , rdApproximation :: ReachingDefinitionsApprox
    , rdStatementNumber :: StatementNumber
    }

-- | Annotate 'StatementNode' with 'ReachingDefinitions' and everything else
-- with a trivial annotation.
--
-- /Node annotation index interpretation/
type family ReachingDefinitionsP' (node :: AstNode) :: * where
  ReachingDefinitionsP' 'StatementNode = ReachingDefinitions
  ReachingDefinitionsP' _ = ()

-- | Newtype simulation for 'ReachingDefinitionsP'.
newtype ReachingDefinitionsP (node :: AstNode)
  = ReachingDefinitionsP { unReachingDefinitionsP :: ReachingDefinitionsP' node }

-- | Construct an initial reaching definitions annotations.
initialReachingDefinitions :: StatementNumber -> ReachingDefinitions
initialReachingDefinitions number
  = ReachingDefinitions
    { rdRemainingIterations = 100
    , rdApproximation = S.empty
    , rdStatementNumber = number
    }

-- | Replace any existing index-annotations on a tree with the initial state
-- for a reaching definitions analysis.
--
-- The input tree must have its statements numbered; see 'numberStatementAlg'.
--
-- /Higher-order F-algebra/
initializeReachingDefinitionsAlg
  :: IAnn StatementNumberingP OatlabAstF f
  :~> IAnn ReachingDefinitionsP OatlabAstF f
initializeReachingDefinitionsAlg = reannotate phi where
  phi
    :: forall (f :: AstNode -> *) (a :: AstNode).
    OatlabAstF f a
    -> StatementNumberingP a
    -> ReachingDefinitionsP a
  phi node a = case collapseIndex node of
    StatementNodeS
      -> ReachingDefinitionsP (initialReachingDefinitions (unStatementNumberingP a))
    ProgramDeclNodeS -> ReachingDefinitionsP ()
    TopLevelDeclNodeS -> ReachingDefinitionsP ()
    VarDeclNodeS -> ReachingDefinitionsP ()
    ExpressionNodeS -> ReachingDefinitionsP ()
    IdentifierNodeS -> ReachingDefinitionsP ()

-- | Node annotation index interpretation that annotates 'StatementNode' with
-- 'StatemenetNumber' and all other indices with the trivial annotation @()@.
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
    => OatlabAstF f a
    -> p a
    -> m (StatementNumberingP a)
  phi node _ = case collapseIndex node of
    StatementNodeS -> StatementNumberingP . StatementNumber <$> nextInt
    ProgramDeclNodeS -> pure $ StatementNumberingP ()
    TopLevelDeclNodeS -> pure $ StatementNumberingP ()
    VarDeclNodeS -> pure $ StatementNumberingP ()
    ExpressionNodeS -> pure $ StatementNumberingP ()
    IdentifierNodeS -> pure $ StatementNumberingP ()

-- | Annotate a bare syntax tree with the initial configuration for a reaching
-- definitions analysis.
--
-- /Monadic tree rewrite/
initializeReachingDefinitions
  :: forall (m :: * -> *) (a :: AstNode).
    (Monad m, MonadState Int m)
  => OatlabAst a
  -> MonadH m (OatlabIAnnAst ReachingDefinitionsP) a
initializeReachingDefinitions = hcata phi where
  phi
    :: OatlabAstF (MonadH m (OatlabIAnnAst ReachingDefinitionsP)) b
    -> MonadH m (OatlabIAnnAst ReachingDefinitionsP) b
  phi = MonadH
    . fmap HFix
    . join
    . fmap (sequenceH . initializeReachingDefinitionsAlg)
    . numberStatementAlg
    . IAnn (K ())

reachingDefinitionsDataflow
  :: Monad m
  => IAnn ReachingDefinitionsP OatlabAstF (OatlabIAnnAst ReachingDefinitionsP) a
  -- ^ Node in the syntax tree
  -> ReachingDefinitionsApprox -- ^ input flow set
  -> m ReachingDefinitionsApprox -- ^ output flow set
reachingDefinitionsDataflow (IAnn a node) approx = pure $ case node of
  ProgramDecl _ -> approx
  FunctionDecl _ _ _ -> approx
  WhileLoop _ _ -> approx
  ForLoop _ _ _ -> approx
  Branch _ _ _ -> approx
  Return _ -> approx
  Assignment lhs rhs -> approx
  Expression _ -> approx
  Var _ -> approx
  StringLiteral _ -> approx
  NumericLiteral _ -> approx
  BinaryOperation _ _ _ -> approx
  Call _ _ -> approx
  Identifier _ -> approx

reachingDefinitions
  :: Monad m
  => OatlabAnalysis
    (OatlabIAnnAst ReachingDefinitionsP)
    ReachingDefinitionsP
    ReachingDefinitionsApprox
    'StatementNode
    m
    'Forward
reachingDefinitions = Analysis
  { analysisMerge = \x y -> pure (S.union x y)
  , analysisDataflow = reachingDefinitionsDataflow
  , analysisApproximationEq = (==)
  , analysisGetApproximation = \(ReachingDefinitionsP a) -> _
  , analysisGetIterations = \(ReachingDefinitionsP a) -> _
  }
