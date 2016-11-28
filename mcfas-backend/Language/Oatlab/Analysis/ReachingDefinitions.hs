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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Oatlab.Analysis.ReachingDefinitions where

import Data.Annotation
import Data.HFunctor
import Data.Reflection
import Language.Common.Analysis
import Language.Oatlab.Analysis
import Language.Oatlab.Analysis.StatementNumbering
import Language.Oatlab.Syntax
import Language.Oatlab.Pretty

import Control.Monad.State
import Data.Proxy
import qualified Data.Map as M
import qualified Data.Set as S

addDefinition
  :: String
  -> StatementNumber
  -> M.Map String (S.Set StatementNumber)
  -> M.Map String (S.Set StatementNumber)
addDefinition s n = M.alter f s where
  f :: Maybe (S.Set StatementNumber) -> Maybe (S.Set StatementNumber)
  f = Just . S.insert n . maybe S.empty id

type family ReachingDefinitionsApprox (node :: AstNode) :: * where
  ReachingDefinitionsApprox 'StatementNode
    = M.Map String (S.Set StatementNumber)
  ReachingDefinitionsApprox _ = ()

-- | The approximation used in the reaching definitions analysis.
newtype ReachingDefinitionsApproxP (node :: AstNode)
  = ReachingDefinitionsApproxP
    { unReachingDefinitionsApproxP :: ReachingDefinitionsApprox node
    }

-- | The annotation used in reaching definitions analysis.
data ReachingDefinitions (node :: AstNode)
  = ReachingDefinitions
    { rdRemainingIterations :: Int
    , rdApproximation :: ReachingDefinitionsApproxP node
    , rdStatementNumber :: StatementNumber
    }

deriving instance ReflectS a => Eq (ReachingDefinitions a)

-- | Annotate 'StatementNode' with 'ReachingDefinitions' and everything else
-- with a trivial annotation.
--
-- /Node annotation index interpretation/
type family ReachingDefinitionsP' (node :: AstNode) :: * where
  ReachingDefinitionsP' 'StatementNode = ReachingDefinitions 'StatementNode
  ReachingDefinitionsP' _ = ()

-- | Newtype simulation for 'ReachingDefinitionsP'.
newtype ReachingDefinitionsP (node :: AstNode)
  = ReachingDefinitionsP { unReachingDefinitionsP :: ReachingDefinitionsP' node }

instance ReflectS a => Eq (ReachingDefinitionsP a) where
  ReachingDefinitionsP x == ReachingDefinitionsP y = case reflectS (Proxy :: Proxy a) of
    ProgramDeclNodeS -> x == y
    TopLevelDeclNodeS -> x == y
    VarDeclNodeS -> x == y
    StatementNodeS -> x == y
    ExpressionNodeS -> x == y
    IdentifierNodeS -> x == y

instance ReflectS a => Eq (ReachingDefinitionsApproxP a) where
  ReachingDefinitionsApproxP x == ReachingDefinitionsApproxP y
    = case reflectS (Proxy :: Proxy a) of
      ProgramDeclNodeS -> x == y
      TopLevelDeclNodeS -> x == y
      VarDeclNodeS -> x == y
      StatementNodeS -> x == y
      ExpressionNodeS -> x == y
      IdentifierNodeS -> x == y

-- | Construct an initial reaching definitions annotations.
initialReachingDefinitions :: StatementNumber -> ReachingDefinitions 'StatementNode
initialReachingDefinitions number
  = ReachingDefinitions
    { rdRemainingIterations = 100
    , rdApproximation = ReachingDefinitionsApproxP M.empty
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
  phi node a = ReachingDefinitionsP $ case collapseIndex node of
    StatementNodeS -> initialReachingDefinitions (unStatementNumberingP a)
    ProgramDeclNodeS -> ()
    TopLevelDeclNodeS -> ()
    VarDeclNodeS -> ()
    ExpressionNodeS -> ()
    IdentifierNodeS -> ()

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
  :: forall (m :: * -> *) (a :: AstNode). (Monad m, ReflectS a)
  => IAnn ReachingDefinitionsP OatlabAstF (OatlabIAnnAst ReachingDefinitionsP) a
  -- ^ Node in the syntax tree
  -> ReachingDefinitionsApproxP a -- ^ input flow set
  -> m (ReachingDefinitionsApproxP a) -- ^ output flow set
reachingDefinitionsDataflow (IAnn a node) approx
  = pure $ case reflectS (Proxy :: Proxy a) of
    StatementNodeS ->
      let (ReachingDefinitionsP (ReachingDefinitions {..})) = a
      in case node of
        WhileLoop _ _ -> approx
        ForLoop var _ _
          -> ReachingDefinitionsApproxP $
            addDefinition
              (nameFromVarDecl (stripI var))
              rdStatementNumber
              (unReachingDefinitionsApproxP rdApproximation)
        Branch _ _ _ -> approx
        Return _ -> approx
        Assignment lhs _
          | unK (hcata (isLValueAlg . bareI) lhs)
            -> ReachingDefinitionsApproxP $
              M.insert
                (renderOatlab (unK $ hcata (ppAlg . bareI) lhs))
                (S.singleton rdStatementNumber)
                (unReachingDefinitionsApproxP rdApproximation)
        Assignment _ _ -> approx
          -- assignment to things that are not lvalues is technically an error
        Expression _ -> approx
    ProgramDeclNodeS -> approx
    TopLevelDeclNodeS -> approx
    VarDeclNodeS -> approx
    ExpressionNodeS -> approx
    IdentifierNodeS -> approx

reachingDefinitions
  :: Monad m
  => OatlabAnalysis
    (OatlabIAnnAst ReachingDefinitionsP)
    ReachingDefinitionsP
    ReachingDefinitionsApproxP
    'StatementNode
    'StatementNode
    m
    'Forward
reachingDefinitions = Analysis
  { analysisMerge
    = \ (ReachingDefinitionsApproxP x :: ReachingDefinitionsApproxP a)
        (ReachingDefinitionsApproxP y :: ReachingDefinitionsApproxP a)
        -> pure $ ReachingDefinitionsApproxP $ case reflectS (Proxy :: Proxy a) of
          StatementNodeS -> M.unionWith S.union x y
          ProgramDeclNodeS -> ()
          TopLevelDeclNodeS -> ()
          VarDeclNodeS -> ()
          ExpressionNodeS -> ()
          IdentifierNodeS -> ()
  , analysisDataflow = reachingDefinitionsDataflow
  , analysisApproximationEq = (==)
  , analysisGetApproximation
    = \ (ReachingDefinitionsP a :: ReachingDefinitionsP a)
      -> case reflectS (Proxy :: Proxy a) of
          StatementNodeS -> rdApproximation a
          ProgramDeclNodeS -> ReachingDefinitionsApproxP ()
          TopLevelDeclNodeS -> ReachingDefinitionsApproxP ()
          VarDeclNodeS -> ReachingDefinitionsApproxP ()
          ExpressionNodeS -> ReachingDefinitionsApproxP ()
          IdentifierNodeS -> ReachingDefinitionsApproxP ()
  , analysisGetIterations
    = \ (ReachingDefinitionsP a) -> rdRemainingIterations a
  , analysisUpdateApproximation
    = \ approx
        (ReachingDefinitionsP a :: ReachingDefinitionsP node)
        -> pure . ReachingDefinitionsP $ case reflectS (Proxy :: Proxy node) of
          StatementNodeS
            -> a { rdApproximation = approx }
          ProgramDeclNodeS -> ()
          TopLevelDeclNodeS -> ()
          VarDeclNodeS -> ()
          ExpressionNodeS -> ()
          IdentifierNodeS -> ()
  , analysisUpdateIterations
    = \ f
        (ReachingDefinitionsP a)
        -> pure $ ReachingDefinitionsP $ a
          { rdRemainingIterations = f (rdRemainingIterations a)
          }
  , analysisBoundaryApproximation = pure (ReachingDefinitionsApproxP M.empty)
  }
