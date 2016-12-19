{-|
Module      : Language.Oatlab.Analysis.ReachingDefinitions
Description : Reaching definitions analysis
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental

TODO use real chunks of AST instead of strings for representing the definitions.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Oatlab.Analysis.ReachingDefinitions
( reachingDefinitions
, initializeReachingDefinitions
-- , initializeReachingDefinitionsAlg
, reachingDefinitionsPpAlg
, ReachingDefinitionsApprox'
, ReachingDefinitionsApprox(..)
) where

import Data.Annotation
import Data.HFunctor
import Data.Reflection
import Language.Common.Analysis
import Language.Oatlab.Analysis.StatementNumbering
import Language.Oatlab.Syntax
import Language.Oatlab.Pretty

import Data.Proxy
import qualified Data.Map as M
import qualified Data.Set as S
import Text.PrettyPrint

-- | Helper for adding a definition to the set of definitions associated with a
-- given expression.
addDefinition
  :: String
  -> StatementNumber
  -> M.Map String (S.Set StatementNumber)
  -> M.Map String (S.Set StatementNumber)
addDefinition s n = M.alter f s where
  f :: Maybe (S.Set StatementNumber) -> Maybe (S.Set StatementNumber)
  f = Just . S.insert n . maybe S.empty id

-- | Type-level function that says that statements are approximated with maps
-- from expressions (represented by strings) to sets of statement numbers
-- (their definiting statements), and that all other AST node are approximated
-- with trivial data.
type family ReachingDefinitionsApprox' (node :: AstNode) :: * where
  ReachingDefinitionsApprox' 'StatementNode
    = M.Map String (S.Set StatementNumber)
  ReachingDefinitionsApprox' _ = ()

-- | The approximation used in the reaching definitions analysis.
--
-- Simulates the action of the type-level function 'ReachingDefinitionsApprox'.
newtype ReachingDefinitionsApprox (node :: AstNode)
  = ReachingDefinitionsApprox
    { unReachingDefinitionsApproxP :: ReachingDefinitionsApprox' node
    }

instance ReflectS a => Show (ReachingDefinitionsApprox a) where
  show (ReachingDefinitionsApprox a) = case reflectS (Proxy :: Proxy a) of
    ProgramDeclNodeS -> show a
    TopLevelDeclNodeS -> show a
    VarDeclNodeS -> show a
    StatementNodeS -> show a
    ExpressionNodeS -> show a
    IdentifierNodeS -> show a

instance ReflectS a => Eq (ReachingDefinitionsApprox a) where
  ReachingDefinitionsApprox x == ReachingDefinitionsApprox y
    = case reflectS (Proxy :: Proxy a) of
      ProgramDeclNodeS -> x == y
      TopLevelDeclNodeS -> x == y
      VarDeclNodeS -> x == y
      StatementNodeS -> x == y
      ExpressionNodeS -> x == y
      IdentifierNodeS -> x == y

initializeReachingDefinitions :: OatlabAstF f node -> ReachingDefinitionsApprox node
initializeReachingDefinitions node = ReachingDefinitionsApprox $ case collapseIndex node of
  StatementNodeS -> M.empty
  ProgramDeclNodeS -> ()
  TopLevelDeclNodeS -> ()
  VarDeclNodeS -> ()
  ExpressionNodeS -> ()
  IdentifierNodeS -> ()

-- | The dataflow equations for the reaching definitions analysis.
--
-- * When a for-loop is encountered, it is considered to define its variable
-- * When an assignment is encounteded, it is considered to define its LHS
reachingDefinitionsDataflow
  :: forall (m :: * -> *) (a :: AstNode). Monad m
  => IAnn StatementNumbering OatlabAstF (OatlabIAnnAst StatementNumbering) a
  -- ^ Node in the syntax tree
  -> ReachingDefinitionsApprox a -- ^ input flow set
  -> m (ReachingDefinitionsApprox a) -- ^ output flow set
reachingDefinitionsDataflow
  (IAnn (StatementNumbering number) node)
  (ReachingDefinitionsApprox approx)
  = pure $ ReachingDefinitionsApprox $ case collapseIndex node of
    StatementNodeS -> case node of
      ForLoop var _ _
        -> addDefinition
            (nameFromVarDecl (stripI var))
            number
            approx
      Assignment lhs _
        | unK (hcata (isLValueAlg . bareI) lhs)
          -> M.insert
              (renderOatlab (unK $ hcata (ppAlg . bareI) lhs))
              (S.singleton number)
              approx
      _ -> approx
    _ -> approx

-- | The reaching definitions analysis.
reachingDefinitions
  :: Monad m
  => Analysis
    (IAnn StatementNumbering OatlabAstF (OatlabIAnnAst StatementNumbering))
    ReachingDefinitionsApprox
    'StatementNode
    'Forward
    m
reachingDefinitions = Analysis
  { analysisMerge =
    \ (ReachingDefinitionsApprox a1 :: ReachingDefinitionsApprox node)
      (ReachingDefinitionsApprox a2)
      -> pure $ ReachingDefinitionsApprox $ case reflectS (Proxy :: Proxy node) of
        StatementNodeS -> M.unionWith S.union a1 a2
        ProgramDeclNodeS -> ()
        TopLevelDeclNodeS -> ()
        VarDeclNodeS -> ()
        ExpressionNodeS -> ()
        IdentifierNodeS -> ()
  , analysisDataflow = reachingDefinitionsDataflow
  , analysisApproximationEq = (==)
  , analysisBoundaryApproximation = pure (ReachingDefinitionsApprox M.empty)
  }

reachingDefinitionsPpAlg
  :: IAnn (AnalysisAnnotation ReachingDefinitionsApprox StatementNumbering) OatlabAstF (K Doc) :~> K Doc
reachingDefinitionsPpAlg (IAnn (AnalysisAnnotation{..}) node) = case collapseIndex node of
  StatementNodeS -> K $ unK (ppAlg node) <+> "/*" <+> ppRd analysisApproximation analysisAnnotation <+> "*/"
  _ -> ppAlg node

ppRd :: ReachingDefinitionsApprox 'StatementNode -> StatementNumbering 'StatementNode-> Doc
ppRd (ReachingDefinitionsApprox approx) (StatementNumbering number)
  = "#" <> ppStmtNum number <> rest where
    defs = S.toList (S.unions (M.elems approx))
    ppDefs = hcat (punctuate ", " (ppStmtNum <$> defs))
    rest = if null defs then empty else " :" <+> ppDefs

ppStmtNum :: StatementNumber -> Doc
ppStmtNum (StatementNumber n) = int n
