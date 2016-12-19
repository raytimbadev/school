{-|
Module      : Language.Common.Analysis
Description : Oatlab-specific analysis code
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental

A dataflow-analysis is specified using the six-steps methodology:

  1. Approximation domain
  2. Precise statement of the analysis
  3. Direction of the analysis: forwards or backwards
  4. Merge operator: typically union or intersection
  5. Data flow equations.
  6. Initial conditions.

Our approach is to represent an analysis using a record whose fields indicate
steps 4 and 5, and whose type parameters indicate steps 1 and 3. Step six is
implicit in the initial state of each node annotation, and step 2 is
documentation.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Common.Analysis
( Direction(..)
, Analysis(..)
, AnalysisAnnotation(..)
, DataflowEquation
, MergeEquation
) where

import Data.HFunctor
import Data.Reflection ( ReflectS )

data AnalysisAnnotation (approx :: k -> *) (inner :: k -> *) (a :: k)
  = AnalysisAnnotation
    { analysisIterations :: !Int
    , analysisApproximation :: approx a
    , analysisAnnotation :: inner a
    }

instance HFunctor (AnalysisAnnotation approx) where
  hfmap f a = a { analysisAnnotation = f (analysisAnnotation a) }

-- | An analysis over some language is a collection of functions parametrized
-- by the configuration of the language's AST.
--
-- The fields 'analysisMerge' and 'analysisDataflow' implement part of the six
-- steps. The remaining fields are housekeeping functions. In detail,
-- 'analysisApproximationEq' is needed to determine when a fixed point has been
-- found. The remaining fields enable the programmer to use highly customizable
-- annotations on AST node, provided that the programmer provides ways for
-- retrieving and storing data essential to the dataflow analysis in those
-- annotations.
--
-- * @h@ - the AST to analyze
-- * @f@ - the result of the analysis, indexed by the type of the node; usually
--   this is another syntax tree, whose annotations are of the same /type/, but
--   have been transformed by the analysis
-- * @approx@ - the approximation used in this analysis
--   generally the index of statements or other looping constructs
-- * @m@ - an arbitrary monad to perform the analysis in; many of the
--   operations of the analysis are required to operate in this monad, so that
--   the programmer can store global state for the analysis or throw
--   exceptions. For example, analysis runners use a monadic effect to indicate
--   fixpoint solution failure.
-- * @dir@ - the direction of the analysis (phantom type)
data Analysis
  (ast :: k -> *) -- how are syntax trees represented in the dataflow equation
  (approx :: k -> *)
  (boundaryNode :: k)
  (dir :: Direction)
  (m :: * -> *)
  = Analysis
    { analysisMerge :: MergeEquation approx m
      -- ^ How are approximations merged?
    , analysisDataflow :: DataflowEquation approx m ast
      -- ^ How do we compute a new approximation for a node in the syntax tree,
      -- given some existing approximation?
    , analysisApproximationEq
      :: forall a. ReflectS a => approx a -> approx a -> Bool
      -- ^ How do we decide equality of approximations?
    , analysisBoundaryApproximation
      :: m (approx boundaryNode)
      -- ^ What is the initial approximation at a boundary? (e.g. the start of
      -- the function)
    -- , analysisInitialApproximation
    --   :: forall a. ast a -> m (approx a)
    -- -- ^ What is the initial approximation for an arbitrary statement?
    }

type DataflowEquation approx m ast
  = forall node. ReflectS node => ast node -> approx node -> m (approx node)

type MergeEquation approx m
  = forall node. ReflectS node => approx node -> approx node -> m (approx node)

-- | The direction of an analysis.
--
-- /Kind/
data Direction = Forward | Backward
