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
) where

-- | An analysis over some language is a collection of functions parametrized
-- by the configuration of the language's AST.
--
-- The fields 'analysisMerge' and 'analysisDataflow' implement part of the six
-- steps. The remaining fields are housekeeping functions. In detail,
-- 'analysisApproximationEq' is needed to determine when a fixed point has been
-- found. The remaining fields enable the programmer to use highly customizable
-- annotations on AST node, provided that the programmer provides ways for
-- retrieving and storing data essential to the dataflow analysis.
--
-- * @h@ - the AST to analyze
-- * @f@ - the result of the analysis, indexed by the type of the node; usually
--   this is another syntax tree, whose annotations have been transformed by
--   the analysis.
-- * @p@ - the annotations on the AST, indexed by the type of the node
-- * @approx@ - the approximation used in this analysis
-- * @iterNode@ - the index of AST nodes that contain iteration counts; this is
--   generally the index of statements or other looping constructs
-- * @m@ - an arbitrary monad to perform the analysis in; many of the
--   operations of the analysis are required to operate in this monad, so that
--   the programmer can store global state for the analysis or throw
--   exceptions.
-- * @dir@ - the direction of the analysis (phantom type)
data Analysis
  (h :: (k -> *) -> k -> *)
  (f :: k -> *)
  (p :: k -> *)
  (approx :: *)
  (iterNode :: k)
  (m :: * -> *)
  (dir :: Direction)
  = Analysis
    { analysisMerge :: approx -> approx -> m approx
    -- ^ How are approximations merged?
    , analysisDataflow :: forall a. h f a -> approx -> m approx
    -- ^ How do we compute a new approximation for a node in the syntax tree,
    -- given some existing approximation?
    , analysisApproximationEq :: approx -> approx -> Bool
    -- ^ How do we decide equality of approximations?
    , analysisUpdateApproximation :: forall a. approx -> p a -> m (p a)
    -- ^ How do we store an approximation inside an annotation?
    , analysisGetApproximation :: forall a. p a -> approx
    -- ^ How do we retrieve an approximation from an annotation?
    , analysisUpdateIterations :: (Int -> Int) -> p iterNode -> m (p iterNode)
    -- ^ How do we update the count of remaining iterations in an iteration
    -- node's annotation?
    , analysisGetIterations :: p iterNode -> Int
    -- ^ How do we retrieve the remaining count of iterations from an iteration
    -- node's annotation?
    , analysisBoundaryApproximation :: m approx
    -- ^ What is the initial approximation at a boundary? (e.g. the start of
    -- the function)

    -- , analysisInitialApproximation :: m approx
    -- -- ^ What is the initial approximation for an arbitrary statement?
    }

-- | The direction of an analysis.
-- Used as a datakind.
data Direction = Forward | Backward
