{-|
Module      : Data.Annotation
Description : Uniform and indexed annotations for higher-order functors
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental

Uniform and indexed annotations for higher-order functors.
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Annotation
( -- * Annotations on plain functors
  Ann(..)
  -- * Uniform annotations on higher-order functors
, HAnn(..)
, HAnnFix
, stripH
  -- * Indexed annotations on higher-order functors
, IAnn(..)
, IAnnFix
, stripI
, topAnnI
, mapTopAnnI
  -- ** Combinators
, reannotate
, reannotateM
) where

import Data.HFunctor ( HFunctor(..), HFix(..), HTraversable(..), hcata, (:~>) )

-- | Annotations for plain functors.
--
-- This type of annotation is unused in McFAS, but serves as the starting point
-- in the generalizations 'AnnH' and 'AnnI'.
data Ann (x :: *) (f :: * -> *) (a :: *)
  = Ann
    { ann :: !x
    -- ^ The annotation.
    , bare :: f a
    -- ^ The annotated data.
    }

-- | Uniform annotations for higher-order functors.
--
-- The type of uniform annotations can be recovered from the type of indexed
-- annotations by using a type-level constant function (e.g. @'K' x@) for the
-- index interpreter.
data HAnn (x :: *) (h :: (k -> *) -> k -> *) (f :: k -> *) (a :: k)
  = HAnn
    { annH :: !x
    -- ^ The annotation.
    , bareH :: h f a
    -- ^ The annotated data.
    }

instance HFunctor h => HFunctor (HAnn x h) where
    hfmap f (HAnn a h) = HAnn a (hfmap f h)

-- | Indexed annotations for higher-order functors.
--
-- * @p@ -- the type-level function that interprets the index into a concrete
--   type.
-- * @h@ -- the higher-order functor
-- * @f@ -- the functor that the hfunctor provides context to
-- * @a@ -- the index
--
-- The reason we require so many type parameters is that for a fixed
-- interpretation @p@, @IAnn p h@ can be given an HFunctor instance provided
-- @h@ is an HFunctor.
data IAnn
  (p :: k -> *)
  (h :: (k -> *) -> k -> *)
  (f :: k -> *)
  (a :: k)
  = IAnn
    { annI :: !(p a)
    -- ^ The annotation, whose type is determined by the index interpretation
    -- on the index.
    , bareI :: h f a
    -- ^ The annotated data.
    }

instance HFunctor h => HFunctor (IAnn x h) where
  hfmap f (IAnn a h) = IAnn a (hfmap f h)

instance HTraversable h => HTraversable (IAnn p h) where
  sequenceH (IAnn a node) = IAnn <$> pure a <*> sequenceH node

-- | The fixpoint of an unindexed-annotated higher-order functor.
type HAnnFix x h = HFix (HAnn x h)

-- | The fixpoint of an indexed-annotated higher-order functor.
type IAnnFix p h = HFix (IAnn p h)

-- | Extract the annotation at the root of the tree.
topAnnI :: IAnnFix p h a -> p a
topAnnI (HFix (IAnn a _)) = a

-- | Apply a function to the annotation at the root of the tree.
mapTopAnnI :: (p a -> p a) -> IAnnFix p h a -> IAnnFix p h a
mapTopAnnI f (HFix (IAnn a s)) = HFix (IAnn (f a) s)

-- | Remove all annotations from an indexed-annotated syntax tree.
stripI :: HFunctor h => IAnnFix p h i -> HFix h i
stripI = hcata (HFix . bareI)

-- | Remove all annotations from an unindexed-annotated syntax tree.
stripH :: HFunctor h => HAnnFix x h i -> HFix h i
stripH = hcata (HFix . bareH)

-- | Combinator for rewriting annotations.
--
-- This saves you the trouble of having to reconstruct each node manually.
reannotate
  :: forall (h :: (k -> *) -> k -> *) (p :: k -> *) (q :: k -> *) (f :: k -> *).
    (forall (a :: k). h f a -> p a -> q a) -> IAnn p h f :~> IAnn q h f
reannotate f (IAnn a node) = IAnn (f node a) node

-- | Combinator for rewriting annotations monadically.
--
-- This saves you the trouble of having to reconstruct each node manually and
-- from having to use newtype tricks to do the same thing using 'reannotate'.
reannotateM
  :: forall
    (m :: * -> *)
    (h :: (k -> *) -> k -> *)
    (p :: k -> *)
    (q :: k -> *)
    (f :: k -> *).
    Applicative m
    => (forall (a :: k). h f a -> p a -> m (q a))
    -> (forall (a :: k). IAnn p h f a -> m (IAnn q h f a))
reannotateM f (IAnn a node) = IAnn <$> (f node a) <*> pure node
