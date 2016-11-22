{-|
Module      : Data.Annotation
Description : Indexed and unindexed annotations for higher-order functors
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental

Polykinded higher-order functors allow you to represent indexed syntax trees.
Examples of indexed syntax trees include syntax trees with (static) type
annotations and mutual recursion.
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Data.Annotation
( Ann(..)
, HAnn(..)
, HAnnFix
, stripH
, IAnn(..)
, IAnnFix
, stripI
) where

import Data.HFunctor ( HFunctor(..), HFix(..), hcata )

-- | Annotations for plain functors.
data Ann (x :: *) (f :: * -> *) (a :: *)
  = Ann
    { ann :: !x
    -- ^ The annotation.
    , bare :: f a
    -- ^ The annotated data.
    }

-- | Unindexed annotations for higher-order functors.
--
-- The type of unindexed annotations can be recovered from the type of indexed
-- annotations by using a type-level constant function for the index
-- interpreter.
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
-- interpretation @p@, @IAnn p@ can be given an HFunctor instance.
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

-- | The fixpoint of an unindexed-annotated higher-order functor.
type HAnnFix x h = HFix (HAnn x h)

-- | The fixpoint of an indexed-annotated higher-order functor.
type IAnnFix p h = HFix (IAnn p h)

-- | Remove all annotations from an indexed-annotated syntax tree.
stripI :: HFunctor h => IAnnFix p h i -> HFix h i
stripI = hcata (HFix . bareI)

-- | Remove all annotations from an unindexed-annotated syntax tree.
stripH :: HFunctor h => HAnnFix x h i -> HFix h i
stripH = hcata (HFix . bareH)
