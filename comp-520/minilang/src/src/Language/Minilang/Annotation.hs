{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Minilang.Annotation where

import Data.Functor.Foldable

import Language.Minilang.Pretty
import Language.Minilang.Precedence

-- | A functor value with an annotation.
data Ann x f a
    = Ann
        { ann :: !x
        -- ^ Extract only the annotation, discarding the data.
        , bare :: f a
        -- ^ Extract only the data, discarding the annotation.
        }
    deriving (Eq, Read, Show, Functor)

type instance Base (Ann x f a) = f

-- | Strip all annotations from a syntax tree.
bareF :: Functor f => Fix (Ann x f) -> Fix f
bareF = cata (Fix . bare)

instance (Functor f, Pretty (Fix f)) => Pretty (Fix (Ann x f)) where
    pretty = pretty . bareF

instance Pretty (f a) => Pretty (Ann x f a) where
    pretty = pretty . bare

instance (Functor f, HasPrecedence (f a)) => HasPrecedence (Ann x f a) where
    precedence = precedence . bare
