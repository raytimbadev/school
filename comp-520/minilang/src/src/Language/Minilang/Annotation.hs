{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Minilang.Annotation where

import Control.Applicative ( Const(..) )
import Data.Functor.Foldable
import Data.Functor.Identity

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

type AnnFix x f = Fix (Ann x f)

-- | Strip all annotations from a syntax tree.
bareF :: Functor f => Fix (Ann x f) -> Fix f
bareF = cata (Fix . bare)

-- | Strips off the annotation and identity functor.
bareId :: Ann x Identity a -> a
bareId = runIdentity . bare

-- | Strips off the annotation and constant functor.
bareConst :: Ann x (Const a) b -> a
bareConst = getConst . bare

instance (Functor f, Pretty (Fix f)) => Pretty (Fix (Ann x f)) where
    pretty = pretty . bareF

instance (Functor f, HasPrecedence (f a)) => HasPrecedence (Ann x f a) where
    precedence = precedence . bare
