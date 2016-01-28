{-# LANGUAGE DeriveFunctor #-}

module Language.Minilang.Validation where

import Language.Minilang.Pretty

data Validation m a
    = Failures m
    | Success a
    deriving (Functor)

instance Monoid m => Applicative (Validation m a) where
    pure = Success
    Failures e1 <*> Failures e2 = Failures (e1 <> e2)
    Failures e  <*> _           = Failures e
    _           <*> Failures e  = Failures e
    Success f   <*> Success x   = Success (f x)

toEither :: Validation m a -> Either m a
toEither e = case e of
    Failures m -> Left m
    Success x -> Right x
--
-- | Strips the annotations and uses the pretty printer for the underlying
-- syntax tree.
instance (Functor f, Pretty (Fix f)) => Pretty (Fix (Ann x f)) where
    pretty = pretty . bareF
