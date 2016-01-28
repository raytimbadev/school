module Language.Minilang.Precedence where

import Data.Functor.Identity

class HasPrecedence a where
    precedence :: a -> Int

instance HasPrecedence a => HasPrecedence (Identity a) where
    precedence = precedence . runIdentity
