module Language.Minilang.Precedence where

class HasPrecedence a where
    precedence :: a -> Int
