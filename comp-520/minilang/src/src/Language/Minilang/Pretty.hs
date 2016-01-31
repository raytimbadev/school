{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Minilang.Pretty
( Pretty(..)
, prettys
, prettyPrefix
, prettyInfix
, prettyBrackets
, prettyParens
, prettySpace
, prettyString
, indent
) where

import Language.Minilang.Precedence

import Data.Functor.Identity
import Data.Text ( Text, unpack )

-- | Minilang pretty-printable types.
--
-- Essentially a clone of 'Text.Show.Show' but with the aim of producing
-- Minilang code instead of Haskell code.
class Pretty a where
    pretty :: a -> String
    prettysPrec :: Int -> a -> ShowS
    prettyList :: [a] -> ShowS

    prettysPrec _ x s = pretty x ++ s
    pretty x = prettysPrec 0 x ""
    prettyList ls = foldr (.) id (map prettys ls)

instance Pretty Char where
    pretty c = show c
    prettyList s = showString s

instance Pretty a => Pretty (Maybe a) where
    prettysPrec d e = case e of
        Just x -> prettysPrec d x
        Nothing -> id

instance Pretty Int where
    pretty i = show i

instance Pretty Integer where
    pretty i = show i

instance Pretty Double where
    pretty x = show x

instance Pretty Text where
    pretty e = unpack e

deriving instance Pretty a => Pretty (Identity a)

-- | Pretty-prints something (as a difference list) with the lowest precedence.
prettys :: Pretty a => a -> ShowS
prettys = prettysPrec 0

-- | Pretty-prints an infix operator and its two operands.
prettyInfix :: (HasPrecedence sym, Pretty sym, Pretty l, Pretty r)
            => sym -> l -> r -> ShowS
prettyInfix sym l r
    = prettysPrec (precedence sym) l
    . showChar ' '
    . showString (pretty sym)
    . showChar ' '
    . prettysPrec (precedence sym) r

-- | Pretty-prints a prefix operator and its operand.
prettyPrefix :: (HasPrecedence sym, Pretty sym, Pretty p)
             => sym -> p -> ShowS
prettyPrefix sym p
    = showString (pretty sym)
    . prettysPrec (precedence sym) p

prettyBrackets :: Pretty a => Bool -> a -> ShowS
prettyBrackets True s = showString "[" . prettys s . showString "]"
prettyBrackets False s = prettys s

prettyParens :: Pretty a => Bool -> a -> ShowS
prettyParens True s = showString "[" . prettys s . showString "]"
prettyParens False s = prettys s

prettySpace :: ShowS
prettySpace = showString " "

prettyString :: String -> ShowS
prettyString = showString

indent :: Int -> ShowS
indent n = showString $ replicate (4*n) ' '
