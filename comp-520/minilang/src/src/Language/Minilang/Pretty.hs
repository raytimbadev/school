{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Minilang.Pretty
( Pretty(..)
, prettyBrackets
, prettyParens
) where

import Data.Functor.Identity
import Data.Text ( Text, unpack )
import Text.PrettyPrint

-- | Minilang pretty-printable types.
--
-- Essentially a clone of 'Text.Show.Show' but with the aim of producing
-- Minilang code instead of Haskell code.
class Pretty a where
    pretty :: a -> Doc
    prettyPrec :: Int -> a -> Doc

    prettyPrec _ x = pretty x
    pretty x = prettyPrec 0 x

instance Pretty Char where
    pretty = char

instance Pretty a => Pretty (Maybe a) where
    prettyPrec d e = case e of
        Just x -> prettyPrec d x
        Nothing -> mempty

instance Pretty Int where
    pretty = int

instance Pretty Integer where
    pretty = integer

instance Pretty Double where
    pretty = double

instance Pretty Text where
    pretty = text . unpack

deriving instance Pretty a => Pretty (Identity a)

prettyBrackets :: Bool -> Doc -> Doc
prettyBrackets True s = text "[" <> s <> text "]"
prettyBrackets False s = s

prettyParens :: Bool -> Doc -> Doc
prettyParens True s = text "(" <> s <> text ")"
prettyParens False s = s
