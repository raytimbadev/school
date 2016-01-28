module Language.Minilang.SrcAnn where

import Language.Minilang.Annotation
import Language.Minilang.Lexer.Core

import Data.Functor.Identity
import Data.Functor.Foldable

-- | A source span has a beginning and an end.
data SrcSpan
    = SrcSpan
        { srcStart :: !SourcePos
        , srcEnd :: !SourcePos
        }
    deriving (Eq, Ord, Show)

-- | A source annotated functor is a functor value annotated with some span of
-- source text.
type SrcAnn = Ann SrcSpan

type SrcAnnFix f = Fix (Ann SrcSpan f)

withSrcAnnFix
    :: Parser (f (Fix (Ann SrcSpan f)))
    -> Parser (Fix (Ann SrcSpan f))
withSrcAnnFix p = do
    p1 <- getPosition
    x <- p
    p2 <- getPosition
    pure (Fix $ Ann (SrcSpan p1 p2) x)

withSrcAnn' :: (a -> f b) -> Parser a -> Parser (SrcAnn f b)
withSrcAnn' f p = do
    p1 <- getPosition
    x <- p
    p2 <- getPosition
    pure (Ann (SrcSpan p1 p2) (f x))

ignoreSrcAnn :: SrcAnn Identity a -> a
ignoreSrcAnn = runIdentity . bare
