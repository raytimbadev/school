module Language.Minilang.SrcAnn
( -- * Source annotations
  SrcSpan(..)
, SrcAnn
, SrcAnnFix
  -- ** Parsing helpers
, withSrcAnnFix
, withSrcAnnId
, withSrcAnn
  -- ** Destroying annotations
, ignoreSrcAnn
  -- * The annotation framework
, module Language.Minilang.Annotation
) where

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

-- withSrcAnnFix
--     :: Parser (f (Fix (Ann SrcSpan f)))
--     -> Parser (Fix (Ann SrcSpan f))
-- withSrcAnnFix p = do
--     p1 <- getPosition
--     x <- p
--     p2 <- getPosition
--     pure (Fix $ Ann (SrcSpan p1 p2) x)

-- | Runs a parser that produces a source-annotated syntax tree and wraps it in
-- another layer of source annotation.
withSrcAnnFix
    :: Parser (f (Fix (Ann SrcSpan f)))
    -> Parser (Fix (Ann SrcSpan f))
withSrcAnnFix = fmap Fix . withSrcAnn id

-- | Run a parser and annotate its result after applying a given wrapping
-- strategy with source position information.
withSrcAnn
    :: (a -> f b) -- ^ A wrapping strategy for the parsed data
    -> Parser a -- ^ The parser to annotate
    -> Parser (SrcAnn f b) -- ^ A parser that produces an annotated result.
withSrcAnn f p = do
    p1 <- getPosition
    x <- p
    p2 <- getPosition
    pure (Ann (SrcSpan p1 p2) (f x))

-- | Run a parser and annotate its result in the identity functor with source
-- position information.
withSrcAnnId :: Parser a -> Parser (SrcAnn Identity a)
withSrcAnnId = withSrcAnn Identity

ignoreSrcAnn :: SrcAnn Identity a -> a
ignoreSrcAnn = runIdentity . bare
