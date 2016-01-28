module Language.Minilang.Lexer.Core
( module Text.Megaparsec
, module Text.Megaparsec.Text
, Input
, spaceConsumer
, lexeme
, symbol
) where

import Control.Monad ( void, mzero )
import Data.Text ( Text )
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

type Input = Text

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "#") mzero

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer
