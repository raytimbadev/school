module Language.Minilang 
( module Language.Minilang.Syntax
, module Language.Minilang.Parser
, parseMinilang
, parseOnlyMinilang
, pretty
) where

import Language.Minilang.Syntax
import Language.Minilang.Lexer
import Language.Minilang.Parser
import Language.Minilang.Pretty

import Text.Megaparsec

parseMinilang :: String -> Input -> Either ParseError Program
parseMinilang = parse minilang

parseOnlyMinilang :: String -> Input -> Either ParseError Program
parseOnlyMinilang = parse (spaceConsumer *> minilang <* eof)
