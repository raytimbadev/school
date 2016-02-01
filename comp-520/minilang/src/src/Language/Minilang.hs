module Language.Minilang 
( module Language.Minilang.Syntax
, module Language.Minilang.Parser
, parseMinilang
, parseOnlyMinilang
, parseOnlyExpr
, roundTrip
, pretty
) where

import Language.Minilang.Syntax
import Language.Minilang.Lexer
import Language.Minilang.Parser
import Language.Minilang.Parser.Expression
import Language.Minilang.Pretty

import Data.Text ( pack )
import Text.Megaparsec
import Text.PrettyPrint ( render )

parseMinilang :: String -> Input -> Either ParseError SrcAnnProgram
parseMinilang = parse minilang

parseOnlyMinilang :: String -> Input -> Either ParseError SrcAnnProgram
parseOnlyMinilang = parse (spaceConsumer *> minilang <* eof)

parseOnlyExpr :: String -> Input -> Either ParseError SrcAnnExpr
parseOnlyExpr = parse (spaceConsumer *> expr <* eof)

roundTrip :: Input -> Either ParseError Input
roundTrip s = pack . render . pretty . unannotateProgram <$> parseOnlyMinilang "roundtrip" s
