{-# LANGUAGE OverloadedStrings #-}

module Language.Minilang.Lexer
( Parser
, Input
, spaceConsumer
, lexeme
, symbol
, rawIdentifier
, identifier
, rawType
, type_
, rawStringLiteral
, stringLiteral
, rawIntegerLiteral
, integerLiteral
, rawFloatLiteral
, floatLiteral
, tokVar
, tokWhile
, tokDo
, tokDone
, tokIf
, tokThen
, tokElse
, tokEnd
, tokRead
, tokPrint
, tokMinus
, tokPlus
, tokTimes
, tokDivide
, parens
, semicolon
, colon
, equals
) where

import Language.Minilang.Syntax

import Control.Monad ( void, mzero )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as T
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

rawStringLiteral :: Parser Text
rawStringLiteral = char '"' *> (T.pack <$> manyTill anyChar (char '"'))

rawIdentifier :: Parser Text
rawIdentifier = do
    c <- letterChar
    s <- many alphaNumChar
    return $ T.pack (c:s)

rawIntegerLiteral :: Parser Int
rawIntegerLiteral = zero <|> num where
    zero = char '0' *> notFollowedBy digitChar *> pure 0
    num = fmap read $ (:) <$> oneOf ['1'..'9'] <*> many digitChar

rawFloatLiteral :: Parser Double
rawFloatLiteral = simple <|> noIntegralPart where
    integralPart = fromIntegral <$> rawIntegerLiteral 
    fractionalPart = (read . ("0." ++) . (++ "0")) <$> many digitChar

    simple = do
        ip <- try $ integralPart <* char '.'
        frac <- optional fractionalPart
        let d = fromMaybe 0 frac
        return $ ip + d

    noIntegralPart = do
        try $ char '.'
        fractionalPart

stringLiteral :: Parser Text
stringLiteral = lexeme rawStringLiteral

identifier :: Parser Text
identifier = lexeme rawIdentifier

integerLiteral :: Parser Int
integerLiteral = lexeme rawIntegerLiteral

floatLiteral :: Parser Double
floatLiteral = lexeme rawFloatLiteral

type_ :: Parser Type
type_ = lexeme rawType

rawType :: Parser Type
rawType = pure TyReal <* tokTyFloat
    <|> pure TyInt <* tokTyInt
    <|> pure TyString <* tokTyString

tokTyFloat :: Parser ()
tokTyFloat = void $ lexeme $ try (string "float") <* notFollowedBy alphaNumChar

tokTyInt :: Parser ()
tokTyInt = void $ lexeme $ try (string "int") <* notFollowedBy alphaNumChar

tokTyString :: Parser ()
tokTyString = void $ lexeme $ try (string "string") <* notFollowedBy alphaNumChar

tokVar :: Parser ()
tokVar = void $ lexeme $ try (string "var") <* notFollowedBy alphaNumChar

tokWhile :: Parser ()
tokWhile = void $ lexeme $ try (string "while") <* notFollowedBy alphaNumChar

tokRead :: Parser ()
tokRead = void $ lexeme $ try (string "read") <* notFollowedBy alphaNumChar

tokPrint :: Parser ()
tokPrint = void $ lexeme $ try (string "print") <* notFollowedBy alphaNumChar

tokIf :: Parser ()
tokIf = void $ lexeme $ try (string "if") <* notFollowedBy alphaNumChar

tokThen :: Parser ()
tokThen = void $ lexeme $ try (string "then") <* notFollowedBy alphaNumChar

tokElse :: Parser ()
tokElse = void $ lexeme $ try (string "else") <* notFollowedBy alphaNumChar

tokEnd :: Parser ()
tokEnd = void $ lexeme $ try (string "end") <* notFollowedBy alphaNumChar

tokDo :: Parser ()
tokDo = void $ lexeme $ try (string "do") <* notFollowedBy alphaNumChar

tokDone :: Parser ()
tokDone = void $ lexeme $ try (string "done") <* notFollowedBy alphaNumChar

tokMinus :: Parser ()
tokMinus = void $ symbol "-"

tokPlus :: Parser ()
tokPlus = void $ symbol "+"

tokTimes :: Parser ()
tokTimes = void $ symbol "*"

tokDivide :: Parser ()
tokDivide = void $ symbol "/"

colon :: Parser ()
colon = void $ symbol ":"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

semicolon :: Parser ()
semicolon = void $ symbol ";"

equals :: Parser ()
equals = void $ symbol "="
