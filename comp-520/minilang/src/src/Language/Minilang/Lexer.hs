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

import Language.Minilang.Lexer.Core
import Language.Minilang.Syntax

import Control.Monad ( void )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as T

rawStringLiteral :: Parser Text
rawStringLiteral = char '"' *> (T.pack <$> manyTill strchar (char '"'))

strchar :: Parser Char
strchar
    = oneOf
    $ concat
        [ "qwertyuiopasdfghjklzxcvbnm"
        , "QWERTYUIOPASDFGHJKLZXCVBNM"
        , "1234567890!.,? "
        ]

rawIdentifier :: Parser Text
rawIdentifier = do
    c <- letterChar
    s <- many alphaNumChar
    return $ T.pack (c:s)

rawIntegerLiteral :: Parser Integer
rawIntegerLiteral = zero <|> num where
    zero = char '0' *> notFollowedBy digitChar *> pure 0
    num = fmap read $ (:) <$> oneOf ['1'..'9'] <*> many digitChar

rawFloatLiteral :: Parser Double
rawFloatLiteral = simple <|> noIntegralPart where
    integralPart = fromIntegral <$> rawIntegerLiteral 
    fractionalPart = (read . ("0." ++) . (++ "0")) <$> many digitChar
    fractionalPart' = (read . ("0." ++) . (++ "0")) <$> some digitChar

    simple = do
        ip <- try $ integralPart <* char '.'
        frac <- optional fractionalPart
        let d = fromMaybe 0 frac
        return $ ip + d

    noIntegralPart = do
        try $ void $ char '.'
        fractionalPart'

stringLiteral :: Parser Text
stringLiteral = lexeme rawStringLiteral

identifier :: Parser Text
identifier = notFollowedBy (choice reserved) *> lexeme rawIdentifier

integerLiteral :: Parser Integer
integerLiteral = lexeme rawIntegerLiteral

floatLiteral :: Parser Double
floatLiteral = lexeme rawFloatLiteral

type_ :: Parser Type
type_ = lexeme rawType

rawType :: Parser Type
rawType = pure TyReal <* tokTyFloat
    <|> pure TyInt <* tokTyInt
    <|> pure TyString <* tokTyString

reservedWord :: String -> Parser ()
reservedWord s = void $ lexeme $ try (string s) <* notFollowedBy alphaNumChar

tokTyFloat :: Parser ()
tokTyFloat = reservedWord "float"

tokTyInt :: Parser ()
tokTyInt = reservedWord "int"

tokTyString :: Parser ()
tokTyString = reservedWord "string"

tokVar :: Parser ()
tokVar = reservedWord "var"

tokWhile :: Parser ()
tokWhile = reservedWord "while"

tokRead :: Parser ()
tokRead = reservedWord "read"

tokPrint :: Parser ()
tokPrint = reservedWord "print"

tokIf :: Parser ()
tokIf = reservedWord "if"

tokThen :: Parser ()
tokThen = reservedWord "then"

tokElse :: Parser ()
tokElse = reservedWord "else"

tokEnd :: Parser ()
tokEnd = reservedWord "endif"

tokDo :: Parser ()
tokDo = reservedWord "do"

tokDone :: Parser ()
tokDone = reservedWord "done"

reserved :: [Parser ()]
reserved =
    [ tokTyFloat
    , tokTyInt
    , tokTyString
    , tokVar
    , tokWhile
    , tokRead
    , tokPrint
    , tokIf
    , tokThen
    , tokElse
    , tokEnd
    , tokDo
    , tokDone
    ]

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
