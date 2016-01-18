{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Minilang
import Language.Minilang.Lexer
import Language.Minilang.Parser

import Control.Monad ( forM_ )
import Data.String ( IsString, fromString )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Text.IO ( readFile )
import Prelude hiding ( readFile )
import System.FilePath
import System.Directory
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec

sourcesDir = "test-sources"

getTestSources :: IO [(String, Text)]
getTestSources = getMinilangFiles >>= mapM readWithPath where
    getMinilangFiles = filter isMinilangFile <$> getSourcePaths
    isMinilangFile = (==) ".min" . takeExtension
    getSourcePaths = getDirectoryContents sourcesDir
    readWithPath p = (,) <$> pure p <*> readFile (sourcesDir </> p)

parseOnly m = parse (m <* eof) "" . fromString

main = do
    testSources <- getTestSources

    putStrLn "Loading the following Minilang test programs:"
    forM_ testSources $ \(n, _) -> putStrLn $ "\t" ++ n
    
    hspec $ describe "Language.Minilang" $ do
        describe "Lexer" $ do
            describe "identifier" $ do
                prop "parses a letter character followed by alphanumeric characters" $
                    forAll identifiers (isRight . parseOnly identifier)

                prop "fails on lexemes beginning with digits" $
                    forAll ((:) <$> elements digit <*> identifiers) (isLeft . parseOnly identifier)

            describe "integerLiteral" $ do
                it "parses simple integer literals" $ do
                    parseOnly integerLiteral "3" `shouldBe` Right 3
                    parseOnly integerLiteral "0" `shouldBe` Right 0

                it "fails to parse nonzero integers beginning with zero" $ do
                    parseOnly integerLiteral "00" `shouldSatisfy` isLeft
                    parseOnly integerLiteral "01" `shouldSatisfy` isLeft

            describe "floatLiteral" $ do
                it "parses simple float literals" $ do
                    parseOnly floatLiteral "0.3" `shouldBe` Right 0.3
                    parseOnly floatLiteral "3.000" `shouldBe` Right 3.000

                it "parses float literals with a missing integral part" $ do
                    parseOnly floatLiteral ".3" `shouldBe` Right 0.3
                    parseOnly floatLiteral ".0" `shouldBe` Right 0.0

                it "parses float literals with a missing fractional part" $ do
                    parseOnly floatLiteral "3." `shouldBe` Right 3
                    parseOnly floatLiteral "0." `shouldBe` Right 0.0

        describe "Parser" $ do
            describe "varStmt" $ do
                it "parses variable declarations" $
                    parseOnly varStmt "var x: int;" `shouldBe` Right (Var "x" TyInt)

        describe "parseMinilang" $ do
            forM_ testSources $ \(name, contents) ->
                it ("successfully parses " ++ name) $
                    parseOnlyMinilang name contents `shouldSatisfy` isRight

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

lower :: [Char]
lower = ['a' .. 'z']

upper :: [Char]
upper = ['A' .. 'Z']

nonzero :: [Char]
nonzero = ['1' .. '9']

digit :: [Char]
digit = '0' : nonzero

letter :: [Char]
letter = lower ++ upper

alphanumeric :: [Char]
alphanumeric = lower ++ upper ++ digit

identifiers :: IsString a => Gen a
identifiers = do
    l <- elements letter
    cs <- shuffle =<< sublistOf alphanumeric
    return $ fromString (l:cs)
