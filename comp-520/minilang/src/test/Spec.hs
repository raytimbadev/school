{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Minilang
import Language.Minilang.Lexer
import Language.Minilang.Typecheck
import Language.Minilang.SrcAnn

import Control.Monad ( forM_, (>=>) )
import Control.Monad.Except ( runExcept )
import Data.String ( IsString, fromString )
import Data.Text ( Text )
import Data.Text.IO ( readFile )
import Prelude hiding ( readFile )
import System.FilePath
import System.Directory
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec

validSourcesDir :: FilePath
validSourcesDir = "valid-test-sources"

invalidSourcesDir :: FilePath
invalidSourcesDir = "invalid-test-sources"

invalidTypeSourcesDir :: FilePath
invalidTypeSourcesDir = "invalid-type"

getTestSources :: FilePath -> IO [(String, Text)]
getTestSources sourcesDir = getMinilangFiles >>= mapM readWithPath where
    getMinilangFiles = filter isMinilangFile <$> getSourcePaths
    isMinilangFile = (==) ".min" . takeExtension
    getSourcePaths = getDirectoryContents sourcesDir
    readWithPath p = (,) <$> pure p <*> readFile (sourcesDir </> p)

parseOnly :: Parser a -> String -> Either ParseError a
parseOnly m = parse (m <* eof) "" . fromString

roundTrip3 :: Input -> Either ParseError Input
roundTrip3 = roundTrip >=> roundTrip >=> roundTrip

main :: IO ()
main = do
    validSources <- getTestSources validSourcesDir

    putStrLn "Loading the following valid Minilang test programs:"
    forM_ validSources $ \(n, _) -> putStrLn $ "\t" ++ n
    putStrLn ""

    invalidSources <- getTestSources invalidSourcesDir

    putStrLn "Loading the following invalid Minilang test programs:"
    forM_ invalidSources $ \(n, _) -> putStrLn $ "\t" ++ n
    putStrLn ""

    invalidTypeSources <- getTestSources invalidTypeSourcesDir

    putStrLn "Loading the semantically invalid Minilang test programs:"
    forM_ invalidTypeSources $ \(n, _) -> putStrLn $ "\t" ++ n
    putStrLn ""
    
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

                it "fails to parse float literals consisting of just a dor" $ do
                    parseOnly floatLiteral "." `shouldSatisfy` isLeft

        describe "Parser" $ do
            describe "varDecl" $ do
                it "parses simple variable declarations" $ do
                    unannotateDecl <$> parseOnly varDecl "var x: int;" `shouldBe` Right (Var "x" TyInt)
                    unannotateDecl <$> parseOnly varDecl "var y: float;" `shouldBe` Right (Var "y" TyReal)
                    unannotateDecl <$> parseOnly varDecl "var z: string;" `shouldBe` Right (Var "z" TyString)

                it "parses a whole statement, ending with a semicolon" $
                    parseOnly varDecl "var hi: string" `shouldSatisfy` isLeft

                it "parses declarations that must include a type" $
                    parseOnly varDecl "var hi: ;" `shouldSatisfy` isLeft

                it "fails if there's no space after the 'var' keyword" $
                    parseOnly varDecl "varh i: string;" `shouldSatisfy` isLeft

                prop "the name of the variable can be any identifier" $
                    forAll identifiers $ \i ->
                        parseOnly varDecl ("var " ++ i ++ " : int;") `shouldSatisfy` isRight

        describe "parseMinilang" $ do
            forM_ validSources $ \(name, contents) ->
                it ("successfully parses the valid program " ++ name) $
                    parseOnlyMinilang name contents `shouldSatisfy` isRight

            forM_ validSources $ \(name, contents) ->
                it ("round-trips the valid program " ++ name) $
                    roundTrip contents `shouldBe` roundTrip3 contents

            forM_ invalidSources $ \(name, contents) ->
                it ("fails to parse the invalid program " ++ name) $
                    parseOnlyMinilang name contents `shouldSatisfy` isLeft

        describe "typecheckProgram" $ do
            forM_ validSources $ \(name, contents) ->
                it ("typechecks the valid program " ++ name) $
                    checkTypecheck
                        -- in case it fails to typecheck
                        (expectationFailure . show)
                        -- in case it successfully typechecks
                        (const (pure ()))
                        name
                        contents

            forM_ invalidTypeSources $ \(name, contents) ->
                it ("fails to typecheck the invalid program " ++ name) $
                    checkTypecheck
                        -- in case it fails to typecheck
                        (const (pure ()))
                        -- in case it successfully typechecks
                        (const (expectationFailure "should not typecheck"))
                        name
                        contents

checkTypecheck
    :: (SrcAnn SemanticError a -> Expectation)
    -> (TySrcAnnProgram -> Expectation)
    -> String -> Input -> Expectation
checkTypecheck bad good name contents = case parseOnlyMinilang name contents of
    Left e -> expectationFailure (show e)
    Right p -> case runExcept (typecheckProgram p) of
        Left e' -> bad e'
        Right p' -> good p'

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
