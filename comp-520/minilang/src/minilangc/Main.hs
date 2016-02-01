{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Minilang
import Language.Minilang.Codegen
import Language.Minilang.Typecheck

import Control.Monad.Except
import Data.Functor.Foldable
import Data.Text ( pack )
import Data.Text.IO ( readFile, writeFile, getContents )
import qualified Language.C.Pretty as CP
import Options.Applicative
import Prelude hiding ( readFile, writeFile, getContents )
import System.FilePath
import Text.PrettyPrint hiding ( (<>) )

unFix :: Fix f -> f (Fix f)
unFix (Fix f) = f

argParser :: ParserInfo String
argParser = info sourcePath details where
    sourcePath = argument str (metavar "FILE")
    details = header h <> progDesc p
    h = "minilangc - checks the syntax of a minilang program"
    p = "The specified FILE is loaded and parsed. "
        ++ "If the parse succeeds, then `Valid` is printed to standard out; "
        ++ "else, `Invalid` is printed. "
        ++ "The special file - can be used to read from standard in."

main :: IO ()
main = do
    sourcePath <- execParser argParser
    (sourcePath', contents) <- if sourcePath == "-"
        then (,) <$> pure "stdin"    <*> getContents
        else (,) <$> pure sourcePath <*> readFile sourcePath

    case parseOnlyMinilang sourcePath' contents of
        Left e -> do
            putStrLn "Parse error."
            print e
        Right p -> handleParsed sourcePath' p

handleParsed :: FilePath -> SrcAnnProgram -> IO ()
handleParsed sourcePath p = do
    -- write the pretty-printed minilang code
    let pp = pack . render . pretty . unannotateProgram $ p
    writeFile (sourcePath -<.> "pretty.min") pp

    -- compute the symbol table from the declarations list
    let (Program decls _) = p
    let symtab = symbolTableFrom (map unannotateDecl decls)

    -- write out the symbol table in a tab-separated format
    writeFile (sourcePath -<.> "symbol.txt") (pack (symbolTableTsv symtab))

    -- typecheck the program
    case runExcept (typecheckProgram p) of
        Left e -> putStrLn (render (pretty e))
        Right p' -> handleTypechecked sourcePath p'

handleTypechecked :: FilePath -> TySrcAnnProgram -> IO ()
handleTypechecked sourcePath p = do
    -- perform code generation
    let c = translateProgramMain p

    -- write out the generated code
    writeFile (sourcePath -<.> "c") (pack (render (CP.pretty c)))
