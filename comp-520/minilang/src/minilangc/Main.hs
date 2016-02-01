{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Minilang
import Language.Minilang.Codegen
import Language.Minilang.Typecheck
import Language.Minilang.SrcAnn

import Control.Monad.Except
import Data.Functor.Foldable
import Data.Text ( pack )
import Data.Text.IO ( readFile, getContents )
-- import qualified Data.Text.IO as TIO
import qualified Language.C.Pretty as CP
import Options.Applicative
import Prelude hiding ( readFile, getContents )
-- import System.Exit ( exitFailure )
import Text.PrettyPrint hiding ( (<>) )

unFix :: Fix f -> f (Fix f)
unFix (Fix f) = f

argParser :: ParserInfo String
argParser = info sourcePath details where
    sourcePath = argument str (metavar "FILE")
    details = header h <> progDesc p
    h = "minilangcheck - checks the syntax of a minilang program"
    p = "The specified FILE is loaded and parsed. "
        ++ "If the parse succeeds, then `Valid` is printed to standard out; " 
        ++ "else, `Invalid` is printed. "
        ++ "The special file - can be used to read from standard in."

main :: IO ()
main = do
    sourcePath <- execParser argParser
    contents <- if sourcePath == "-" then getContents else readFile sourcePath

    case parseOnlyMinilang (case sourcePath of "-" -> "stdin" ; x -> x) contents of
        Left e -> do
            putStrLn "Invalid"
            print e
        Right p -> do
            putStrLn "Valid\n"
            putStrLn "Pretty print:"
            let roundT = render (pretty p)
            putStrLn roundT

            case parseOnlyMinilang "roundtrip" (pack roundT) of
                Left e -> do
                    putStrLn "roundtrip failed"
                    print e
                Right e -> do
                    putStrLn "roundtrip succeeded"
                    putStrLn (render (pretty e))

            case runExcept (typecheckProgram p) of
                Left (Ann pos e') -> do
                    putStrLn "type error:"
                    print e'
                    putStrLn "at:"
                    print (srcStart pos)
                Right p' -> do
                    let c = translateProgramMain p'
                    putStrLn (render (CP.pretty c))
