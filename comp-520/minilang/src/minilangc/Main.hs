{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Minilang
import Language.Minilang.Typecheck
import Language.Minilang.SrcAnn

import Control.Monad.Except
import Data.Functor.Foldable
import qualified Data.Map.Strict as M
import Data.Text.IO ( readFile, getContents )
import Prelude hiding ( readFile, getContents )
import Options.Applicative
import Text.Show.Pretty

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

testEnv :: Env
testEnv = M.fromList
    [ ( "i", TyInt )
    , ( "s", TyString )
    , ( "r", TyReal )
    ]

main :: IO ()
main = do
    sourcePath <- execParser argParser
    contents <- if sourcePath == "-" then getContents else readFile sourcePath
    case parseOnlyExpr (case sourcePath of "-" -> "stdin" ; x -> x) contents of
        Left e -> do
            putStrLn "Invalid"
            print e
        Right e -> do
            putStrLn "Valid"
            putStrLn "Pretty print:"
            putStrLn $ pretty e
            putStrLn (ppShow e)
            case runExcept (typecheckExpr testEnv e) of
                Left (Ann pos e') -> do
                    putStrLn "type error:"
                    print e'
                    putStrLn "at:"
                    print (srcStart pos)
                Right e' -> do
                    putStrLn "type:"
                    print (exprType e')
