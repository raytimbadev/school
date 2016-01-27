module Main where

import Language.Minilang

import Data.Text.IO ( readFile )
import Prelude hiding ( readFile )
import Options.Applicative

argParser :: ParserInfo String
argParser = info sourcePath details where
    sourcePath = argument str (metavar "FILE")
    details = header h <> progDesc p
    h = "minilangcheck - checks the syntax of a minilang program"
    p = "The specified FILE is loaded and parsed. "
        ++ "If the parse succeeds, then `Valid` is printed to standard out; " 
        ++ "else, `Invalid` is printed."

main :: IO ()
main = do
    sourcePath <- execParser argParser 
    contents <- readFile sourcePath
    case parseOnlyMinilang sourcePath contents of
        Left e -> do
            putStrLn "Invalid"
            print e
        Right e -> do
            putStrLn "Valid"
            putStrLn "Pretty print:"
            putStr $ pretty e
