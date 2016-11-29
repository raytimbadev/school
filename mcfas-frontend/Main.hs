{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Annotation ( bareI, stripI )
import Data.HFunctor ( hcata, unK )
import Language.Oatlab.Analysis
import Language.Oatlab.Analysis.ReachingDefinitions
import Language.Oatlab.Analysis.StatementNumbering
import Language.Oatlab.Pretty ( ppAlg, renderOatlab )
import Language.Oatlab.Parser

import Control.Monad ( forM_ )
import Control.Monad.State ( evalState )
import Control.Monad.Except ( runExcept )
import Prelude hiding ( return )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Text.Megaparsec ( parse, parseErrorPretty, eof )

main :: IO ()
main = do
  -- runAllTests

  input <- getContents
  errorPutStrLn "parsing"
  case parse (programDecl <* eof) "stdin" input of
    Left e -> do
      putStrLn (parseErrorPretty e)
      exitFailure
    Right ast -> do
      errorPutStrLn "parsing succeeded"
      errorPutStrLn "pretty-print:"
      putStrLn (renderOatlab $ unK $ hcata (ppAlg . bareI) $ ast)

      let initRDAst = evalState (initializeReachingDefinitions . stripI $ ast) 0

      case runForwardOatlabAnalysis reachingDefinitions initRDAst of
        FR (IMR (runExcept -> foo)) -> case foo of
          Left () -> do
            errorPutStrLn "fixed-point solver iterations exhauseted"
            exitFailure
          Right ast' -> do
            errorPutStrLn "analysis completed"
            putStrLn (renderOatlab $ unK $ hcata reachingDefinitionsPpAlg ast')

  errorPutStrLn "exited normally"

errorPutStrLn :: String -> IO ()
errorPutStrLn = hPutStrLn stderr

test :: String -> Parser (ParsedOatlabAst node) -> Either String String
test input p = case parse (p <* eof) "test" input of
  Left e -> Left (parseErrorPretty e)
  Right ast -> Right $ renderOatlab $ unK $ hcata (ppAlg . bareI) $ ast

displayTest :: String -> Parser (ParsedOatlabAst node) -> IO ()
displayTest input p = errorPutStrLn $ case test input p of
  Left e -> "failed: " ++ e
  Right ast -> "ok: " ++ ast

data SomeParser = forall node. SomeParser (Parser (ParsedOatlabAst node))

tests :: [ (String, SomeParser) ]
tests =
  [ ("someIdent", SomeParser identifier)
  , ("varDecl", SomeParser varDecl)
  , ("var", SomeParser variable)
  , ("3.0", SomeParser literal)
  , ("\"hello world\"", SomeParser literal)
  , ("5.400", SomeParser term)
  , ("\"hello\"", SomeParser term)
  , ("(5.4)", SomeParser term)
  , ("2.0 + 2.0", SomeParser expression)
  , ("f(a, b)", SomeParser expression)
  , ("(f)(x, y)", SomeParser expression)
  , ("2.0 + x + 4.3", SomeParser expression)
  , ("(x + 3.0) * foo", SomeParser expression)
  , ("returned + functioning", SomeParser expression)
  , ("f();", SomeParser expressionStmt)
  , ("2.0 + 2.0;", SomeParser expressionStmt)
  , ("f(a, b);", SomeParser expressionStmt)
  , ("(f)(x, y);", SomeParser expressionStmt)
  , ("2.0 + x + 4.3;", SomeParser expressionStmt)
  , ("(x + 3.0) * foo;", SomeParser expressionStmt)
  , ("a = b;", SomeParser assignment)
  , ("a@i = b@j;", SomeParser assignment)
  , ("a = f();", SomeParser assignment)
  , ("a = 2.0 + b;", SomeParser assignment)
  , ("a@i@j = f(g(4.0, x * y * z @ q));", SomeParser assignment)
  , ("a@b@c@d = (f + g)(f);", SomeParser assignment)
  , ("return x;", SomeParser return)
  , ("return returned;", SomeParser return)
  , ("if x { display(\"foo\") ; } else { exit(1.337) ; }", SomeParser branch)
  , ("if xyz + abc { foobar() + frobber @ j ; }", SomeParser branch)
  , ("for i <- 4.0 : 17.3 { disp(i) ; }", SomeParser forLoop)
  , ( "for q <- f(4.0 : 36.67, j + 17.0 * q @ k) { for h <- foo { disp (q @ h); }}"
    , SomeParser forLoop
    )
  ]

runAllTests :: IO ()
runAllTests = do
  forM_ tests $ \ (input, SomeParser p) -> do
    errorPutStrLn $ "trying: " ++ input
    displayTest input p
