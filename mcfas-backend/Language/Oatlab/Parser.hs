{-# LANGUAGE DataKinds #-}

module Language.Oatlab.Parser where

import Data.Annotation
import Data.HFunctor
import Language.Oatlab.Syntax

import Control.Monad ( void )
import Prelude hiding ( return )
import Text.Megaparsec
import Text.Megaparsex.Expr
import Text.Megaparsec.String

-- | The syntax tree produced by the parser is uniformly annotated by source
-- spans.
type ParsedOatlabAst = IAnnFix (K SrcSpan) OatlabAstF

programDecl :: Parser (ParsedOatlabAst 'ProgramDeclNode)
programDecl = do
  (span, topLevelDecls) <- spanning (many topLevelDecl)
  -- pure $ HFix (IAnn (K span) (HFix (ProgramDecl topLevelDecls)))
  pure $ HFix (IAnn (K span) (ProgramDecl topLevelDecls))

topLevelDecl :: Parser (ParsedOatlabAst 'TopLevelDeclNode)
topLevelDecl = choice [functionDecl]

functionDecl :: Parser (ParsedOatlabAst 'TopLevelDeclNode)
functionDecl = do
  (fnSpan, (name, params, stmts)) <- spanning $ do
    try (keyword "function")
    name <- identifier
    params <- parens (commaSep varDecl)
    stmts <- braces (many statement)
    pure (name, params, stmts)

  pure $ HFix (IAnn (K fnSpan) (FunctionDecl name params stmts))

statement :: Parser (ParsedOatlabAst 'StatementNode)
statement
  = choice [whileLoop, forLoop, branch, return, assignment, expressionStmt]

whileLoop :: Parser (ParsedOatlabAst 'StatementNode)
whileLoop = do
  (span, (expr, stmts)) <- spanning $ do
    try (keyword "while")
    expr <- expression
    stmts <- braces (many statement)
    pure (expr, stmts)

  pure $ HFix (IAnn (K span) (WhileLoop expr stmts))

forLoop :: Parser (ParsedOatlabAst 'StatementNode)
forLoop = do
  (span, (var, expr, stmts)) <- spanning $ do
    try (keyword "for")
    var <- varDecl
    symbol "<-"
    expr <- expression
    stmts <- braces (many statement)
    pure (var, expr, stmts)

  pure $ HFix (IAnn (K span) (ForLoop var expr stmts))

branch :: Parser (ParsedOatlabAst 'StatementNode)
branch = do
  (span, (expr, thenBody, mElseBody)) <- spanning $ do
    try (keyword "if")
    expr <- expression
    thenBody <- braces (many statement)
    mElseBody <- optional $ do
      try (keyword "else")
      braces (many statement)
    pure (expr, thenBody, mElseBody)

  pure $ HFix (IAnn (K span) (Branch expr thenBody mElseBody))

return :: Parser (ParsedOatlabAst 'StatementNode)
return = do
  (span, expr) <- spanning $ do
    try (keyword "return")
    expression

  pure $ HFix (IAnn (K span) (Return expr))

assignment :: Parser (ParsedOatlabAst 'StatementNode)
assignment = do
  (span, (lhs, rhs)) <- spanning $ do
    lhs <- expression
    symbol "="
    rhs <- expression
    symbol ";"
    pure (lhs, rhs)

  pure $ HFix (IAnn (K span) (Assignment lhs rhs))

expressionStmt :: Parser (ParsedOatlabAst 'StatementNode)
expressionStmt = do
  expr <- expression
  pure $ HFix (IAnn (K (unK (topAnnI expr))) (Expression expr))

expression :: Parser (ParsedOatlabAst 'ExpressionNode)
expression = _

varDecl :: Parser (ParsedOatlabAst 'VarDeclNode)
varDecl = do
  name <- identifier <* ws
  pure $ HFix (IAnn (K (unK (topAnnI name))) (VarDecl name))

identifier :: Parser (ParsedOatlabAst 'IdentifierNode)
identifier = do
  (span, ident) <- spanning identifier'
  pure $ HFix (IAnn (K span) (Identifier ident))

-- | A span of text.
data SrcSpan
  = SrcSpan
    { spanStart :: SourcePos
    , spanEnd :: SourcePos
    }

-- | Runs a parser and also returns the span consumed by the parser.
spanning :: Parser a -> Parser (SrcSpan, a)
spanning p = do
  start <- getPosition
  x <- p
  end <- getPosition
  pure (SrcSpan start end, x)

-- | Combine two spans into a span that spans both spans.
--
-- Remark: the spans must be given in order.
joinSpan :: SrcSpan -> SrcSpan -> SrcSpan
joinSpan (SrcSpan start _) (SrcSpan _ end) = SrcSpan start end

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Runs the given parser followed by the space consumer.
lexeme :: Parser a -> Parser a
lexeme = (<* sc)

-- | Space consumer. Eats up as much whitespace and comments as possible.
sc :: Parser ()
sc = pure () <* many (choice [ws', comment])

-- | Mandatory whitespace.
ws' :: Parser ()
ws' = skipSome space

-- | Optional whitespace.
ws :: Parser ()
ws = skipMany space

-- | Comments
--
-- @\/\* this is a comment \*\/@
comment :: Parser ()
comment = try (string "/*") *> manyTill anyChar (string "*/") *> pure ()

-- | Keywords are parsed the same way as identifiers, but must equal a given
-- string rather than be arbitrary.
keyword :: String -> Parser String
keyword s = lexeme (try (string s) <* notFollowedBy alphaNumChar)

symbol :: String -> Parser String
symbol s = lexeme (try (string s) <* notFollowedBy symbolChar)

reservedWords :: [String]
reservedWords =
  [ "function"
  , "for"
  , "while"
  , "if"
  , "return"
  , "else"
  ]

identifier' :: Parser String
identifier' = lexeme p where
  p = try ((:) <$> identHead <*> many identTail) <* notFollowedBy identTail

identHead :: Parser Char
identHead = letterChar

identTail :: Parser Char
identTail = alphaNumChar

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` comma)

comma :: Parser String
comma = symbol ","
