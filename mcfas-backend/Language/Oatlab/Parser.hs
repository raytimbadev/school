{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Oatlab.Parser
( -- * Parsers for AST
  programDecl
, topLevelDecl
, statement
, whileLoop
, forLoop
, branch
, return
, assignment
, expressionStmt
, expression
, term
, literal
, variable
, varDecl
, identifier
  -- * Parsers for AST components
, numericLiteral
, stringLiteral
, comment
, keyword
, keyword'
, symbol
, symbol'
, identifier'
  -- * Types
, ParsedOatlabAst
, SrcSpan(..)
, module Text.Megaparsec.String
  -- * Misc
, lexeme
, sc
) where

import Data.Annotation
import Data.HFunctor
import Language.Oatlab.Syntax

import Control.Monad ( join )
import Prelude hiding ( return )
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Text.Read ( readMaybe )

-- | The syntax tree produced by the parser is uniformly annotated by source
-- spans.
type ParsedOatlabAst = IAnnFix (K SrcSpan) OatlabAstF

programDecl :: Parser (ParsedOatlabAst 'ProgramDeclNode)
programDecl = do
  (s, topLevelDecls) <- spanning (many topLevelDecl)
  pure $ HFix (IAnn (K s) (ProgramDecl topLevelDecls))

topLevelDecl :: Parser (ParsedOatlabAst 'TopLevelDeclNode)
topLevelDecl = choice [functionDecl]

functionDecl :: Parser (ParsedOatlabAst 'TopLevelDeclNode)
functionDecl = do
  (s, (name, params, stmts)) <- spanning $ do
    try (keyword' "function")
    name <- identifier
    params <- parens (commaSep varDecl)
    stmts <- braces (many statement)
    pure (name, params, stmts)

  pure $ HFix (IAnn (K s) (FunctionDecl name params stmts))

statement :: Parser (ParsedOatlabAst 'StatementNode)
statement
  = choice [whileLoop, forLoop, branch, return, assignment, expressionStmt]

whileLoop :: Parser (ParsedOatlabAst 'StatementNode)
whileLoop = do
  (s, (expr, stmts)) <- spanning $ do
    try (keyword' "while")
    expr <- expression
    stmts <- braces (many statement)
    pure (expr, stmts)

  pure $ HFix (IAnn (K s) (WhileLoop expr stmts))

forLoop :: Parser (ParsedOatlabAst 'StatementNode)
forLoop = do
  (s, (var, expr, stmts)) <- spanning $ do
    try (keyword' "for")
    var <- varDecl
    symbol' "<-"
    expr <- expression
    stmts <- braces (many statement)
    pure (var, expr, stmts)

  pure $ HFix (IAnn (K s) (ForLoop var expr stmts))

branch :: Parser (ParsedOatlabAst 'StatementNode)
branch = do
  (s, (expr, thenBody, mElseBody)) <- spanning $ do
    try (keyword' "if")
    expr <- expression
    thenBody <- braces (many statement)
    mElseBody <- optional $ do
      try (keyword' "else")
      braces (many statement)
    pure (expr, thenBody, mElseBody)

  pure $ HFix (IAnn (K s) (Branch expr thenBody mElseBody))

return :: Parser (ParsedOatlabAst 'StatementNode)
return = do
  (s, expr) <- spanning $ do
    try (keyword' "return")
    expression <* semi

  pure $ HFix (IAnn (K s) (Return expr))

assignment :: Parser (ParsedOatlabAst 'StatementNode)
assignment = do
  (s, (lhs, rhs)) <- spanning $ do
    lhs <- try (expression <* equals)
    rhs <- expression
    semi
    pure (lhs, rhs)

  pure $ HFix (IAnn (K s) (Assignment lhs rhs))

expressionStmt :: Parser (ParsedOatlabAst 'StatementNode)
expressionStmt = do
  expr <- expression
  semi
  pure $ HFix (IAnn (K (unK (topAnnI expr))) (Expression expr))

expression :: Parser (ParsedOatlabAst 'ExpressionNode)
expression = makeExprParser term operators where
  operators =
    [ [ call
      , binary IndexBy "@"
      ]
    , [ binary Multiplication "*"
      , binary Division "*"
      ]
    , [ binary Modulo "%"
      ]
    , [ binary Addition "+"
      , binary Subtraction "-"
      ]
    , [ binary RangeToFrom ":"
      ]
    , [ binary LessThan "<"
      , binary LessThanEqual "<="
      , binary GreaterThan ">"
      , binary GreaterThanEqual ">="
      , binary Equal "=="
      , binary NotEqual "!="
      ]
    ]

  binary
    :: BinaryOperator
    -> String
    -> Operator Parser (ParsedOatlabAst 'ExpressionNode)
  binary op name = InfixL $ do
    symbol' name
    pure $ \left right
      -> HFix $ IAnn
        (K $ joinSpan (unK $ topAnnI left) (unK $ topAnnI right))
        (BinaryOperation op left right)

  call :: Operator Parser (ParsedOatlabAst 'ExpressionNode)
  call = Postfix $ do
    (s, args) <- spanning $ do
      between (try $ symbol "(") (symbol ")") (expression `sepBy` comma)

    pure $ \e
      -> HFix $ IAnn
        (K $ joinSpan (unK $ topAnnI e) s)
        (Call e args)

term :: Parser (ParsedOatlabAst 'ExpressionNode)
term = parens expression <|> literal <|> variable

literal :: Parser (ParsedOatlabAst 'ExpressionNode)
literal
  = lexeme
  $ (bro NumericLiteral numericLiteral)
  <|> (bro StringLiteral stringLiteral) where
    bro
      :: (forall f. a -> OatlabAstF f 'ExpressionNode)
      -> Parser a
      -> Parser (ParsedOatlabAst 'ExpressionNode)
    bro ctor p = do
      (s, x) <- spanning p
      pure $ HFix (IAnn (K s) (ctor x))

variable :: Parser (ParsedOatlabAst 'ExpressionNode)
variable = do
  name <- identifier
  pure $ HFix (IAnn (K (unK (topAnnI name))) (Var name))

varDecl :: Parser (ParsedOatlabAst 'VarDeclNode)
varDecl = do
  name <- identifier
  pure $ HFix (IAnn (K (unK (topAnnI name))) (VarDecl name))

identifier :: Parser (ParsedOatlabAst 'IdentifierNode)
identifier = do
  (s, ident) <- spanning identifier'
  pure $ HFix (IAnn (K s) (Identifier ident))

-- | A span of text.
data SrcSpan
  = SrcSpan
    { spanStart :: SourcePos
    , spanEnd :: SourcePos
    }

numericLiteral :: Parser Double
numericLiteral
  = join $ fmap
    (
      maybe (fail "can't parse number") pure
      . readMaybe
    )
    (digits ++! dot ++! digits) where
      digits = some digitChar
      pl ++! pr = (++) <$> pl <*> pr
      dot = string "."

stringLiteral :: Parser String
stringLiteral = try (string "\"") *> manyTill anyChar (string "\"")

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
sc = skipMany (choice [() <$ spaceChar, comment])

-- | Comments
--
-- @\/\* this is a comment \*\/@
comment :: Parser ()
comment = try (string "/*") *> manyTill anyChar (string "*/") *> pure ()

-- | Keywords are parsed the same way as identifiers, but must equal a given
-- string rather than be arbitrary.
keyword :: String -> Parser String
keyword s = lexeme (try (string s) <* notFollowedBy alphaNumChar)

keyword' :: String -> Parser ()
keyword' = (() <$) . keyword

symbol :: String -> Parser String
symbol s = lexeme (try (string s) <* notFollowedBy symbolChar)

symbol' :: String -> Parser ()
symbol' = (() <$) . symbol

reservedWords :: [String]
reservedWords =
  [ "function"
  , "for"
  , "while"
  , "if"
  , "return"
  , "else"
  ]

-- | Parser that succeeds only if a reserved word is matched.
someReservedWord :: Parser ()
someReservedWord = () <$ choice ps where
  ps = (\s -> string s <* notFollowedBy identTail) <$> reservedWords

identifier' :: Parser String
identifier'
  = notFollowedBy someReservedWord *> lexeme (p <* notFollowedBy identTail) where
    p = (:) <$> identHead <*> many identTail

identHead :: Parser Char
identHead = letterChar

identTail :: Parser Char
identTail = alphaNumChar

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` comma)

comma :: Parser ()
comma = () <$ lexeme (string ",")

semi :: Parser ()
semi = () <$ lexeme (string ";")

equals :: Parser ()
equals = symbol' "="
