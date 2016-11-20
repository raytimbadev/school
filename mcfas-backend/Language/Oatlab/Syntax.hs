{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Oatlab.Syntax where

import Data.Kind
import Text.PrettyPrint

data AstNode
  = ProgramDeclNode
  | TopLevelDeclNode
  | VarDeclNode
  | StatementNode
  | ExpressionNode
  | IdentifierNode

-- | The higher-order functor that represents the signature of the syntax tree.
data OatlabAstF :: (AstNode -> *) -> AstNode -> * where
  -- | A list of top-level declarations is a program.
  ProgramDecl
    :: [ast 'TopLevelDeclNode]
    -> OatlabAstF ast 'ProgramDeclNode

  -- | A function declaration is a top-level declaration.
  FunctionDecl
    -- | The name of the function.
    :: ast 'IdentifierNode
    -- | The formal parameters of the function.
    -> [ast 'IdentifierNode]
    -- | The body of the function.
    -> [ast 'StatementNode]
    -> OatlabAstF ast 'TopLevelDeclNode

  -- | A while-loop is a statement.
  WhileLoop
    -- | The condition of the loop.
    :: ast 'ExpressionNode
    -- | The body of the loop.
    -> [ast 'StatementNode]
    -> OatlabAstF ast 'StatementNode

  -- | A for-loops is a statement.
  ForLoop
    -- | The variable bound by the loop.
    :: ast 'IdentifierNode
    -- | The expression providing values to the variable.
    -> ast 'ExpressionNode
    -- | The body of the loop.
    -> [ast 'StatementNode]
    -> OatlabAstF ast 'StatementNode

  -- | A condition is a statement.
  Branch
    -- | The expression to branch on.
    :: ast 'ExpressionNode
    -- | The statements to execute if the condition is true.
    -> [ast 'StatementNode]
    -- | The statements, if any, to execute if the expression is false.
    -> Maybe [ast 'StatementNode]
    -> OatlabAstF ast 'StatementNode

  -- | Returning a value from a function is a statement.
  Return
    -- | The value to return.
    :: ast 'ExpressionNode
    -> OatlabAstF ast 'StatementNode

  Assignment
    -- | The expression to assign to.
    -- The expression must be "assignable" (an lvalue), which in Oatlab means
    -- it is either an indexing expression or a variable.
    :: ast 'ExpressionNode
    -- | The expression whose value is assigned to the left-hand side.
    -> ast 'ExpressionNode
    -> OatlabAstF ast 'StatementNode

  -- | Expressions can be used as statements.
  Expression
    -- | The expression to evaluate.
    :: ast 'ExpressionNode
    -> OatlabAstF ast 'StatementNode

  -- | A variable is an expression.
  Var
    -- | The name of the variable.
    :: ast 'IdentifierNode
    -> OatlabAstF ast 'ExpressionNode

  -- | A string is an expression.
  StringLiteral
    -- | The string literal.
    :: String
    -> OatlabAstF ast 'ExpressionNode

  -- | A number is an expression.
  NumericLiteral
    -- | The numeric literal.
    :: Double
    -> OatlabAstF ast 'ExpressionNode

  -- | A binary operation is an expression.
  BinaryOperation
    :: BinaryOperator
    -> ast 'ExpressionNode
    -> ast 'ExpressionNode
    -> OatlabAstF ast 'ExpressionNode

  -- | A function call is an expression.
  Call
    -- | The function to call.
    :: ast 'ExpressionNode
    -- | The arguments to pass.
    -> [ast 'ExpressionNode]
    -> OatlabAstF ast 'ExpressionNode

  -- | Strings are identifiers.
  Identifier
    -- | The identifier's value.
    :: String
    -> OatlabAstF ast 'IdentifierNode

newtype HFix (h :: (k -> *) -> k -> *) (a :: k)
  = HFix { unHFix :: h (HFix h) a }

-- | An unannotated Oatlab abstract syntax tree.
type OatlabAst = HFix OatlabAstF

data HAnn (x :: *) (h :: (k -> *) -> k -> *) (f :: k -> *) (a :: k)
  = HAnn
    { ann :: !x
    -- ^ The annotation.
    , bare :: h f a
    -- ^ The annotated data.
    }

data PAnn (p :: k -> *) (h :: (k -> *) -> k -> *) (f :: k -> *) (a :: k)
  = PAnn
    { annP :: !(p a)
    , bareP :: h f a
    }

type family MyAnn (node :: AstNode) :: * where
   MyAnn 'ProgramDeclNode = ()
   MyAnn 'TopLevelDeclNode = ()
   MyAnn 'VarDeclNode = ()
   MyAnn 'StatementNode = String
   MyAnn 'ExpressionNode = ()
   MyAnn 'IdentifierNode = ()

newtype MyAnnW (node :: AstNode) = MyAnnW { unMyAnnW :: MyAnn node }

type HAnnFix x h = HFix (HAnn x h)

type PAnnFix p h = HFix (PAnn p h)

-- | An annotated Oatlab abstract syntax tree.
type OatlabAnnAst x = HAnnFix x OatlabAstF

type OatlabMyAnnAst = PAnnFix MyAnnW OatlabAstF

-- | Natural transformations.
type f :~> g = forall a. f a -> g a

class HFunctor (h :: (k -> *) -> k -> *) where
  hfmap :: (f :~> g) -> h f :~> h g

instance HFunctor OatlabAstF where
  hfmap f hf = case hf of
    ProgramDecl topLevelDecls ->
      ProgramDecl (f <$> topLevelDecls)
    FunctionDecl ident params body ->
      FunctionDecl (f ident) (f <$> params) (f <$> body)
    WhileLoop expr body ->
      WhileLoop (f expr) (f <$> body)
    ForLoop ident expr body ->
      ForLoop (f ident) (f expr) (f <$> body)
    Branch expr body1 body2 ->
      Branch (f expr) (f <$> body1) (fmap f <$> body2)
    Return expr ->
      Return (f expr)
    Var ident ->
      Var (f ident)
    BinaryOperation op opl opr ->
      BinaryOperation op (f opl) (f opr)
    Call expr exprs ->
      Call (f expr) (f <$> exprs)
    Identifier name ->
      Identifier name
    Assignment lhs rhs ->
      Assignment (f lhs) (f rhs)
    Expression expr ->
      Expression (f expr)
    StringLiteral str ->
      StringLiteral str
    NumericLiteral num ->
      NumericLiteral num

instance HFunctor h => HFunctor (HAnn x h) where
  hfmap f (HAnn a h) = HAnn a (hfmap f h)

-- | Higher-order catamorphism.
hcata :: HFunctor h => (h f :~> f) -> HFix h :~> f
hcata hAlg = hAlg . hfmap (hcata hAlg) . unHFix

{-
 - This is interesting. The choice of functor used in hcata has to do with the
 - type index, i.e. AstNode in our case. For example, if we fold with an
 - identity functor,
 -}

-- | Constant functor.
newtype K x a = K { unK :: x } deriving Functor

-- | Higher-order F-algebra for pretty-printing.
-- The annotations are ignored
ppHAlg :: OatlabAstF (K Doc) :~> K Doc
ppHAlg node = case node of
  ProgramDecl (map unK -> topLevelDecls) ->
    K (
      foldr (\next done -> next $+$ "" $+$ done) empty topLevelDecls
    )
  FunctionDecl (K ident) (map unK -> params) (map unK -> body) ->
    K (
      "function" <+> ident <> parens (hcat $ punctuate ", " params) <+> "{" $+$
      nest 4 (foldr ($+$) empty body) $+$
      "}"
    )
  WhileLoop (K expr) (map unK -> body) ->
    K (
      "while" <+> parens expr <+> "{" $+$
      nest 4 (foldr ($+$) empty body) $+$
      "}"
    )
  ForLoop (K ident) (K expr) (map unK -> body) ->
    K (
      "for" <+> ident <+> "=" <+> expr <+> "{" $+$
      nest 4 (foldr ($+$) empty body) $+$
      "}"
    )
  Branch (K expr) (map unK -> body1) body2 ->
    K (
      "if" <+> parens expr <+> "{" $+$
      nest 4 (foldr ($+$) empty body1) $+$
      "}" $+$ case body2 of
        Just (map unK -> body2') -> foldr ($+$) empty body2'
        Nothing -> empty
    )
  Return (K expr) ->
    K (
      "return" <+> expr
    )
  Var (K ident) -> K ident
  BinaryOperation op (K opl) (K opr) ->
    K (
      parens (opl <+> ppOp op <+> opr)
    )
  Call (K expr) (map unK -> exprs) ->
    K (
      parens expr <> parens (hcat $ punctuate ", " exprs)
    )
  Assignment (K lhs) (K rhs) -> K (lhs <+> "=" <+> rhs <> ";")
  Expression (K expr) -> K (expr <> ";")
  StringLiteral str -> K (doubleQuotes (text str))
  NumericLiteral num -> K (double num)
  Identifier name -> K (text name)

example :: OatlabAnnAst Int 'ProgramDeclNode
example
  = HFix (
    HAnn 1 (
      ProgramDecl [
        HFix (
          HAnn 2 (
            FunctionDecl
              ( HFix (HAnn 3 (Identifier "foo")) )
              [ HFix (HAnn 4 (Identifier "x")) ]
              [
                HFix (
                  HAnn 5 (
                    Return (
                      HFix (
                        HAnn 6 (
                          Var (
                            HFix (
                              HAnn 7 (
                                Identifier "x"
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ]
          )
        )
      ]
    )
  )

-- | Remove all annotations from a syntax tree.
strip :: OatlabAnnAst ann node -> OatlabAst node
strip = hcata (HFix . bare)

-- | Pretty-print an unannotated syntax tree.
ppAst :: OatlabAst node -> Doc
ppAst = unK . hcata ppHAlg

-- | Pretty-print an annotated syntax tree.
ppAnnAst :: OatlabAnnAst ann node -> Doc
ppAnnAst = unK . hcata (ppHAlg . bare)

data BinaryOperator
  = Addition
  | Multiplication
  | Division
  | Subtraction
  | Modulo
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Equal
  | NotEqual
  | RangeToFrom

ppOp :: BinaryOperator -> Doc
ppOp op = case op of
  Addition -> "+"
  Multiplication -> "*"
  Division -> "/"
  Subtraction -> "-"
  Modulo -> "%"
  LessThan -> "<"
  LessThanEqual -> "<="
  GreaterThan -> ">"
  GreaterThanEqual -> ">="
  Equal -> "=="
  NotEqual -> "!="
  RangeToFrom -> ":"
