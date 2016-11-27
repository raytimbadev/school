{-|
Module      : Language.Oatlab.Syntax
Description : The Oatlab syntax tree
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Oatlab.Syntax where

import Data.Annotation
import Data.HFunctor

-- | Kind of indices for nodes in the Oatlab AST.
data AstNode
  = ProgramDeclNode
  -- ^ The unique element in the tree representing the whole program has this
  -- index.
  | TopLevelDeclNode
  -- ^ All top-level declarations have this index.
  | VarDeclNode
  -- ^ Each parameter declaration in a function has this index.
  | StatementNode
  -- ^ Each statement in a function body has this index.
  | ExpressionNode
  -- ^ Each expression has this index.
  | IdentifierNode
  -- ^ Each identifier has this index.

-- | Singletons for 'AstNode'.
data AstNodeS :: AstNode -> * where
  ProgramDeclNodeS :: AstNodeS 'ProgramDeclNode
  TopLevelDeclNodeS :: AstNodeS 'TopLevelDeclNode
  VarDeclNodeS :: AstNodeS 'VarDeclNode
  StatementNodeS :: AstNodeS 'StatementNode
  ExpressionNodeS :: AstNodeS 'ExpressionNode
  IdentifierNodeS :: AstNodeS 'IdentifierNode

-- | The higher-order functor that represents the signature of the syntax tree.
data OatlabAstF :: (AstNode -> *) -> AstNode -> * where
  -- | A list of top-level declarations is a program.
  ProgramDecl
    :: [ast 'TopLevelDeclNode]
    -- The top-level declarations that make up the program.
    -> OatlabAstF ast 'ProgramDeclNode

  -- | A function declaration is a top-level declaration.
  FunctionDecl
    :: ast 'IdentifierNode
    -- The name of the function.
    -> [ast 'IdentifierNode]
    -- The formal parameters of the function.
    -> [ast 'StatementNode]
    -- The body of the function.
    -> OatlabAstF ast 'TopLevelDeclNode

  -- | A while-loop is a statement.
  WhileLoop
    :: ast 'ExpressionNode
    -- The condition of the loop.
    -> [ast 'StatementNode]
    -- The body of the loop.
    -> OatlabAstF ast 'StatementNode

  -- | A for-loop is a statement.
  ForLoop
    :: ast 'VarDeclNode
    -- The variable bound by the loop.
    -> ast 'ExpressionNode
    -- The expression providing values to the variable.
    -> [ast 'StatementNode]
    -- The body of the loop.
    -> OatlabAstF ast 'StatementNode

  -- | A condition is a statement.
  Branch
    :: ast 'ExpressionNode
    -- The expression to branch on.
    -> [ast 'StatementNode]
    -- The statements to execute if the condition is true.
    -> Maybe [ast 'StatementNode]
    -- The statements, if any, to execute if the expression is false.
    -> OatlabAstF ast 'StatementNode

  -- | Returning a value from a function is a statement.
  Return
    :: ast 'ExpressionNode
    -- The value to return.
    -> OatlabAstF ast 'StatementNode

  Assignment
    :: ast 'ExpressionNode
    -- The expression to assign to.
    -- The expression must be "assignable" (an lvalue), which in Oatlab means
    -- it is either an indexing expression or a variable.
    -> ast 'ExpressionNode
    -- The expression whose value is assigned to the left-hand side.
    -> OatlabAstF ast 'StatementNode

  -- | Expressions can be used as statements.
  Expression
    :: ast 'ExpressionNode
    -- The expression to evaluate.
    -> OatlabAstF ast 'StatementNode

  -- | A variable is an expression.
  Var
    :: ast 'IdentifierNode
    -- The name of the variable.
    -> OatlabAstF ast 'ExpressionNode

  -- | A string is an expression.
  StringLiteral
    :: String
    -- The string literal.
    -> OatlabAstF ast 'ExpressionNode

  -- | A number is an expression.
  NumericLiteral
    :: Double
    -- The numeric literal.
    -> OatlabAstF ast 'ExpressionNode

  -- | A binary operation is an expression.
  BinaryOperation
    :: BinaryOperator
    -- The operator relating the operands.
    -> ast 'ExpressionNode
    -- The left operand.
    -> ast 'ExpressionNode
    -- The right operand.
    -> OatlabAstF ast 'ExpressionNode

  -- | A function call is an expression.
  Call
    :: ast 'ExpressionNode
    -- The function to call.
    -> [ast 'ExpressionNode]
    -- The arguments to pass.
    -> OatlabAstF ast 'ExpressionNode

  -- | Strings are identifiers.
  Identifier
    :: String
    -- The identifier's value.
    -> OatlabAstF ast 'IdentifierNode

{- Handy patterns for copy-pasta.
  ProgramDecl topLevelDecls
  FunctionDecl name params body
  WhileLoop expr body
  ForLoop var expr body
  Branch expr thenBody mElseBody
  Return expr
  Assignment lhs rhs
  Expression expr
  Var name
  StringLiteral str
  NumericLiteral num
  BinaryOperation op opl opr
  Call expr args
  Identifier str
-}

-- | Collapses an 'OatlabAstF' into an 'AstNodeS' preserving only the type
-- index. The reason you'd want to do this is if you only care about the index
-- and have extremely repetitive matches that depend only on the index.
collapseIndex
  :: forall (f :: AstNode -> *) (a :: AstNode). OatlabAstF f a -> AstNodeS a
collapseIndex = \case
  ProgramDecl _ -> ProgramDeclNodeS
  FunctionDecl _ _ _ -> TopLevelDeclNodeS
  WhileLoop _ _ -> StatementNodeS
  ForLoop _ _ _ -> StatementNodeS
  Branch _ _ _ -> StatementNodeS
  Return _ -> StatementNodeS
  Assignment _ _ -> StatementNodeS
  Expression _ -> StatementNodeS
  Var _ -> ExpressionNodeS
  StringLiteral _ -> ExpressionNodeS
  NumericLiteral _ -> ExpressionNodeS
  BinaryOperation _ _ _ -> ExpressionNodeS
  Call _ _ -> ExpressionNodeS
  Identifier _ -> IdentifierNodeS

-- | OatlabAstF is a higher-order functor.
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
    Assignment lhs rhs ->
      Assignment (f lhs) (f rhs)
    Expression expr ->
      Expression (f expr)
    Var ident ->
      Var (f ident)
    BinaryOperation op opl opr ->
      BinaryOperation op (f opl) (f opr)
    Call expr exprs ->
      Call (f expr) (f <$> exprs)
    Identifier name ->
      Identifier name
    StringLiteral str ->
      StringLiteral str
    NumericLiteral num ->
      NumericLiteral num

-- | An unannotated Oatlab abstract syntax tree.
type OatlabAst = HFix OatlabAstF

-- | An unindexed-annotated Oatlab abstract syntax tree.
type OatlabHAnnAst x = HAnnFix x OatlabAstF

-- | An indexed-annotated Oatlab abstract syntax tree.
type OatlabIAnnAst p = IAnnFix p OatlabAstF

instance HTraversable OatlabAstF where
  -- traverseH
  --   :: forall
  --     (m :: * -> *)
  --     (f :: AstNode -> *)
  --     (a :: AstNode).
  --     Applicative m
  --     => OatlabAstF (MonadH m f) a
  --     -> m (OatlabAstF f a)
  sequenceH = \case
    ProgramDecl (traverse unMonadH -> topLevelDecls)
      -> ProgramDecl <$> topLevelDecls
    FunctionDecl (MonadH name) (traverse unMonadH -> vars) (traverse unMonadH -> body)
      -> FunctionDecl <$> name <*> vars <*> body
    WhileLoop (MonadH expr) (traverse unMonadH -> body)
      -> WhileLoop <$> expr <*> body
    ForLoop (MonadH var) (MonadH expr) (traverse unMonadH -> body)
      -> ForLoop <$> var <*> expr <*> body
    Branch
      (MonadH expr)
      (traverse unMonadH -> thenBody)
      (traverse (traverse unMonadH) -> elseBody)
      -> Branch <$> expr <*> thenBody <*> elseBody
    Return (MonadH expr)
      -> Return <$> expr
    Assignment (MonadH lhs) (MonadH rhs)
      -> Assignment <$> lhs <*> rhs
    Expression (MonadH expr)
      -> Expression <$> expr
    Var (MonadH name)
      -> Var <$> name
    BinaryOperation op (MonadH opl) (MonadH opr)
      -> BinaryOperation <$> pure op <*> opl <*> opr
    Call (MonadH expr) (traverse unMonadH -> params)
      -> Call <$> expr <*> params
    Identifier name -> pure (Identifier name)
    StringLiteral str -> pure (StringLiteral str)
    NumericLiteral num -> pure (NumericLiteral num)

-- | A binary operator.
data BinaryOperator
  -- | Add things.
  = Addition
  -- | Multiply things.
  | Multiplication
  -- | Divide things.
  | Division
  -- | Subtract things.
  | Subtraction
  -- | Compute the modulo, i.e. the remainder in division by.
  | Modulo
  -- | Check whether the left operand is less than the right operand.
  | LessThan
  -- | Check whether the left operand is less than or equal to the right.
  | LessThanEqual
  -- | Check whether the left operand is greater than the right.
  | GreaterThan
  -- | Check whether the left operand is greater or equal to the right.
  | GreaterThanEqual
  -- | Check whether the left operand is equal to the right.
  | Equal
  -- | Check whether the left operand is not equal to the right.
  | NotEqual
  -- | Compute the inclusive range between the two operands.
  | RangeToFrom
