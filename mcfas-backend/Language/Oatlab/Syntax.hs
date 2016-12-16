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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Oatlab.Syntax where

import Data.Annotation
import Data.HFunctor
import Data.Reflection

import Data.Functor.Compose
import Data.Proxy

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
  deriving (Eq, Ord, Read, Show)

type instance Demote' ('KProxy :: KProxy AstNode) = AstNode

instance Reflect 'ProgramDeclNode where reflect _ = ProgramDeclNode
instance Reflect 'TopLevelDeclNode where reflect _ = TopLevelDeclNode
instance Reflect 'VarDeclNode where reflect _ = VarDeclNode
instance Reflect 'StatementNode where reflect _ = StatementNode
instance Reflect 'ExpressionNode where reflect _ = ExpressionNode
instance Reflect 'IdentifierNode where reflect _ = IdentifierNode

-- | Singletons for 'AstNode'.
data AstNodeS :: AstNode -> * where
  ProgramDeclNodeS :: AstNodeS 'ProgramDeclNode
  TopLevelDeclNodeS :: AstNodeS 'TopLevelDeclNode
  VarDeclNodeS :: AstNodeS 'VarDeclNode
  StatementNodeS :: AstNodeS 'StatementNode
  ExpressionNodeS :: AstNodeS 'ExpressionNode
  IdentifierNodeS :: AstNodeS 'IdentifierNode

{- for copy pasta
  ProgramDeclNodeS
  TopLevelDeclNodeS
  VarDeclNodeS
  StatementNodeS
  ExpressionNodeS
  IdentifierNodeS
-}

type instance DemoteS' ('KProxy :: KProxy AstNode) = AstNodeS

instance ReflectS 'ProgramDeclNode where reflectS _ = ProgramDeclNodeS
instance ReflectS 'TopLevelDeclNode where reflectS _ = TopLevelDeclNodeS
instance ReflectS 'VarDeclNode where reflectS _ = VarDeclNodeS
instance ReflectS 'StatementNode where reflectS _ = StatementNodeS
instance ReflectS 'ExpressionNode where reflectS _ = ExpressionNodeS
instance ReflectS 'IdentifierNode where reflectS _ = IdentifierNodeS

-- | The higher-order functor that represents the signature of the syntax tree.
data OatlabAstF :: (AstNode -> *) -> AstNode -> * where
  -- | A list of top-level declarations is a program.
  --
  -- > topLevelDecl1
  -- > topLevelDecl2
  -- > topLevelDecl3
  ProgramDecl
    :: [ast 'TopLevelDeclNode]
    -- The top-level declarations that make up the program.
    -> OatlabAstF ast 'ProgramDeclNode

  -- | A function declaration is a top-level declaration.
  --
  -- > function foo(p1, p2, p3) {
  -- >   stmt;
  -- > }
  FunctionDecl
    :: ast 'IdentifierNode
    -- The name of the function.
    -> [ast 'VarDeclNode]
    -- The formal parameters of the function.
    -> [ast 'StatementNode]
    -- The body of the function.
    -> OatlabAstF ast 'TopLevelDeclNode

  -- | A while-loop is a statement.
  --
  -- > while expr {
  -- >   stmt;
  -- > }
  WhileLoop
    :: ast 'ExpressionNode
    -- The condition of the loop.
    -> [ast 'StatementNode]
    -- The body of the loop.
    -> OatlabAstF ast 'StatementNode

  -- | A for-loop is a statement.
  --
  -- > for var = expr {
  -- >   stmt;
  -- > }
  ForLoop
    :: ast 'VarDeclNode
    -- The variable bound by the loop.
    -> ast 'ExpressionNode
    -- The expression providing values to the variable.
    -> [ast 'StatementNode]
    -- The body of the loop.
    -> OatlabAstF ast 'StatementNode

  -- | A condition is a statement.
  --
  -- > if expr {
  -- >   thenStmt;
  -- > }
  -- > else {
  -- >   elseStmt;
  -- > }
  Branch
    :: ast 'ExpressionNode
    -- The expression to branch on.
    -> [ast 'StatementNode]
    -- The statements to execute if the condition is true.
    -> Maybe [ast 'StatementNode]
    -- The statements, if any, to execute if the expression is false.
    -> OatlabAstF ast 'StatementNode

  -- | Returning a value from a function is a statement.
  --
  -- e.g. @return x;@
  Return
    :: ast 'ExpressionNode
    -- The value to return.
    -> OatlabAstF ast 'StatementNode

  -- | Assigning to an lvalue is a statement.
  --
  -- e.g. @lhsExpr = rhsExpr;@
  Assignment
    :: ast 'ExpressionNode
    -- The expression to assign to.
    -- The expression must be "assignable" (an lvalue), which in Oatlab means
    -- it is either an indexing expression or a variable.
    -> ast 'ExpressionNode
    -- The expression whose value is assigned to the left-hand side.
    -> OatlabAstF ast 'StatementNode

  -- | Expressions can be used as statements.
  --
  -- e.g. @expr;@
  Expression
    :: ast 'ExpressionNode
    -- The expression to evaluate.
    -> OatlabAstF ast 'StatementNode

  -- | A variable is an expression.
  --
  -- e.g. @x@
  Var
    :: ast 'IdentifierNode
    -- The name of the variable.
    -> OatlabAstF ast 'ExpressionNode

  -- | A string is an expression.
  --
  -- e.g. @\"Hello, world!\"@
  StringLiteral
    :: String
    -- The string literal.
    -> OatlabAstF ast 'ExpressionNode

  -- | A number is an expression.
  --
  -- e.g. @4.0@
  NumericLiteral
    :: Double
    -- The numeric literal.
    -> OatlabAstF ast 'ExpressionNode

  -- | A binary operation is an expression.
  --
  -- @x + y@
  BinaryOperation
    :: BinaryOperator
    -- The operator relating the operands.
    -> ast 'ExpressionNode
    -- The left operand.
    -> ast 'ExpressionNode
    -- The right operand.
    -> OatlabAstF ast 'ExpressionNode

  -- | A function call is an expression.
  --
  -- @f(x)@
  Call
    :: ast 'ExpressionNode
    -- The function to call.
    -> [ast 'ExpressionNode]
    -- The arguments to pass.
    -> OatlabAstF ast 'ExpressionNode

  VarDecl
    :: ast 'IdentifierNode
    -> OatlabAstF ast 'VarDeclNode

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
--
-- This is in some sense redundant to 'reflectS', but is nicer because it
-- avoids a 'ReflectS' constraint.
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
  VarDecl _ -> VarDeclNodeS

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
    VarDecl ident ->
      VarDecl (f ident)

instance HEq f => HEq (OatlabAstF f) where
  (===) = (==)

instance HEq f => Eq (OatlabAstF f a) where
  ProgramDecl t1 == ProgramDecl t2
    = Compose t1 === Compose t2
  FunctionDecl x1 y1 z1 == FunctionDecl x2 y2 z2
    = x1 === x2 && Compose y1 === Compose y2 && Compose z1 === Compose z2
  WhileLoop e1 b1 == WhileLoop e2 b2
    = e1 === e2 && Compose b1 === Compose b2
  ForLoop v1 e1 b1 == ForLoop v2 e2 b2
    = v1 === v2 && e1 === e2 && Compose b1 === Compose b2
  Branch e1 bt1 be1 == Branch e2 bt2 be2
    = e1 === e2
    && Compose bt1 === Compose bt2
    && Compose (Compose <$> be1) === Compose (Compose <$> be2)
  Return e1 == Return e2
    = e1 === e2
  Assignment lhs1 rhs1 == Assignment lhs2 rhs2
    = lhs1 === lhs2 && rhs1 === rhs2
  Expression e1 == Expression e2
    = e1 === e2
  Var i1 == Var i2
    = i1 === i2
  BinaryOperation op1 opl1 opr1 == BinaryOperation op2 opl2 opr2
    = op1 == op2 && opl1 === opl2 && opr1 === opr2
  Call e1 ps1 == Call e2 ps2
    = e1 === e2 && Compose ps1 === Compose ps2
  Identifier s1 == Identifier s2
    = s1 == s2
  StringLiteral s1 == StringLiteral s2
    = s1 == s2
  NumericLiteral n1 == NumericLiteral n2
    = n1 == n2
  VarDecl i1 == VarDecl i2
    = i1 === i2
  _ == _ = False

-- | An unannotated Oatlab abstract syntax tree.
type OatlabAst = HFix OatlabAstF

-- | Decides whether an AST represents an lvalue.
--
-- /Higher-order F-algebra/
isLValueAlg :: OatlabAstF (K Bool) b -> K Bool b
isLValueAlg node = case collapseIndex node of
  ExpressionNodeS -> case node of
    BinaryOperation IndexBy (K True) _ -> K True
    Var _ -> K True
    _ -> K False
  _ -> K False

-- | Decides whether the AST represents an lvalue.
isLValue :: OatlabAst :~> K Bool
isLValue = hcata isLValueAlg

-- | Extracts the name of the declared variable from a variable declaration
-- node.
nameFromVarDecl :: OatlabAst 'VarDeclNode -> String
nameFromVarDecl (HFix (VarDecl (HFix (Identifier name)))) = name

-- | A uniformly annotated Oatlab abstract syntax tree.
type OatlabHAnnAst x = HAnnFix x OatlabAstF

-- | An indexed-annotated Oatlab abstract syntax tree.
type OatlabIAnnAst p = IAnnFix p OatlabAstF

instance HTraversable OatlabAstF where
  sequenceH = \case
    ProgramDecl (traverse getCompose -> topLevelDecls)
      -> ProgramDecl <$> topLevelDecls
    FunctionDecl (Compose name) (traverse getCompose -> vars) (traverse getCompose -> body)
      -> FunctionDecl <$> name <*> vars <*> body
    WhileLoop (Compose expr) (traverse getCompose -> body)
      -> WhileLoop <$> expr <*> body
    ForLoop (Compose var) (Compose expr) (traverse getCompose -> body)
      -> ForLoop <$> var <*> expr <*> body
    Branch
      (Compose expr)
      (traverse getCompose -> thenBody)
      (traverse (traverse getCompose) -> elseBody)
      -> Branch <$> expr <*> thenBody <*> elseBody
    Return (Compose expr)
      -> Return <$> expr
    Assignment (Compose lhs) (Compose rhs)
      -> Assignment <$> lhs <*> rhs
    Expression (Compose expr)
      -> Expression <$> expr
    Var (Compose name)
      -> Var <$> name
    BinaryOperation op (Compose opl) (Compose opr)
      -> BinaryOperation <$> pure op <*> opl <*> opr
    Call (Compose expr) (traverse getCompose -> params)
      -> Call <$> expr <*> params
    Identifier name -> pure (Identifier name)
    StringLiteral str -> pure (StringLiteral str)
    NumericLiteral num -> pure (NumericLiteral num)
    VarDecl (Compose ident) -> VarDecl <$> ident

-- | A binary operator.
data BinaryOperator
  -- | Add things.
  --
  -- @x + y@
  = Addition

  -- | Multiply things.
  --
  -- @x * y@
  | Multiplication

  -- | Divide things.
  --
  -- @x / y@
  | Division

  -- | Subtract things.
  --
  -- @x - y@
  | Subtraction

  -- | Compute the modulo, i.e. the remainder in division by.
  --
  -- @x % y@
  | Modulo

  -- | Check whether the left operand is less than the right operand.
  --
  -- @x < y@
  | LessThan

  -- | Check whether the left operand is less than or equal to the right.
  --
  -- @x <= y@
  | LessThanEqual

  -- | Check whether the left operand is greater than the right.
  --
  -- @x > y@
  | GreaterThan

  -- | Check whether the left operand is greater or equal to the right.
  --
  -- @x >= y@
  | GreaterThanEqual

  -- | Check whether the left operand is equal to the right.
  --
  -- @x == y@
  | Equal

  -- | Check whether the left operand is not equal to the right.
  --
  -- @x != y@
  | NotEqual

  -- | Compute the inclusive range between the two operands.
  --
  -- @x : y@
  | RangeToFrom

  -- | Index into another expression.
  --
  -- @x \@ y@
  | IndexBy
  deriving (Eq, Ord, Read, Show)
