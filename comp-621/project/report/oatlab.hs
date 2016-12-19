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
  -- > for var <- expr {
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
