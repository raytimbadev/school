{-|
Module      : Language.Oatlab.Analysis
Description : Oatlab-specific analysis code
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Oatlab.Analysis where

import Control.Monad ( when )
import Data.Annotation ( IAnn(..), IAnnFix, topAnnI, mapTopAnnI )
import Data.HFunctor ( HFix(..), hcata, (:~>) )
import Language.Common.Analysis ( Analysis(..), Direction(..) )
import Language.Oatlab.Syntax

-- | An Oatlab analysis is an analysis over an indexed-annotated Oatlab AST.
type OatlabAnalysis f p = Analysis (IAnn p OatlabAstF) f p

-- | A dataflow analysis fundamentally operates on statements: those are the
-- nodes in the syntax tree where we attach annotations containing
-- approximations.
--
-- This type-level function delegates to the given type-level function @f@ for
-- all nodes except statements, where it requires a function type. These
-- functions are what we use to connect the input flow-set of a statement to
-- the output flow-set of the previous statement.
--
-- See 'chainStmts'.
type family StatementFlowAnalysis
  (node :: AstNode)
  (approx :: AstNode -> *)
  (f :: AstNode -> *) :: * where
    StatementFlowAnalysis 'ProgramDeclNode _ f
      = f 'ProgramDeclNode
    StatementFlowAnalysis 'TopLevelDeclNode _ f
      = f 'TopLevelDeclNode
    StatementFlowAnalysis 'VarDeclNode _ f
      = f 'VarDeclNode
    StatementFlowAnalysis 'StatementNode approx f
      = (approx 'StatementNode) -> f 'StatementNode
    StatementFlowAnalysis 'ExpressionNode _ f
      = f 'ExpressionNode
    StatementFlowAnalysis 'IdentifierNode _ f
      = f 'IdentifierNode

-- | A monadic computation that produces an indexed-annotated Oatlab AST.
--
-- This wrapper exists so that the type parameters can be uniformly presented
-- on the type-level, allowing partial application.
newtype IndexedMonadicResult
  (m :: * -> *)
  (p :: AstNode -> *)
  (node :: AstNode)
  = IMR { imResult :: m (OatlabIAnnAst p node) }

-- | A newtype wrapper that simulates the action of the 'StatementFlowAnalysis'
-- type-level function.
newtype StatementFlowResult
  (m :: * -> *)
  (p :: AstNode -> *)
  (approx :: AstNode -> *)
  (node :: AstNode)
  = FR
    { result :: StatementFlowAnalysis node approx (IndexedMonadicResult m p)
    }

-- | A class of monads for analyses.
-- This is just an error monad for raising an exception when the iterations are
-- exhausted for a fixpoint solution.
class MonadAnalysis m where
  -- | Raises an exception claiming that iterations are exhausted.
  iterationsExhausted :: m a
  -- TODO pass in the problematic tree, for error messages?

-- | Feed an approximation through a list of statements.
--
-- The initial approximation is fed to the first function to compute a new
-- statement node. This statement node's output set is the fed to the next
-- statement in the list, and so on, until the list is exhausted and the output
-- approximation of the last statement is computed.
chainStmts
  :: Monad m
  => (p 'StatementNode -> approx)
  -- ^ A strategy for extracting approximations from annotations.
  -> approx
  -- ^ The in-set for the initial statement.
  -> [approx -> IndexedMonadicResult m p 'StatementNode]
  -- ^ The statements to chain.
  -> m (approx, [OatlabIAnnAst p 'StatementNode])
chainStmts _ approx [] = pure (approx, [])
chainStmts extract approx (f:fs) = do
  stmt <- imResult (f approx)
  (approx', stmts) <- chainStmts extract (extract (topAnnI stmt)) fs
  pure (approx', stmt:stmts)

-- | Feed an approximation through a list of statements that have already been
-- chained, and reexecute the dataflow equation on each.
chainStmts'
  :: Monad m
  => (OatlabIAnnAst p 'StatementNode -> approx -> m approx)
  -> (approx -> p 'StatementNode -> m (p 'StatementNode))
  -> approx
  -> [OatlabIAnnAst p 'StatementNode]
  -> m (approx, [OatlabIAnnAst p 'StatementNode])
chainStmts' _ _ approx [] = pure (approx, [])
chainStmts' dataflow update approx (stmt:stmts) = do
  approx' <- dataflow stmt approx
  ann' <- update approx' (topAnnI stmt)
  (approx'', stmts') <- chainStmts' dataflow update approx' stmts
  pure (approx'', mapTopAnnI (const ann') stmt : stmts')

-- | Converts an 'OatlabAnalysis' into a higher-order F-algebra that performs
-- the action of that analysis locally.
analyzeForward
  :: (Monad m, MonadAnalysis m)
  => OatlabAnalysis
    (IAnnFix p OatlabAstF)
    p
    approx
    'StatementNode
    'StatementNode
    m
    'Forward
  -> IAnn p OatlabAstF (StatementFlowResult m p approx)
  :~> StatementFlowResult m p approx
analyzeForward analysis@(Analysis {..}) (IAnn ann node) = case node of

  ProgramDecl (map (imResult . result) -> topLevelDecls) -> FR $ IMR $ do
    topLevelDecls' <- sequenceA topLevelDecls
    let node' = ProgramDecl topLevelDecls'

    approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
    ann' <- analysisUpdateApproximation approx' ann

    pure $ HFix (IAnn ann' node')

  FunctionDecl
    (FR (IMR name))
    (map (imResult . result) -> params)
    (map result -> fstmts)
    -> FR $ IMR $ do
      name' <- name
      params' <- sequenceA params
      startingApprox <- analysisBoundaryApproximation
      (_, body') <- chainStmts analysisGetApproximation startingApprox fstmts
      let node' = FunctionDecl name' params' body'

      approx' <- analysisDataflow
        (IAnn ann node')
        (analysisGetApproximation ann)
      ann' <- analysisUpdateApproximation approx' ann

      pure $ HFix (IAnn ann' node')

  Assignment (FR (IMR lhs)) (FR (IMR rhs)) -> FR $ \inSet -> IMR $ do
    lhs' <- lhs
    rhs' <- rhs
    let node' = Assignment lhs' rhs'

    outSet <- analysisDataflow (IAnn ann node') inSet
    ann' <- analysisUpdateApproximation outSet ann

    pure $ HFix (IAnn ann' node')

  Return (FR (IMR expr)) -> FR $ \inSet -> IMR $ do
    expr' <- expr
    let node' = Return expr'

    outSet <- analysisDataflow (IAnn ann node') inSet
    ann' <- analysisUpdateApproximation outSet ann

    pure $ HFix (IAnn ann' node')

  Expression (FR (IMR expr)) -> FR $ \inSet -> IMR $ do
    expr' <- expr
    let node' = Expression expr'

    outSet <- analysisDataflow (IAnn ann node') inSet
    ann' <- analysisUpdateApproximation outSet ann

    pure $ HFix (IAnn ann' node')

  WhileLoop (FR (IMR expr)) (map result -> bodyFs) -> FR $ \approx -> IMR $ do
    when (analysisGetIterations ann == 0) iterationsExhausted

    expr' <- expr

    let stmtApprox = analysisGetApproximation . topAnnI

    (approx', body) <- chainStmts analysisGetApproximation approx bodyFs

    -- get the list of approximations, one for each stmt in the body
    let approxes = stmtApprox <$> body

    -- chain the statements a second time with the dataflow
    (_, body') <- chainStmts'
      (analysisDataflow . unHFix)
      analysisUpdateApproximation
      approx'
      body

    -- and get the list of approximations for the second run
    let approxes' = stmtApprox <$> body'

    let node' = WhileLoop expr' body'
    ann' <- analysisUpdateApproximation approx' ann

    -- if all lists of approximations are the same ...
    if all id (zipWith analysisApproximationEq approxes approxes')
    then do
      -- we found the fixed point, so we can just return this AST
      pure $ HFix (IAnn ann' node')
    else do
      -- decrease the number of iterations remaining in this node by one
      ann'' <- analysisUpdateIterations (subtract 1) ann'
      -- build a bona fide AST out of this node
      let ast = HFix (IAnn ann'' node')
      -- and recurse
      imResult (result (runForwardOatlabAnalysis analysis ast) approx)

  ForLoop (FR (IMR var)) (FR (IMR expr)) (map result -> bodyFs)
    -> FR $ \approx -> IMR $ do
      when (analysisGetIterations ann == 0) iterationsExhausted

      var' <- var
      expr' <- expr

      let stmtApprox = analysisGetApproximation . topAnnI

      (approx', body) <- chainStmts analysisGetApproximation approx bodyFs

      let node' = ForLoop var' expr' body

      approx'' <- analysisDataflow (IAnn ann node') approx'

      let approxes = stmtApprox <$> body

      (approx''', body') <- chainStmts'
        (analysisDataflow . unHFix)
        analysisUpdateApproximation
        approx''
        body

      let approxes' = stmtApprox <$> body'

      let node'' = ForLoop var' expr' body'

      ann' <- analysisUpdateApproximation approx''' ann

      if all id (zipWith analysisApproximationEq approxes approxes')
      then pure $ HFix (IAnn ann' node')
      else do
        ann'' <- analysisUpdateIterations (subtract 1) ann'
        let ast = HFix (IAnn ann'' node'')
        imResult (result (runForwardOatlabAnalysis analysis ast) approx''')

  Branch (FR (IMR expr)) (map result -> thenBodyFs) mElseBodyFs
    -> FR $ \approx -> IMR $ do
      expr' <- expr

      (thenApprox, thenBody) <- chainStmts
        analysisGetApproximation
        approx
        thenBodyFs

      (elseApprox, elseBody) <- case mElseBodyFs of
        Nothing -> pure (approx, Nothing)
        Just (map result -> elseBodyFs) -> do
          (elseApprox, stmts) <- chainStmts
            analysisGetApproximation
            approx
            elseBodyFs
          pure (elseApprox, Just stmts)

      outApprox <- analysisMerge thenApprox elseApprox
      ann' <- analysisUpdateApproximation outApprox ann
      let outApprox' = analysisGetApproximation ann'
      let node' = Branch expr' thenBody elseBody
      outApprox'' <- analysisDataflow (IAnn ann' node') outApprox' -- unnecessary?
      ann'' <- analysisUpdateApproximation outApprox'' ann'

      pure $ HFix (IAnn ann'' node')

  Var (FR (IMR ident)) -> FR $ IMR $ do
    ident' <- ident
    let node' = Var ident'

    approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
    ann' <- analysisUpdateApproximation approx' ann

    pure $ HFix (IAnn ann' node')

  StringLiteral s -> FR $ IMR $ do
    let node' = StringLiteral s

    approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
    ann' <- analysisUpdateApproximation approx' ann

    pure $ HFix (IAnn ann' node')

  NumericLiteral n -> FR $ IMR $ do
    let node' = NumericLiteral n

    approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
    ann' <- analysisUpdateApproximation approx' ann

    pure $ HFix (IAnn ann' node')

  Call (FR (IMR expr)) (map (imResult . result) -> args) -> FR $ IMR $ do
    expr' <- expr
    args' <- sequenceA args
    let node' = Call expr' args'

    approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
    ann' <- analysisUpdateApproximation approx' ann

    pure $ HFix (IAnn ann' node')

  VarDecl (FR (IMR ident)) -> FR $ IMR $ do
    ident' <- ident
    let node' = VarDecl ident'

    approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
    ann' <- analysisUpdateApproximation approx' ann

    pure $ HFix (IAnn ann' node')

  Identifier name -> FR $ IMR $ do
    let node' = Identifier name

    approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
    ann' <- analysisUpdateApproximation approx' ann

    pure $ HFix (IAnn ann' node')

  BinaryOperation op (FR (IMR opl)) (FR (IMR opr)) -> FR $ IMR $ do
    opl' <- opl
    opr' <- opr
    let node' = BinaryOperation op opl' opr'

    approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
    ann' <- analysisUpdateApproximation approx' ann

    pure $ HFix (IAnn ann' node')

-- | Perform a forward analysis on a syntax tree.
runForwardOatlabAnalysis
  :: forall (m :: * -> *) (approx :: AstNode -> *) (p :: AstNode -> *).
    (Monad m, MonadAnalysis m)
  => OatlabAnalysis
    (OatlabIAnnAst p)
    p
    approx
    'StatementNode
    'StatementNode
    m
    'Forward
  -> OatlabIAnnAst p :~> StatementFlowResult m p approx
runForwardOatlabAnalysis analysis = hcata (analyzeForward analysis)
