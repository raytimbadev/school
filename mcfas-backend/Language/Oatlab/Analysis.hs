{-|
Module      : Language.Oatlab.Analysis
Description : Oatlab-specific analysis code
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Oatlab.Analysis where

import Control.Monad.Except ( MonadError(..) )
import Data.Annotation ( IAnn(..), IAnnFix, topAnnI, reannotate, reannotateTree )
import Data.Functor.Compose
import Data.HFunctor ( HFix(..), hcata, HFunctor(..) )
import Language.Common.Analysis ( Analysis(..), AnalysisAnnotation(..), Direction(..) )
import Language.Oatlab.Syntax

import Debug.Trace

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
  (approx :: AstNode -> *)
  (node :: AstNode)
  (f :: AstNode -> *) :: * where
    StatementFlowAnalysis approx 'StatementNode f
      = approx 'StatementNode -> f 'StatementNode
    StatementFlowAnalysis _ node f = f node

-- | A newtype wrapper that simulates the action of the 'StatementFlowAnalysis'
-- type-level function.
newtype StatementFlowResult
  (m :: * -> *)
  (approx :: AstNode -> *)
  (ann :: AstNode -> *)
  (node :: AstNode)
  = R
    { result :: StatementFlowAnalysis
        approx
        node
        (Compose m (OatlabIAnnAst (AnalysisAnnotation approx ann)))
    }

-- | A class of monads for analyses.
-- This is just an error monad for raising an exception when the iterations are
-- exhausted for a fixpoint solution.
class MonadAnalysis m where
  -- | Raises an exception claiming that iterations are exhausted.
  iterationsExhausted :: m a
  -- TODO pass in the problematic tree, for error messages?

instance MonadError () m => MonadAnalysis m where
  iterationsExhausted = throwError ()

instance MonadAnalysis Maybe where
  iterationsExhausted = Nothing

analyzeForward
  :: forall (m :: * -> *) (ann :: AstNode -> *) (approx :: AstNode -> *) (node :: AstNode).
    (Show (approx 'StatementNode), Monad m, MonadAnalysis m)
  => Analysis
    (IAnn ann OatlabAstF (OatlabIAnnAst ann)) -- context for dataflow
    approx
    'StatementNode
    'Forward
    m
  -> IAnn
    (AnalysisAnnotation approx ann)
    OatlabAstF
    (StatementFlowResult m approx ann)
    node
  -> StatementFlowResult m approx ann node
analyzeForward _ (IAnn AnalysisAnnotation { analysisIterations = 0 } node)
  = R $ case collapseIndex node of
    ProgramDeclNodeS -> Compose iterationsExhausted
    TopLevelDeclNodeS -> Compose iterationsExhausted
    VarDeclNodeS -> Compose iterationsExhausted
    StatementNodeS -> const (Compose iterationsExhausted)
    ExpressionNodeS -> Compose iterationsExhausted
    IdentifierNodeS -> Compose iterationsExhausted

analyzeForward analysis@Analysis{..} (IAnn a@AnalysisAnnotation{analysisApproximation = ap} node)
  = case collapseIndex node of
    ProgramDeclNodeS -> R $ Compose $ case node of
      ProgramDecl (map (getCompose . result) -> topLevelDecls) -> do
        topLevelDecls' <- sequence topLevelDecls
        pure $ HFix $ IAnn a (ProgramDecl topLevelDecls')
        -- let node' = ProgramDecl (result <$> topLevelDecls)
        -- let node'' = ProgramDecl (killAnalAnn . result <$> topLevelDecls)
        -- approx <- analysisDataflow (IAnn (analysisAnnotation a) node'') ap
        -- pure $ R $ HFix $ IAnn a { analysisApproximation = approx } node'

    TopLevelDeclNodeS -> R $ Compose $ case node of
      FunctionDecl
        (getCompose . result -> ident)
        (map (getCompose . result) -> params)
        (map (fmap getCompose . result) -> fbody) -> do
          ident' <- ident
          params' <- sequence params

          let extractTopApprox = analysisApproximation . topAnnI

          initialApprox <- analysisBoundaryApproximation

          (b1, _) <- chainStmts initialApprox fbody
          let as1 = extractTopApprox <$> b1

          let fbody2 = fmap getCompose . result . runForwardOatlabAnalysis analysis <$> b1
          (b2, _) <- chainStmts initialApprox fbody2
          let as2 = extractTopApprox <$> b2

          let fixed l = all (traceShowId . uncurry analysisApproximationEq) . zip l
          if fixed (trace ("1: " ++ show as1) as1) (trace ("2: " ++ show as2) as2)
          then do
            pure $ HFix $ IAnn a $ FunctionDecl ident' params' b2
          else do
            getCompose . result $ runForwardOatlabAnalysis
              analysis
              (HFix $ IAnn a { analysisIterations = analysisIterations a - 1 } $ FunctionDecl ident' params' b2)

    StatementNodeS -> case node of
      WhileLoop
        (getCompose . result -> expr)
        (map (fmap getCompose . result) -> fbody) -> R $ \approx -> Compose $ do
          expr' <- expr
          merged <- analysisMerge approx ap
          (b, out) <- chainStmts merged fbody
          pure $ HFix $ IAnn a { analysisApproximation = out } (WhileLoop expr' b)

      ForLoop
        (getCompose . result -> var)
        (getCompose . result -> expr)
        (map (fmap getCompose . result) -> fbody) -> R $ \approx -> Compose $ do
          var' <- var
          expr' <- expr
          merged <- analysisMerge approx ap
          (b, out) <- chainStmts merged fbody
          pure $ HFix $ IAnn a { analysisApproximation = out } (ForLoop var' expr' b)

      Branch
        (getCompose . result -> expr)
        (map (fmap getCompose . result) -> thenB)
        (fmap (map (fmap getCompose . result)) -> mElseB) -> R $ \approx -> Compose $ do
          expr' <- expr
          (thenB', thenOut) <- chainStmts approx thenB
          (elseB', elseOut) <- case mElseB of
            Nothing -> pure (Nothing, approx)
            Just elseB -> do
              (b, out) <- chainStmts approx elseB
              pure (Just b, out)
          out <- analysisMerge thenOut elseOut
          pure $ HFix $ IAnn a { analysisApproximation = out } (Branch expr' thenB' elseB')

      Assignment
        (getCompose . result -> lhs)
        (getCompose . result -> rhs) -> R $ \approx -> Compose $ do
          lhs' <- lhs
          rhs' <- rhs
          let node' = Assignment lhs' rhs'
          let (HFix node'') = killAnalAnn $ HFix (IAnn a node')
          approx' <- analysisDataflow node'' approx
          pure $ HFix $ IAnn a { analysisApproximation = approx' } node'

      Expression (getCompose . result -> expr) -> R $ \approx -> Compose $ do
        expr' <- expr
        let node' = Expression expr'
        let (HFix node'') = killAnalAnn $ HFix (IAnn a node')
        approx' <- analysisDataflow node'' approx
        pure $ HFix $ IAnn a { analysisApproximation = approx' } node'

      Return (getCompose . result -> expr) -> R $ \approx -> Compose $ do
        expr' <- expr
        let node' = Return expr'
        let (HFix node'') = killAnalAnn $ HFix (IAnn a node')
        approx' <- analysisDataflow node'' approx
        pure $ HFix $ IAnn a { analysisApproximation = approx' } node'

    ExpressionNodeS -> case node of
      Var (getCompose . result -> ident)
        -> R . Compose $ HFix . IAnn a . Var <$> ident
      StringLiteral s
        -> R . Compose . pure . HFix . IAnn a $ StringLiteral s
      NumericLiteral n
        -> R . Compose . pure . HFix . IAnn a $ NumericLiteral n
      Call (getCompose . result -> callee) (sequence . map (getCompose . result) -> args)
        -> R . Compose $ do
          callee' <- callee
          args' <- args
          pure . HFix . IAnn a $ Call callee' args'

      BinaryOperation op (getCompose . result -> l) (getCompose . result -> r)
        -> R . Compose $ do
          l' <- l
          r' <- r
          pure . HFix . IAnn a $ BinaryOperation op l' r'

    IdentifierNodeS -> case node of
      Identifier s -> R $ Compose $ pure $ HFix $ IAnn a $ Identifier s

    VarDeclNodeS -> case node of
      VarDecl (getCompose . result -> ident)
        -> R . Compose $ HFix . IAnn a . VarDecl <$> ident

  where
    killAnalAnn :: HFunctor h => IAnnFix (AnalysisAnnotation foo jazz) h bar -> IAnnFix jazz h bar
    killAnalAnn = reannotateTree (const analysisAnnotation)

    chainStmts initial b = foldr phi (pure ([], initial)) (reverse b) where
      phi f m = do
        (l, approx) <- m
        tree <- f approx
        let approx' = (analysisApproximation . topAnnI) tree
        pure (tree : l, approx')

-- -- | Converts an 'OatlabAnalysis' into a higher-order F-algebra that performs
-- -- the action of that analysis locally.
-- analyzeForward
--   :: (Monad m, MonadAnalysis m)
--   => OatlabAnalysis
--     (IAnnFix p OatlabAstF)
--     p
--     approx
--     'StatementNode
--     'StatementNode
--     m
--     'Forward
--   -> IAnn p OatlabAstF (StatementFlowResult m p approx)
--   :~> StatementFlowResult m p approx
-- analyzeForward analysis@(Analysis {..}) (IAnn ann node) = case node of
--
--   ProgramDecl (map (imResult . result) -> topLevelDecls) -> FR $ IMR $ do
--     topLevelDecls' <- sequenceA topLevelDecls
--     let node' = ProgramDecl topLevelDecls'
--
--     approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
--     ann' <- analysisUpdateApproximation approx' ann
--
--     pure $ HFix (IAnn ann' node')
--
--   FunctionDecl
--     (FR (IMR name))
--     (map (imResult . result) -> params)
--     (map result -> fstmts)
--     -> FR $ IMR $ do
--       name' <- name
--       params' <- sequenceA params
--       startingApprox <- analysisBoundaryApproximation
--       (_, body') <- chainStmts analysisGetApproximation startingApprox fstmts
--       let node' = FunctionDecl name' params' body'
--
--       approx' <- analysisDataflow
--         (IAnn ann node')
--         (analysisGetApproximation ann)
--       ann' <- analysisUpdateApproximation approx' ann
--
--       pure $ HFix (IAnn ann' node')
--
--   Assignment (FR (IMR lhs)) (FR (IMR rhs)) -> FR $ \inSet -> IMR $ do
--     lhs' <- lhs
--     rhs' <- rhs
--     let node' = Assignment lhs' rhs'
--
--     outSet <- analysisDataflow (IAnn ann node') inSet
--     ann' <- analysisUpdateApproximation outSet ann
--
--     pure $ HFix (IAnn ann' node')
--
--   Return (FR (IMR expr)) -> FR $ \inSet -> IMR $ do
--     expr' <- expr
--     let node' = Return expr'
--
--     outSet <- analysisDataflow (IAnn ann node') inSet
--     ann' <- analysisUpdateApproximation outSet ann
--
--     pure $ HFix (IAnn ann' node')
--
--   Expression (FR (IMR expr)) -> FR $ \inSet -> IMR $ do
--     expr' <- expr
--     let node' = Expression expr'
--
--     outSet <- analysisDataflow (IAnn ann node') inSet
--     ann' <- analysisUpdateApproximation outSet ann
--
--     pure $ HFix (IAnn ann' node')
--
--   WhileLoop (FR (IMR expr)) (map result -> bodyFs) -> FR $ \approx -> IMR $ do
--     when (analysisGetIterations ann == 0) iterationsExhausted
--
--     expr' <- expr
--
--     let stmtApprox = analysisGetApproximation . topAnnI
--
--     (approx', body) <- chainStmts analysisGetApproximation approx bodyFs
--
--     let node' = WhileLoop expr' body
--
--     approx'' <- analysisMerge approx approx'
--
--     approx''' <- analysisDataflow (IAnn ann node') approx''
--
--     -- get the list of approximations, one for each stmt in the body
--     let approxes = stmtApprox <$> body
--
--     -- chain the statements a second time with the dataflow
--     (approx'''', body') <- chainStmts'
--       (analysisDataflow . unHFix)
--       analysisUpdateApproximation
--       approx'''
--       body
--
--     -- and get the list of approximations for the second run
--     let approxes' = stmtApprox <$> body'
--
--     approx''''' <- analysisMerge approx approx''''
--
--     let node'' = WhileLoop expr' body'
--     ann' <- analysisUpdateApproximation approx''''' ann
--
--     -- if all lists of approximations are the same ...
--     if all id (zipWith analysisApproximationEq approxes approxes')
--     then do
--       -- we found the fixed point, so we can just return this AST
--       pure $ HFix (IAnn ann' node'')
--     else do
--       -- decrease the number of iterations remaining in this node by one
--       ann'' <- analysisUpdateIterations (subtract 1) ann'
--       -- build a bona fide AST out of this node
--       let ast = HFix (IAnn ann'' node'')
--       -- and recurse
--       imResult (result (runForwardOatlabAnalysis analysis ast) approx''''')
--
--   ForLoop (FR (IMR var)) (FR (IMR expr)) (map result -> bodyFs)
--     -> FR $ \approx -> IMR $ do
--       when (analysisGetIterations ann == 0) iterationsExhausted
--
--       var' <- var
--       expr' <- expr
--
--       let stmtApprox = analysisGetApproximation . topAnnI
--
--       (approx', body) <- chainStmts analysisGetApproximation approx bodyFs
--
--       let node' = ForLoop var' expr' body
--
--       approx'' <- analysisDataflow (IAnn ann node') approx'
--
--       approx''' <- analysisMerge approx approx''
--
--       let approxes = stmtApprox <$> body
--
--       (approx'''', body') <- chainStmts'
--         (analysisDataflow . unHFix)
--         analysisUpdateApproximation
--         approx'''
--         body
--
--       let approxes' = stmtApprox <$> body'
--
--       let node'' = ForLoop var' expr' body'
--
--       approx''''' <- analysisMerge approx approx''''
--
--       ann' <- analysisUpdateApproximation approx''''' ann
--
--       if all id (zipWith analysisApproximationEq approxes approxes')
--       then pure $ HFix (IAnn ann' node'')
--       else do
--         ann'' <- analysisUpdateIterations (subtract 1) ann'
--         let ast = HFix (IAnn ann'' node'')
--         imResult (result (runForwardOatlabAnalysis analysis ast) approx''''')
--
--   Branch (FR (IMR expr)) (map result -> thenBodyFs) mElseBodyFs
--     -> FR $ \approx -> IMR $ do
--       expr' <- expr
--
--       (thenApprox, thenBody) <- chainStmts
--         analysisGetApproximation
--         approx
--         thenBodyFs
--
--       (elseApprox, elseBody) <- case mElseBodyFs of
--         Nothing -> pure (approx, Nothing)
--         Just (map result -> elseBodyFs) -> do
--           (elseApprox, stmts) <- chainStmts
--             analysisGetApproximation
--             approx
--             elseBodyFs
--           pure (elseApprox, Just stmts)
--
--       outApprox <- analysisMerge thenApprox elseApprox
--       ann' <- analysisUpdateApproximation outApprox ann
--       let outApprox' = analysisGetApproximation ann'
--       let node' = Branch expr' thenBody elseBody
--       outApprox'' <- analysisDataflow (IAnn ann' node') outApprox' -- unnecessary?
--       ann'' <- analysisUpdateApproximation outApprox'' ann'
--
--       pure $ HFix (IAnn ann'' node')
--
--   Var (FR (IMR ident)) -> FR $ IMR $ do
--     ident' <- ident
--     let node' = Var ident'
--
--     approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
--     ann' <- analysisUpdateApproximation approx' ann
--
--     pure $ HFix (IAnn ann' node')
--
--   StringLiteral s -> FR $ IMR $ do
--     let node' = StringLiteral s
--
--     approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
--     ann' <- analysisUpdateApproximation approx' ann
--
--     pure $ HFix (IAnn ann' node')
--
--   NumericLiteral n -> FR $ IMR $ do
--     let node' = NumericLiteral n
--
--     approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
--     ann' <- analysisUpdateApproximation approx' ann
--
--     pure $ HFix (IAnn ann' node')
--
--   Call (FR (IMR expr)) (map (imResult . result) -> args) -> FR $ IMR $ do
--     expr' <- expr
--     args' <- sequenceA args
--     let node' = Call expr' args'
--
--     approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
--     ann' <- analysisUpdateApproximation approx' ann
--
--     pure $ HFix (IAnn ann' node')
--
--   VarDecl (FR (IMR ident)) -> FR $ IMR $ do
--     ident' <- ident
--     let node' = VarDecl ident'
--
--     approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
--     ann' <- analysisUpdateApproximation approx' ann
--
--     pure $ HFix (IAnn ann' node')
--
--   Identifier name -> FR $ IMR $ do
--     let node' = Identifier name
--
--     approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
--     ann' <- analysisUpdateApproximation approx' ann
--
--     pure $ HFix (IAnn ann' node')
--
--   BinaryOperation op (FR (IMR opl)) (FR (IMR opr)) -> FR $ IMR $ do
--     opl' <- opl
--     opr' <- opr
--     let node' = BinaryOperation op opl' opr'
--
--     approx' <- analysisDataflow (IAnn ann node') (analysisGetApproximation ann)
--     ann' <- analysisUpdateApproximation approx' ann
--
--     pure $ HFix (IAnn ann' node')
--
-- | Perform a forward analysis on a syntax tree.
runForwardOatlabAnalysis
  :: forall (m :: * -> *) (approx :: AstNode -> *) (ann :: AstNode -> *) (node :: AstNode).
    (Show (approx 'StatementNode), Monad m, MonadAnalysis m)
  => Analysis
    (IAnn ann OatlabAstF (OatlabIAnnAst ann))
    approx
    'StatementNode
    'Forward
    m
  -> OatlabIAnnAst (AnalysisAnnotation approx ann) node
  -> StatementFlowResult m approx ann node
runForwardOatlabAnalysis analysis = hcata (analyzeForward analysis)

-- | Prepare a syntax tree for analysis, by annotating every node with an
-- iteration count and an initial approximation.
prepareAnalysisAlg
  :: forall
    (h :: (k -> *) -> k -> *)
    (f :: k -> *)
    (approx :: k -> *)
    (ann :: k -> *)
    (node :: k).
    (forall a. h f a -> approx a)
  -> Int
  -> IAnn ann h f node
  -> IAnn (AnalysisAnnotation approx ann) h f node
prepareAnalysisAlg initialize maxIterations = reannotate phi where
  phi :: forall a. h f a -> ann a -> AnalysisAnnotation approx ann a
  phi node annotation = AnalysisAnnotation
    { analysisIterations = maxIterations
    , analysisApproximation = initialize node
    , analysisAnnotation = annotation
    }
