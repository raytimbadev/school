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

import Data.Annotation ( IAnn(..) )
import Data.HFunctor ( HFix(..), hcata, (:~>) )
import Language.Oatlab.Syntax

-- we can represent an analysis with a record that specifies the six steps
--
-- 1. approximation
-- 2. precise statement
-- 3. direction
-- 4. merge operator
-- 5. data flow equation
-- 6. initial conditions

data Direction = Forwards | Backwards

data Analysis
  (h :: (k -> *) -> k -> *)
  (f :: k -> *)
  (p :: k -> *)
  (approx :: *)
  (m :: * -> *)
  (dir :: Direction)
  = Analysis
    { analysisMerge :: approx -> approx -> m approx
    -- ^ How are approximations merged?
    , analysisDataflow :: forall a. h f a -> approx -> m approx
    -- ^ How do we compute a new approximation for a node in the syntax tree,
    -- given some existing approximation?
    , analysisUpdateAnnotation :: forall a. approx -> p a -> m (p a)
    -- ^ How do we store an approximation inside an annotation?
    , analysisGetApproximation :: forall a. p a -> approx
    -- ^ How do we retrieve an approximation from an annotation?
    , analysisBoundaryApproximation :: m approx
    -- ^ What is the initial approximation at a boundary? (e.g. the start of
    -- the function)
    , analysisInitialApproximation :: m approx
    }

type OatlabAnalysis = Analysis OatlabAstF

type family ForwardAnalysis (node :: AstNode) (approx :: *) (f :: AstNode -> *) :: * where
  ForwardAnalysis 'ProgramDeclNode _ f = f 'ProgramDeclNode
  ForwardAnalysis 'TopLevelDeclNode _ f = f 'TopLevelDeclNode
  ForwardAnalysis 'VarDeclNode _ f = f 'VarDeclNode
  ForwardAnalysis 'StatementNode approx f = approx -> f 'StatementNode
  ForwardAnalysis 'ExpressionNode _ f = f 'ExpressionNode
  ForwardAnalysis 'IdentifierNode _ f = f 'IdentifierNode

newtype IndexedMonadicResult
  (m :: * -> *)
  (p :: AstNode -> *)
  (node :: AstNode)
  = IMR { imResult :: m (OatlabIAnnAst p node) }

newtype ForwardResult
  (m :: * -> *)
  (p :: AstNode -> *)
  (approx :: *)
  (node :: AstNode)
  = FR { result :: ForwardAnalysis node approx (IndexedMonadicResult m p) }

analyzeForward
  :: Monad m => OatlabAnalysis (OatlabIAnnAst p) p approx m dir
  -> IAnn p OatlabAstF (ForwardResult m p approx) :~> ForwardResult m p approx
analyzeForward analysis@(Analysis {..}) (IAnn ann node) = case node of

  ProgramDecl (map (imResult . result) -> topLevelDecls) -> FR $ IMR $ do
    topLevelDecls' <- sequenceA topLevelDecls
    let node' = ProgramDecl topLevelDecls'

    approx' <- analysisDataflow node' (analysisGetApproximation ann)
    ann' <- analysisUpdateAnnotation approx' ann

    pure $ HFix (IAnn ann' node')

  FunctionDecl (FR (IMR name)) (map (imResult . result) -> params) (map result -> fstmts)
    -> FR $ IMR $ do
      name' <- name
      params' <- sequenceA params
      startingApprox <- analysisBoundaryApproximation
      (body', _) <- foldr -- TODO need to think about sequencing order
        (\f m -> do
          (stmts, approx) <- m
          stmt@(HFix (IAnn stmtAnn _)) <- imResult (f approx)
          let stmtApprox = analysisGetApproximation stmtAnn
          pure (stmt:stmts, stmtApprox)
        )
        (pure ([], startingApprox))
        fstmts
      let node' = FunctionDecl name' params' body'

      approx' <- analysisDataflow node' (analysisGetApproximation ann)
      ann' <- analysisUpdateAnnotation approx' ann

      pure $ HFix (IAnn ann' node')

  Assignment (FR (IMR lhs)) (FR (IMR rhs)) -> FR $ \inSet -> IMR $ do
    lhs' <- lhs
    rhs' <- rhs
    let node' = Assignment lhs' rhs'

    outSet <- analysisDataflow node' inSet
    ann' <- analysisUpdateAnnotation outSet ann

    pure $ HFix (IAnn ann' node')

  Return (FR (IMR expr)) -> FR $ \inSet -> IMR $ do
    expr' <- expr
    let node' = Return expr'

    outSet <- analysisDataflow node' inSet
    ann' <- analysisUpdateAnnotation outSet ann

    pure $ HFix (IAnn ann' node')

  Expression (FR (IMR expr)) -> FR $ \inSet -> IMR $ do
    expr' <- expr
    let node' = Expression expr'

    outSet <- analysisDataflow node' inSet
    ann' <- analysisUpdateAnnotation outSet ann

    pure $ HFix (IAnn ann' node')

  WhileLoop _ _ -> error "WhileLoop"
  ForLoop _ _ _ -> error "ForLoop"
  Branch _ _ _ -> error "Branch"

  Var (FR (IMR ident)) -> FR $ IMR $ do
    ident' <- ident
    let node' = Var ident'

    approx' <- analysisDataflow node' (analysisGetApproximation ann)
    ann' <- analysisUpdateAnnotation approx' ann

    pure $ HFix (IAnn ann' node')

  StringLiteral s -> FR $ IMR $ do
    let node' = StringLiteral s

    approx' <- analysisDataflow node' (analysisGetApproximation ann)
    ann' <- analysisUpdateAnnotation approx' ann

    pure $ HFix (IAnn ann' node')

  NumericLiteral n -> FR $ IMR $ do
    let node' = NumericLiteral n

    approx' <- analysisDataflow node' (analysisGetApproximation ann)
    ann' <- analysisUpdateAnnotation approx' ann

    pure $ HFix (IAnn ann' node')

  Call (FR (IMR expr)) (map (imResult . result) -> args) -> FR $ IMR $ do
    expr' <- expr
    args' <- sequenceA args
    let node' = Call expr' args'

    approx' <- analysisDataflow node' (analysisGetApproximation ann)
    ann' <- analysisUpdateAnnotation approx' ann

    pure $ HFix (IAnn ann' node')

  Identifier name -> FR $ IMR $ do
    let node' = Identifier name

    approx' <- analysisDataflow node' (analysisGetApproximation ann)
    ann' <- analysisUpdateAnnotation approx' ann

    pure $ HFix (IAnn ann' node')

  BinaryOperation op (FR (IMR opl)) (FR (IMR opr)) -> FR $ IMR $ do
    opl' <- opl
    opr' <- opr
    let node' = BinaryOperation op opl' opr'

    approx' <- analysisDataflow node' (analysisGetApproximation ann)
    ann' <- analysisUpdateAnnotation approx' ann

    pure $ HFix (IAnn ann' node')

-- runOatlabAnalysis
--   :: forall
--     (a :: AstNode)
--     (m :: * -> *)
--     (p :: AstNode -> *)
--     (f :: AstNode -> *)
--     (approx :: *)
--     (dir :: Direction).
--     Monad m
--   => OatlabAnalysis f a p approx m dir
--   -> HFix (IAnn p OatlabAstF) a -- -> OatlabIAnnAst p a
--   -> m (OatlabIAnnAst p a)
-- runOatlabAnalysis analysis@(Analysis {..}) = hcata _ where
