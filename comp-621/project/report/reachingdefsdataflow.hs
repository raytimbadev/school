reachingDefinitionsDataflow
  :: forall (m :: * -> *) (a :: AstNode). Monad m
  => IAnn StatementNumbering OatlabAstF (OatlabIAnnAst StatementNumbering) a
  -- ^ Node in the syntax tree
  -> ReachingDefinitionsApprox a -- ^ input flow set
  -> m (ReachingDefinitionsApprox a) -- ^ output flow set
reachingDefinitionsDataflow
  (IAnn (StatementNumbering number) node)
  (ReachingDefinitionsApprox approx)
  = pure $ ReachingDefinitionsApprox $ case collapseIndex node of
    StatementNodeS -> case node of
      ForLoop var _ _
        -> addDefinition
            (nameFromVarDecl (stripI var))
            number
            approx
      Assignment lhs _
        | unK (hcata (isLValueAlg . bareI) lhs)
          -> M.insert
              (renderOatlab (unK $ hcata (ppAlg . bareI) lhs))
              (S.singleton number)
              approx
      _ -> approx
    _ -> approx
