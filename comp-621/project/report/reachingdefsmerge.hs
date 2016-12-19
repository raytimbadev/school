analysisMerge
  :: ReflectS node
  => ReachingDefinitionsApprox node -> ReachingDefinitionsApprox node
  -> m (ReachingDefinitionsApprox node)
analysisMerge
  (ReachingDefinitionsApprox a1)
  (ReachingDefinitionsApprox a2)
  = pure $ ReachingDefinitionsApprox $ case reflectS (Proxy :: Proxy node) of
    StatementNodeS -> M.unionWith S.union a1 a2
    ProgramDeclNodeS -> ()
    TopLevelDeclNodeS -> ()
    VarDeclNodeS -> ()
    ExpressionNodeS -> ()
    IdentifierNodeS -> ()
