numberStatementAlg
  :: (Monad m, MonadState Int m)
  => IAnn p OatlabAstF f a -> m (IAnn StatementNumbering OatlabAstF f a)
numberStatementAlg = reannotateM phi where
  phi
    :: (Monad m, MonadState Int m)
    => OatlabAstF f a -> p a -> m (StatementNumbering a)
  phi node _ = case collapseIndex node of
    StatementNodeS -> StatementNumbering . StatementNumber <$> nextInt
    ProgramDeclNodeS -> pure $ StatementNumbering ()
    TopLevelDeclNodeS -> pure $ StatementNumbering ()
    VarDeclNodeS -> pure $ StatementNumbering ()
    ExpressionNodeS -> pure $ StatementNumbering ()
    IdentifierNodeS -> pure $ StatementNumbering ()
