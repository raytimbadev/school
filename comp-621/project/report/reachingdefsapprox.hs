type family ReachingDefinitionsApprox' (node :: AstNode) :: * where
  ReachingDefinitionsApprox' 'StatementNode
    = M.Map String (S.Set StatementNumber)
  ReachingDefinitionsApprox' _ = ()

newtype ReachingDefinitionsApprox (node :: AstNode)
  = ReachingDefinitionsApprox (ReachingDefinitionsApprox' node)
