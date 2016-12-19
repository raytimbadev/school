newtype StatementNumber = StatementNumber { unStatementNumber :: Int }
  deriving (Eq, Ord, Show)

type family StatementNumbering' (node :: AstNode) :: * where
  StatementNumbering' 'StatementNode = StatementNumber
  StatementNumbering' _ = ()

newtype StatementNumbering (node :: AstNode)
  = StatementNumbering { unStatementNumbering :: StatementNumbering' node }
