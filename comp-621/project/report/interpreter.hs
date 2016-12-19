{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

data AstNode = ExprNode | StmtNode

data ProgramF :: (AstNode -> *) -> AstNode -> * where
  Print :: p 'ExprNode -> ProgramF p 'StmtNode
  Assign :: String -> p 'ExprNode -> ProgramF p 'StmtNode
  While :: p 'ExprNode -> [p 'StmtNode] -> ProgramF p 'StmtNode
  Add :: p 'ExprNode -> p 'ExprNode -> ProgramF p 'ExprNode
  Mul :: p 'ExprNode -> p 'ExprNode -> ProgramF p 'ExprNode
  Const :: Int -> ProgramF p 'ExprNode
  Var :: String -> ProgramF p 'ExprNode

class HFunctor h where
  hfmap :: (forall a. f a -> g a) -> h f a -> h g a

instance HFunctor ProgramF where
  hfmap phi e = case e of
    Print expr -> Print (phi expr)
    Assign s expr -> Assign s (phi expr)
    While expr stmts -> While (phi expr) (fmap phi stmts)
    Add l r -> Add (phi l) (phi r)
    Mul l r -> Mul (phi l) (phi r)
    Const n -> Const n
    Var s -> Var s

newtype HFix h a = HFix (h (HFix h) a)

hcata :: HFunctor h => (forall a. h f a -> f a) -> HFix h a -> f a
hcata halg (HFix h) = halg (hfmap (hcata halg) h)

type Env = [(String, Int)]

type family Interpreter' (node :: AstNode) :: * where
  Interpreter' ExprNode = Env -> Either String Int
  Interpreter' StmtNode = Env -> IO (Maybe Env)

newtype Interpreter (node :: AstNode) = Interpreter (Interpreter' node)
unwrap :: Interpreter node -> Interpreter' node
unwrap (Interpreter e) = e

interpretAlg :: ProgramF Interpreter node -> Interpreter node
interpretAlg p = case p of
  Print (Interpreter fx) -> Interpreter $ \env ->
    case fx env of
      Left err -> do
        putStrLn $ "runtime error: " ++ err
        pure Nothing
      Right x -> do
        print x
        pure (Just env)
  Assign s (Interpreter fx) -> Interpreter $ \env ->
    case fx env of
      Left err -> do
        putStrLn $ "runtime error: " ++ err
        pure Nothing
      Right x -> pure (Just ( (s, x) : env ))
  While (Interpreter e) is ->
    let fstmts = map unwrap is
    in Interpreter $ \env -> chainStatements fstmts env
  Add (Interpreter fx) (Interpreter fy) -> Interpreter $ \env ->
    case (fx env, fy env) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right x, Right y) -> Right (x + y)
  Mul (Interpreter fx) (Interpreter fy) -> Interpreter $ \env ->
    case (fx env, fy env) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right x, Right y) -> Right (x * y)
  Const n -> Interpreter $ \_ -> Right n
  Var s -> Interpreter $ \env ->
    case lookup s env of
      Just x -> Right x
      Nothing -> Left $ "unbound variable `" ++ s ++ "' referenced"

chainStatements :: [Env -> IO (Maybe Env)] -> Env -> IO (Maybe Env)
chainStatements [] env = pure (Just env)
chainStatements (fstmt:fstmts) env = do
  result <- fstmt env
  case result of
    Nothing -> pure Nothing -- stop execution, due to runtime error
    Just env' -> chainStatements fstmts env'

interpretStmt = hcata interpretAlg

type Line = HFix ProgramF 'StmtNode
type Program = [Line]

interpretProgram :: Program -> Env -> IO (Maybe Env)
interpretProgram stmts = chainStatements interpretedStmts where
  interpretedStmts = map (unwrap . interpretStmt) stmts
