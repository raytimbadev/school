ppAlg :: OatlabAstF (K Doc) node -> K Doc node
ppAlg node = case node of
  Branch (K expr) (map unK -> body1) body2 ->
    K (
      "if" <+> parens expr <+> "{" $+$ nest 4 (joinLines body1) $+$ "}"
      $+$ maybe
        empty
        (\(map unK -> body') -> "else {" $+$ nest 4 (joinLines body') $+$ "}")
        body2
    )
