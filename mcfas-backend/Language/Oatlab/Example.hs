{-|
Module      : Language.Oatlab.Example
Description : Example code using McFAS
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Oatlab.Example where

import Data.Annotation
import Data.HFunctor
import Language.Oatlab.Syntax

type family MyAnn (node :: AstNode) :: * where
   MyAnn 'ProgramDeclNode = ()
   MyAnn 'TopLevelDeclNode = ()
   MyAnn 'VarDeclNode = ()
   MyAnn 'StatementNode = String
   MyAnn 'ExpressionNode = ()
   MyAnn 'IdentifierNode = ()

newtype MyAnnW (node :: AstNode) = MyAnnW { unMyAnnW :: MyAnn node }

example :: OatlabHAnnAst Int 'ProgramDeclNode
example
  = HFix (
    HAnn 1 (
      ProgramDecl [
        HFix (
          HAnn 2 (
            FunctionDecl
              ( HFix (HAnn 3 (Identifier "foo")) )
              [ HFix (HAnn 4 (Identifier "x")) ]
              [
                HFix (
                  HAnn 5 (
                    Return (
                      HFix (
                        HAnn 6 (
                          Var (
                            HFix (
                              HAnn 7 (
                                Identifier "x"
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ]
          )
        )
      ]
    )
  )

type family Result (node :: AstNode) :: * where
  Result 'ProgramDeclNode = ()
  Result 'TopLevelDeclNode = ()
  Result 'VarDeclNode = ()
  Result 'StatementNode = String
  Result 'ExpressionNode = ()
  Result 'IdentifierNode = ()

-- | The result of a fold can be indexed in much the same way as an annotation
-- can.
newtype R (node :: AstNode) = R { unR :: Result node }

rAlg :: OatlabAstF R :~> R
rAlg = \case
  ProgramDecl (map unR -> _) -> R ()
  FunctionDecl _ _ (map unR -> _) -> R ()
  _ -> error "rAlg"
