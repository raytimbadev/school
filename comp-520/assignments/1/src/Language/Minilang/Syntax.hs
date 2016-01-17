{-# LANGUAGE DeriveFunctor #-}

module Language.Minilang.Syntax where

import Prelude hiding ( print, read )
import Control.Monad.Free
import Data.Text ( Text )

data ProgramF f
    = Var Ident Type f
    | Assign Ident Expr f
    | While Expr (Program ()) f
    | If Expr (Program ()) (Program ()) f
    | Print Expr f
    | Read (ValueE -> f)
    deriving (Functor)

type Expr = TermE

data TermE
    = Plus TermE FactorE
    | Minus TermE FactorE
    | Factor FactorE

data FactorE
    = Times FactorE ValueE
    | Divide FactorE ValueE
    | Value ValueE

data ValueE
    = Negate LiteralE
    | Literal LiteralE

data LiteralE
    = Variable Text
    | Int Int
    | Real Double
    | String Text

type Program = Free ProgramF

type Ident = Text
type Type = Text

var :: Ident -> Type -> Program ()
var i t = liftF . Var i t $ ()

(.=) :: Ident -> Expr -> Program ()
i .= e = liftF . Assign i e $ ()

while :: Expr -> Program () -> Program ()
while e p = liftF . While e p $ ()

ifThenElse :: Expr -> Program () -> Program () -> Program ()
ifThenElse e tb eb = liftF . If e tb eb $ ()

print :: Expr -> Program ()
print e = liftF . Print e $ ()

read :: Program ValueE
read = liftF . Read $ id
