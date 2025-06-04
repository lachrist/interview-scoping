module Expr where

type Var = String

data Expr
  = Lit Int
  | Mult Expr Expr
  | Add Expr Expr
  | Let Var Expr Expr
  | Var Var
  deriving (Show)
