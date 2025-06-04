module Scope where

import qualified Data.Set as S
import Expr (Var)

type VarSet = S.Set Var

data Scope = Scope {free :: VarSet, shadow :: VarSet} deriving (Eq, Show)

instance Semigroup Scope where
  (Scope {free = f1, shadow = s1}) <> (Scope {free = f2, shadow = s2}) =
    Scope {free = S.union f1 f2, shadow = S.union s1 s2}

instance Monoid Scope where
  mempty = Scope {free = S.empty, shadow = S.empty}
