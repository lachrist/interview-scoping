module ScopeRecur where

import qualified Data.Map as M
import qualified Data.Set as S
import Expr (Var)
import Scope (VarSet)

type Count = Int

type MapVarCount = M.Map Var Count

data ScopeRecur = ScopeRecur {free :: VarSet, bound :: MapVarCount} deriving (Show)

instance Semigroup ScopeRecur where
  (ScopeRecur {free = f1, bound = b1}) <> (ScopeRecur {free = f2, bound = b2}) =
    ScopeRecur {free = S.union f1 f2, bound = M.unionWith max b1 b2}

instance Monoid ScopeRecur where
  mempty = ScopeRecur {free = S.empty, bound = M.empty}

bindVar :: Var -> ScopeRecur -> ScopeRecur
bindVar v (ScopeRecur {free = f, bound = b}) =
  ScopeRecur {free = S.delete v f, bound = M.insertWith (+) v 1 b}
