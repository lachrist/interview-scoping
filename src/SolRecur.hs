module SolRecur where

import qualified Data.Map as M
import qualified Data.Set as S
import Expr (Expr (..))
import Scope (Scope (Scope))
import ScopeRecur (ScopeRecur (ScopeRecur, bound, free), bindVar)

scope :: Expr -> ScopeRecur
scope (Lit _) = mempty
scope (Var v) = ScopeRecur {free = S.singleton v, bound = M.empty}
scope (Add e1 e2) = scope e1 <> scope e2
scope (Mult e1 e2) = scope e1 <> scope e2
scope (Let v e1 e2) = scope e1 <> bindVar v (scope e2)

toScope :: ScopeRecur -> Scope
toScope (ScopeRecur {free = f, bound = b}) =
  Scope f (M.keysSet (M.filter (> 1) b))

scopeRecur :: Expr -> Scope
scopeRecur = toScope . scope
