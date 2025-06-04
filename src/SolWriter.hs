module SolWriter where

import Control.Monad (unless, when)
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import qualified Data.Set as S
import Expr (Expr (..))
import Scope (Scope (..), VarSet)

scope :: Expr -> VarSet -> Writer Scope ()
scope (Lit _) _ = return ()
scope (Var v) vs =
  unless
    (S.member v vs)
    (tell $ Scope {free = S.singleton v, shadow = S.empty})
scope (Add e1 e2) vs = scope e1 vs >> scope e2 vs
scope (Mult e1 e2) vs = scope e1 vs >> scope e2 vs
scope (Let v e1 e2) vs =
  when
    (S.member v vs)
    (tell $ Scope {free = S.empty, shadow = S.singleton v})
    >> scope e1 vs
    >> scope e2 (S.insert v vs)

scopeWriter :: Expr -> Scope
scopeWriter e = snd $ runWriter $ scope e S.empty
