module Main where

import qualified Data.Set as S
import Expr (Expr (..))
import Scope (Scope (Scope, free, shadow))
import SolRecur (scopeRecur)
import SolWriter (scopeWriter)

test :: (Expr -> Scope) -> [Bool]
test scope =
  [ -- Free Variable
    (==)
      (scope $ Var "x")
      (Scope {free = S.singleton "x", shadow = S.empty}),
    -- Binding --
    (==)
      (scope $ Let "x" (Lit 123) (Var "x"))
      (Scope {free = S.empty, shadow = S.empty}),
    -- Shadowing (nested lets) --
    (==)
      ( scope $
          Let
            "x"
            (Lit 123)
            (Let "x" (Lit 456) (Lit 789))
      )
      (Scope {free = S.empty, shadow = S.singleton "x"}),
    -- No Shadowing (side-by-side lets) --
    (==)
      ( scope $
          Add
            (Let "x" (Lit 123) (Var "x"))
            (Let "x" (Lit 123) (Var "x"))
      )
      (Scope {free = S.empty, shadow = S.empty})
  ]

main :: IO ()
main = do
  putStrLn $ "Recur: " ++ show (test scopeRecur)
  putStrLn $ "Writer: " ++ show (test scopeWriter)