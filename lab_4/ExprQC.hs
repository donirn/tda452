module ExprQC where
import Expr
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.Maybe

-- E.
-- Property prop_ShowReadExpr: Shows that first showing and then reading an
-- expression (using your functions showExpr and readExpr) should produce
-- "the same" result as the expression you started with.
prop_ShowReadExpr :: Expr  -> Bool
prop_ShowReadExpr e = fromJust (readExpr(showExpr e)) `exprEqual` e

almostEqual :: Double -> Double -> Bool
almostEqual x y = abs(x-y) < 1.0e-6

exprEqual :: Expr -> Expr -> Bool
exprEqual a b = sortExpr a == sortExpr b

instance Arbitrary Expr where
  arbitrary = sized arbExpr
arbExpr :: Int -> Gen Expr
arbExpr s =
  frequency [ (1, do n <- arbitrary
                     return (Num n))
            , (s, do a <- arbExpr s'
                     b <- arbExpr s'
                     return (Add a b))
            , (s, do a <- arbExpr s'
                     b <- arbExpr s'
                     return (Mul a b))
            , (s, do a <- arbExpr s'
                     return (Cos a))
            , (s, do a <- arbExpr s'
                     return (Sin a))
            , (s, do return Var)
            ]
  where s' = s `div` 5

  
prop_SimplifyCorrect :: Expr -> Double-> Bool
prop_SimplifyCorrect e n = eval e n  `almostEqual` eval (simplify e) n