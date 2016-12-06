data Expr = Num Float
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Cos Expr
          | Sin Expr

instance Show Expr where
  show = showExpr

type Name = String

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e = showExpr e
