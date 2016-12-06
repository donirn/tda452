module Expr where
import Data.Char(isDigit)
import Parsing hiding (chain,digit)

data Expr = Num Double
          | Var
          | Add Expr Expr
          | Mul Expr Expr
          | Cos Expr
          | Sin Expr

instance Show Expr where
  show = showExpr
-- 2*sin x + 0.5*cos(10*x)
ex1 = Add (Mul (Num 2) (Sin Var)) (Mul (Num 0.5) (Cos (Mul (Num 10) Var)))

---------------------------------------------------------------------------

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr Var       = "x"
showExpr (Cos a)   = "cos " ++ showFactor' a ++ " "
showExpr (Sin a)   = "sin " ++ showFactor' a ++ " "

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e         = showExpr e

showFactor' :: Expr -> String
showFactor' (Num n) = showExpr (Num n)
showFactor' Var     = showExpr Var
showFactor' e       = "(" ++ showExpr e ++ ")"
---------------------------------------------------------------------------
-- given an expression, and the value for the variable x
-- , calculates the value of the expression.
eval :: Expr -> Double -> Double
eval (Num n) _   = n
eval Var x       = x
eval (Add a b) x = (eval a x) + (eval b x)
eval (Mul a b) x = (eval a x) * (eval b x)
eval (Cos a) x   = cos (eval a x)
eval (Sin a) x   = sin (eval a x)
---------------------------------------------------------------------------
-- Given a string, tries to interpret the string as an expression, and returns Just of that expression if it succeeds. Otherwise, Nothing will be returned.
--readExpr :: String -> Maybe Expr

-- | Parse a digit (also available in the Parsing module)
digit :: Parser Char
digit = sat isDigit

-- | Parse a number
number :: Parser Double
number = do --s <- oneOrMore digit
            --char '.'
            --t <- zeroOrMore digit
            return (read s)

--expr, term, factor :: Parser Expr
--
--expr = leftAssoc Add term (char '+')
--
--term = leftAssoc Mul factor (char '*')
--
--factor = (Num <$> number) <|> (char '(' *> expr <* char ')')
