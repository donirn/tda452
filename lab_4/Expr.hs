module Expr where
import Parsing
import Data.Char

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
ex1' = "2.0*sin x +0.5*cos (10.0*x)"

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
readExpr :: String -> Maybe Expr
readExpr s = case parse expr (stripWhitespace s) of
  Just (e,_) -> Just e
  _ -> Nothing

stripWhitespace :: String -> String
stripWhitespace [] = []
stripWhitespace (' ':xs) = xs
stripWhitespace (x:xs)   = x : stripWhitespace xs

-- TODO parser for sin and cos
expr, term, factor :: Parser Expr

expr = leftAssoc Add term (char '+')

term = leftAssoc Mul factor (char '*')

factor = (sinP *> (Sin <$> (var <|> (Num <$> readsP) ))) <|> var <|> (Num <$> readsP) <|> (char '(' *> expr <* char ')')

-- | Parse a list of items with separators
-- (also available in the Parsing module)
leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do i:is <- chain item sep
                           return (foldl op i is)

var :: Parser Expr
var = do c <- char 'x'
         return Var

sinP :: Parser String
sinP = do s <- char 's'
          i <- char 'i'
          n <- char 'n'
          return ""
