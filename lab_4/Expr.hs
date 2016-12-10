-- TODO
-- DONI: H, I, J
-- JP:  E, F, G
-- TBD: K


module Expr where
import Parsing
import Data.Char
import Data.List

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
ex2' = "2*3+(4*5)"
ex3' = "sin(x)"
ex4' = "sin (cos x)"
ex5' = "(sin x) + (cos x)"
ex6' = "sin cos x"

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
showFactor' (Sin a) = showExpr (Sin a)
showFactor' (Cos a) = showExpr (Cos a)
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
stripWhitespace = filter (' ' /=)

-- TODO parser for sin and cos
expr, term, factor :: Parser Expr

expr = leftAssoc Add term (char '+')

term = leftAssoc Mul factor (char '*')

factor = sinP <|> cosP <|> var <|> double <|> parentheses

-- | Parse a list of items with separators
-- (also available in the Parsing module)
leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do i:is <- chain item sep
                           return (foldl op i is)

double :: Parser Expr
double = Num <$> readsP

var :: Parser Expr
var = char 'x' *> return Var

parentheses :: Parser Expr
parentheses = char '(' *> expr <* char ')'

-- Parse an expression that has prefix, e.g. sin, cos
prefixParser :: (Expr -> Expr) -> String -> Parser Expr
prefixParser f (x:xs) = foldl (\b a -> b *> char a) (char x) xs *> (f <$> factor)

sinP :: Parser Expr
sinP = prefixParser Sin "sin"

cosP :: Parser Expr
cosP = prefixParser Cos "cos"
