-- TODO
-- DONI: H, I, J
-- JP:  E, F, G
-- TBD: K


module Expr where
import Data.Char
import Data.List
import Parsing
import System.Random
import Data.Maybe
import Data.List

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          | Var
          | Num Double
          deriving (Eq,Ord)


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
ex7' = "sin (-7.321271617241244+sin 1.3841251597435147 +cos 21.38739735015702)"

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
-- Function readExpr: Given a string, tries to interpret the string as an
-- expression, and returns Just of that expression if it succeeds. Otherwise,
-- Nothing will be returned.
readExpr :: String -> Maybe Expr
readExpr s = case parse expr (stripWhitespace s) of
  Just (e,_) -> Just e
  _ -> Nothing

stripWhitespace :: String -> String
stripWhitespace = filter (' ' /=)

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

-----------------------------------------------------------------------------
-- F.
-- Function simplify: simplifies expressions so that subexpressions not
-- involving variables are always simplified to their smallest
-- representation.
simplify :: Expr -> Expr
simplify (Add a b)  =   add (simplify a) (simplify b)
simplify (Mul a b)  =   mul (simplify a) (simplify b)
simplify (Sin e)    =   Sin (simplify e)
simplify (Cos e)    =   Cos (simplify e)
simplify e          =   e

mul :: Expr -> Expr -> Expr
mul (Num 0) b         = Num 0
mul a       (Num 0)   = Num 0
mul (Num 1) b         = b
mul a       (Num 1)   = a
mul (Num x) (Num y)   = Num (x*y)
mul a       b         = Mul a b

-- TODO: should take account for Var
-- e.g. "x+x" should return "2*x"
-- maybe also "4*x+2*x" should return "6*x"
-- but this is optional

add :: Expr -> Expr -> Expr
add (Num 0) a         = a
add a       (Num 0)   = a
add (Num x) (Num y)   = Num (x+y)
add a       b         = Add a b

sortExpr :: Expr -> Expr
sortExpr (Add a b) = sortAdd a b
sortExpr (Mul a b) = sortMul a b
sortExpr (Sin a)   = Sin (sortExpr a)
sortExpr (Cos a)   = Cos (sortExpr a)
sortExpr e         = e

sortAdd :: Expr -> Expr -> Expr
sortAdd a b = foldl Add x xs
  where l = listAdd a b
        (x:xs) = sort l

listAdd :: Expr -> Expr -> [Expr]
listAdd (Add a b) (Add c d) = listAdd (sortExpr a) (sortExpr b) ++ listAdd (sortExpr c) (sortExpr d)
listAdd a         (Add b c) = (sortExpr a) : listAdd (sortExpr b) (sortExpr c)
listAdd (Add a b) c         = (sortExpr c) : listAdd (sortExpr a) (sortExpr b)
listAdd a         b         = [(sortExpr a),(sortExpr b)]

sortMul :: Expr -> Expr -> Expr
sortMul a b = foldl Mul x xs
  where l = listMul a b
        (x:xs) = sort l

listMul :: Expr -> Expr -> [Expr]
listMul (Mul a b) (Mul c d) = listMul (sortExpr a) (sortExpr b) ++ listMul (sortExpr c) (sortExpr d)
listMul a         (Mul b c) = (sortExpr a) : listMul (sortExpr b) (sortExpr c)
listMul (Mul a b) c         = (sortExpr c) : listMul (sortExpr a) (sortExpr b)
listMul a         b         = [(sortExpr a),(sortExpr b)]

-----------------------------------------------------------------------------
-- G.
-- Function differentiate

-- FIXME: use "simplify" function instead of "add" and "mul"
differentiate :: Expr -> Expr
differentiate (Num n)   = Num 0
differentiate Var       = Num 1
differentiate (Add a b) = add (differentiate a) (differentiate b)
differentiate (Mul a b) = add (mul a (differentiate b))
                                (mul b (differentiate a))
differentiate (Sin e)     = mul (differentiate e) (Cos e)
differentiate (Cos e)     = mul (Num (-1)) (mul (differentiate e) (Sin e))

-- FIXME: failed, the result should be "(-1) 12*x*x*x*sin (3*x*x*x*x)"
exG1 = fromJust (readExpr "cos(3*x*x*x*x)")
