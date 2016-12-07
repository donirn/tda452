-- | Parsing
-- Examples to illustrate how to write parsers using parsing combinators
-- Functional Programming course 2016.
-- Thomas Hallgren

{-
This started out as a skeleton, the definitions were filled in
during the lecture.
-}
module ParsingExamples where
import Data.Char(isDigit)
import Parsing hiding (chain,digit)


--------------------------------------------------------------------------------
-- * Our first parser
-- Adding two numbers, e.g. parsing "10+5" should return 15

{- EBNF:
digit      ::= "0".."9".
number     ::= digit {digit}.
addNumbers ::= number "+" number.
-}

-- | Parse a digit (also available in the Parsing module)
digit :: Parser Char
digit = sat isDigit

-- | Parse a number
number :: Parser Integer
number = do s <- oneOrMore digit
            return (read s)

-- | Parse two numbers, separated by +, and add them
addNumbers :: Parser Integer
addNumbers = do a <- number
                char '+'
                b <- number
                return (a+b)

--------------------------------------------------------------------------------
-- * Writing the same parsing functions directly

number' :: String -> Maybe (Integer,String)
number' s = case span isDigit s of
              ("",_) -> Nothing
              (s1,s2) -> Just (read s1,s2)

addNumbers' :: String -> Maybe (Integer,String)
addNumbers' s =
  case number' s of
    Just (a,'+':r) ->
      case number' r of
        Just (b,r) -> Just (a+b,r)
        _ -> Nothing
    _ -> Nothing

--------------------------------------------------------------------------------
-- * An expression parser (version 1)
data Expr = Num Double
            | Var
            | Add Expr Expr
            | Mul Expr Expr
            | Cos Expr
            | Sin Expr
            deriving (Eq,Show)
{- BNF:
expr   ::= term "+" expr | term.
term   ::= factor "*" term | factor.
factor ::= number | "(" expr ")".
-}
{-
expr, term, factor :: Parser Expr

expr = expr' <|> term
  where
    expr' = do t <- term
               char '+'
               e <- expr
               return (Add t e)

term = term' <|> factor
  where
    term' = do f <- factor
               char '*'
               t <- term
               return (Mul f t)

factor = (do n <- number;
             return (Num n))
         <|>
         (do char '('
             e <- expr
             char ')'
             return e)

--}

--------------------------------------------------------------------------------
-- * A more elegant expression parser
{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= number | "(" expr ")".
-}
--{-
expr, term, factor :: Parser Expr

expr = leftAssoc Add term (char '+')

term = leftAssoc Mul factor (char '*')

factor = (Num <$> number) <|> (char '(' *> expr <* char ')')

--}

-- | Parse a list of items with separators
-- (also available in the Parsing module)
chain :: Parser item -> Parser sep -> Parser [item]
chain item sep = do i <- item
                    is <- zeroOrMore (sep *> item)
                    return (i:is)

leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do i:is <- chain item sep
                           return (foldl op i is)


rightAssoc op item sep = undefined




--------------------------------------------------------------------------------
-- * More examples
{-
-- ** Data types with infix operatos
infixl 6 :+
infixl 7 :*

data Expr2 = C Integer
           | Expr2 :+ Expr2
           | Expr2 :* Expr2
           deriving (Show,Read)

ex1 = C 2
ex2 = ex1 :+ ex1
ex3 = C 1 :+ C 2 :* C 3
ex4 = (C 1 :+ C 2) :* C 3


-- | Like addNumbers', but taking advantage of the fact that Maybe is a monad
addNumbers'' s =
  do (a,r) <- number' s
     '+':r <- Just r
     (b,r) <- number' r
     return (a+b,r)
-}
