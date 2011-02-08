module TestParserCombinators where

import Basic
import Derived
import Control.Applicative
import Control.Monad

{- In BNF:

expression ::= additiveExp

additiveExp       ::= multiplicativeExp (whitespace '+' whitespace multiplicativeExp)*
multiplicativeExp ::= basicExp (whitespace '*' whitespace basicExp)*
basicExp          ::= number
                    | '(' additiveExp ')'

-}

data Exp = IntConst Integer
         | Add      Exp Exp
         | Mul      Exp Exp
           deriving Show

expression = additiveExp

additiveExp =
    do e  <- multiplicativeExp
       es <- many (do whitespace
                      char '+'
                      whitespace
                      multiplicativeExp)
       return (foldl Add e es)

multiplicativeExp =
    do e <- basicExp
       es <- many (do whitespace
                      char '*'
                      whitespace
                      basicExp)
       return (foldl Mul e es)

basicExp =
    do n <- number
       return (IntConst n)
    <|>
    parens additiveExp