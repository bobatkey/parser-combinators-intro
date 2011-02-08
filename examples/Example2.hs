module Example2 where

import Basic
import Derived
import Control.Applicative
import Control.Monad

data Exp = IntConst Integer
         | Add      Exp Exp
         | Mul      Exp Exp
           deriving Show

expression = additiveExp

additiveExp
    = foldl Add <$> multiplicativeExp
                <*> many (whitespace *> char '+' *> whitespace *> multiplicativeExp)

multiplicativeExp
    = foldl Mul <$> basicExp
                <*> many (whitespace *> char '*' *> whitespace *> basicExp)

basicExp
    = IntConst <$> number
      <|>
      parens additiveExp
