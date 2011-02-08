module Example4 where

import Text.ParserCombinators.Parsec hiding ((<|>), many, parse)
import Text.ParserCombinators.Parsec.Language (javaStyle)
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative
import Control.Monad

instance Applicative (GenParser tok st) where
    pure = return
    (<*>) = ap

instance Alternative (GenParser tok st) where
    empty = mzero
    (<|>) = mplus

data Exp = IntConst Integer
         | Add      Exp Exp
         | Mul      Exp Exp
           deriving Show
                    
lexer = P.makeTokenParser javaStyle
parens = P.parens lexer
number = P.decimal lexer
ident = P.identifier lexer
op = P.reservedOp lexer
whitespace = P.whiteSpace lexer

expression = additiveExp

additiveExp
    = (foldl Add <$> multiplicativeExp
                 <*  whitespace
                 <*> many (id <$ op "+" <* whitespace <*> multiplicativeExp))
      <?> "additive expression"

multiplicativeExp
    = foldl Mul <$> basicExp
                <*  whitespace
                <*> many (id <$ op "*" <* whitespace <*> basicExp)
      <?> "multiplicative expression"
      
basicExp
    = IntConst <$> number
      <|>
      parens additiveExp
      <?> "basic expression"

parse p = runParser p () "<repl>"