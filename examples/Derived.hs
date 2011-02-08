module Derived where

import Basic
import Control.Applicative
import Control.Monad
import Data.Char (isDigit, ord, isAlpha)

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do c <- readChar
                 guard (p c)
                 return c

char :: Char -> Parser ()
char c = do satisfies (==c)
            return ()
            
identifier :: Parser String
identifier = some (satisfies isAlpha)

number :: Parser Integer
number = do d <- digit; numberInner d
    where
      numberInner n = do d <- digit
                         numberInner (n*10 + d)
                      <|>
                      return n

digit :: Parser Integer
digit = do c <- satisfies isDigit
           return (fromIntegral $ ord c - ord '0')

whitespace :: Parser ()
whitespace = const () <$> many (char ' ')

parens :: Parser a -> Parser a
parens p = do whitespace
              char '('
              whitespace
              a <- p
              whitespace
              char ')'
              whitespace
              return a

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy elem sep = (do e <- elem; es <- elems; return (e:es)) <|> return []
    where
      elems = (do sep; e <- elem; es <- elems; return (e:es)) <|> return []

comma :: Parser ()
comma = char ','