module Basic where

import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a,String)] }

endOfInput :: Parser ()
endOfInput = Parser (\input -> case input of
                                 [] -> [((),"")]
                                 _  -> [])

readChar :: Parser Char
readChar = Parser (\input -> case input of
                               []       -> []
                               c:input' -> [(c,input')])

token :: Parser (Maybe Char)
token = Parser (\input -> case input of
                            ""       -> [(Nothing,"")]
                            c:input' -> [(Just c,input')])

instance Monad Parser where
    return x = Parser (\input -> [(x,input)])
    p >>= f  = Parser (\input -> concatMap (\(a,input') -> parse (f a) input')
                                           (parse p input))

instance Functor Parser where
    fmap f p = Parser (\input -> map (\(a,s) -> (f a, s)) (parse p input))

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Alternative Parser where
    empty   = Parser (\input -> [])
    p <|> q = Parser (\input -> parse p input ++ parse q input)

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)