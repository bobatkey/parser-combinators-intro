module Example3 where

import Basic
import Derived
import Control.Applicative
import Control.Monad

data Node = Element String [Node]
          deriving Show

node = do nm  <- startTag
          nodes <- many node
          endTag nm
          return (Element nm nodes)

startTag = do char '<'
              nm <- identifier
              char '>'
              return nm

endTag nm = do char '<'
               char '/'
               nm' <- identifier
               char '>'
               guard (nm==nm')
