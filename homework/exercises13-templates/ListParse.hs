{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ListParse where

import Control.Applicative
import Control.Monad
import Parser

{- grammar:
 -   intList   = "{" { integer } "}"
 
 - In right-regular NF:
 - intList     = '{' tailIntList
 - tailIntList = int space tailIntList
               | '}'
 -}
intList :: Parser [Int]
{-
intList = do
    _ <- symbol "{"
    beleg <- many integer
    _ <- symbol "}"
    pure beleg
-}

intList = symbol "{" *> many integer <* symbol "}"
    

{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

intRecord :: Parser [Int]
{-
intRecord = do
    _     <- symbol "{"
    nr    <- integer
    _     <- symbol "#"
    beleg <- times nr integer
    _     <- symbol "}"
    pure beleg
-}
intRecord = symbol "{" *> integer >>= \nr -> (symbol "#" *> (times nr integer) <* symbol "}")
