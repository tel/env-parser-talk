
module System.Environment.Parser where

data Parser a
instance Monad Parser

get :: String -> Parser String
get = undefined
