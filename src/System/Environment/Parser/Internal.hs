{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  System.Environment.Parser.Internal
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--

module System.Environment.Parser.Internal where

import           Control.Applicative
import           System.Environment
import           System.Environment.Parser.Internal.FreeA

-- | The signature of the 'Parser' applicative.
data ParserF x
  = Get String (String -> x)
  deriving ( Functor )

-- | The 'Parser' type is an effectual context where you can use ENV
-- information to build a value containing configuration information.
newtype Parser a = Parser { unParser :: FreeA ParserF a }
  deriving ( Functor, Applicative )

-- | Look up a key in the ENV. This operation may fail if the key is
-- missing.
get :: String -> Parser String
get s = Parser . one $ Get s id

-- | Compute all ENV variables which are required in order to run
-- a 'Parser'.
deps :: Parser a -> [String]
deps (Parser (Pure _)) = []
deps (Parser (Get s _ :$: free)) = s : deps (Parser free)

parse :: Parser a -> IO (Either [String] a)
parse (Parser (Pure a)) = return (Right a)
parse (Parser (Get s f :$: free)) = do
  mayVal  <- lookupEnv s
  eitRest <- parse (Parser free)
  return $ case (mayVal, eitRest) of
    (Just val, Right rest) -> Right (f val rest)
    (Nothing , Left es   ) -> Left (s:es)
    (Nothing , _         ) -> Left [s]
    (_       , Left es   ) -> Left es

