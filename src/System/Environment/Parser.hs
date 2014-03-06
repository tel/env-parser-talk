-- |
-- Module      :  System.Environment.Parser
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--

module System.Environment.Parser where

import           Control.Applicative
import           System.Environment

-- | The 'Parser' type is an effectual context where you can use ENV
-- information to build a value containing configuration information.
newtype Parser a = Parser ( IO (Either [String] a) )

instance Functor Parser where
  fmap f (Parser io) = Parser (fmap (fmap f) io)

instance Applicative Parser where
  pure a = Parser (pure (pure a))
  Parser iof <*> Parser iox = Parser $ do
    ef <- iof
    ex <- iox
    return $ case (ef, ex) of
      (Right f, Right x) -> Right (f x)
      (Left e1, Left e2) -> Left (e1 ++ e2) -- < This is important!
      (Left e , _      ) -> Left e
      (_      , Left e ) -> Left e

-- | Runs a 'Parser' in the 'IO' monad, looking up the required environment
-- variables and using them to build the final value. In the event that
-- lookup fails this reports the name of the missing variable.
parse :: Parser a -> IO (Either [String] a)
parse (Parser ioe) = ioe

-- | Look up a key in the ENV. This operation may fail if the key is
-- missing.
get :: String -> Parser String
get s = Parser $ do
  ma <- lookupEnv s
  return $ case ma of
    Nothing -> Left [s]
    Just a  -> Right a
