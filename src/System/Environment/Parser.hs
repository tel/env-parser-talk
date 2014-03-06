{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  System.Environment.Parser
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--

module System.Environment.Parser where

import           System.Environment

-- | The 'Parser' type is an effectual context where you can use ENV
-- information to build a value containing configuration information.
newtype Parser a = Parser ( IO (Either [String] a) )

instance Monad Parser where
  return a = Parser (return (Right a))
  Parser ioa >>= f = Parser $ do
    ea <- ioa
    case ea of
      Left err -> return $ Left err
      Right a  -> parse  $ f a

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
