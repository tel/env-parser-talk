-- |
-- Module      :  System.Environment.Parser
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- (This module is intended to be imported qualified.)
-- 
-- The environment 'Parser' is an 'Applicative' context for constructing
-- values which depend upon the environment (the ENV). This is preferable
-- to using 'getEnv' or 'lookupEnv' explicitly since it provides a clear
-- type for the purpose and is amenable to analysis.
-- 
-- Given a configuration type such as
-- 
-- > data Config = Config
-- >   { serverName :: String
-- >   , timeZone   :: String
-- >   , salutation :: String
-- >   } 
-- 
-- we can parse its value from the environment using the 'Parser'
-- 
-- > import qualified System.Environment.Parser as Env
-- >
-- > configP :: Env.Parser Config
-- > configP = Config <$> Env.get "SERVER_NAME"
-- >                  <*> Env.get "TIME_ZONE"
-- >                  <*> Env.get "SERVER_SALUTATION"
-- 
-- and then we can execute this lookup in 'IO' using 'parse'.
-- 
-- > Env.parse configP :: IO (Either [String] Config)
-- 
-- which also provides the complete set of missing environment variables
-- upon failure. Already this is more functional than 'getEnv'.
-- 
-- Additionally, we can analyze a 'Parser' to compute /all/ of the
-- environment variables it depends upon as a pure action
-- 
-- >>> Env.deps configP == ["SERVER_NAME", "TIME_ZONE", "SERVER_SALUTATION"]
-- True

module System.Environment.Parser (

    Parser
  , parse
  , deps
  , get
                                 
) where

import           Control.Applicative
import           System.Environment

-- | The 'Parser' type is an effectual context where you can use ENV
-- information to build a value containing configuration information.
data Parser a = Parser [String] ( IO (Either [String] a) )

instance Functor Parser where
  fmap f (Parser ns io) = Parser ns (fmap (fmap f) io)

instance Applicative Parser where
  pure a = Parser [] (pure (pure a))
  Parser ns1 iof <*> Parser ns2 iox = Parser (ns1 ++ ns2) $ do
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
parse (Parser _ ioe) = ioe

-- | Compute all ENV variables which are required in order to run
-- a 'Parser'.
deps :: Parser a -> [String]
deps (Parser ns _) = ns

-- | Look up a key in the ENV. This operation may fail if the key is
-- missing.
get :: String -> Parser String
get s = Parser [s] $ do
  ma <- lookupEnv s
  return $ case ma of
    Nothing -> Left [s]
    Just a  -> Right a
