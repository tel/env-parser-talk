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

import System.Environment.Parser.Internal
