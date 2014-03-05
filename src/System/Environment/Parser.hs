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
newtype Parser a = Parser { parse :: IO a }
  deriving ( Functor, Monad )

-- | Look up a key in the ENV. This operation may fail if the key is
-- missing.
get :: String -> Parser String
get s = Parser (getEnv s)
