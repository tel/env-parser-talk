{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

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
import           System.Environment.Parser.Internal.Collect
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
deps = getConst . raise (\(Get s _) -> Const [s]) . unParser

-- | Runs a 'Parser' in the 'IO' monad, looking up the required environment
-- variables and using them to build the final value. In the event that
-- lookup fails this reports the name of the missing variable.
parse :: Parser a -> IO (Either [String] a)
parse = fmap collect . getCompose . raise phi . unParser where
  phi :: ParserF x -> (IO :.: Collect [String]) x
  phi (Get s go) = 
    Compose (collMay [s] go <$> lookupEnv s)

-- | Evaluates a 'Parser' purely in a fake environment. Compare this with
-- 'parse'. In the event that -- lookup fails this reports the name of the
-- missing variables.
test :: (String -> Maybe String) -> Parser a -> Either [String] a
test find (Parser p) = collect (raise phi p) where
  phi :: ParserF x -> Collect [String] x
  phi (Get s go) = collMay [s] go (find s)
