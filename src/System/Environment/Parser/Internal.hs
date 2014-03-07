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
import qualified Data.ByteString                            as S
import           System.Posix.Env.ByteString
import           System.Environment.Parser.Internal.Collect
import           System.Environment.Parser.Internal.FreeA

-- | The signature of the 'Parser' applicative.
data ParserF x
  = Get S.ByteString (S.ByteString -> x)
  deriving ( Functor )

-- | The 'Parser' type is an effectual context where you can use ENV
-- information to build a value containing configuration information.
newtype Parser a = Parser { unParser :: FreeA ParserF a }
  deriving ( Functor, Applicative )

-- | Look up a key in the ENV. This operation may fail if the key is
-- missing.
get :: S.ByteString -> Parser S.ByteString
get s = Parser . one $ Get s id

-- | Compute all ENV variables which are required in order to run
-- a 'Parser'.
deps :: Parser a -> [S.ByteString]
deps = getConst . raise (\(Get s _) -> Const [s]) . unParser

-- | Runs a 'Parser' in the 'IO' monad, looking up the required environment
-- variables and using them to build the final value. In the event that
-- lookup fails this reports the name of the missing variable.
parse :: Parser a -> IO (Either [S.ByteString] a)
parse = fmap collect . getCompose . raise phi . unParser where
  phi :: ParserF x -> (IO :.: Collect [S.ByteString]) x
  phi (Get s go) =
    Compose (collMay [s] go <$> getEnv s)

-- | Evaluates a 'Parser' purely in a fake environment. Compare this with
-- 'parse'. In the event that -- lookup fails this reports the name of the
-- missing variables.
test :: (S.ByteString -> Maybe S.ByteString) 
     -> Parser a -> Either [S.ByteString] a
test find (Parser p) = collect (raise phi p) where
  phi :: ParserF x -> Collect [S.ByteString] x
  phi (Get s go) = collMay [s] go (find s)
