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
import           Control.Monad
import qualified Data.Aeson                                 as Ae
import qualified Data.ByteString                            as S
import           System.Environment.Parser.FromEnv
import           System.Environment.Parser.Internal.Collect
import           System.Environment.Parser.Internal.FreeA
import           System.Environment.Parser.Slot
import           System.Posix.Env.ByteString

-- | The signature of the 'Parser' applicative.
data ParserF x
  = Get {-# UNPACK #-} !(Slot x)
                       !(S.ByteString -> Either String x)

getSlot :: ParserF a -> Slot a
getSlot (Get s _) = s

unpickle :: ParserF a -> S.ByteString -> Either String a
unpickle (Get _ f) = f

instance Functor ParserF where
  fmap f (Get s go) = Get (fmap f s) (fmap f . go)

-- | The 'Parser' type is an effectual context where you can use ENV
-- information to build a value containing configuration information.
newtype Parser a = Parser { unParser :: FreeA ParserF a }
  deriving ( Functor, Applicative )

-- | If a 'parse' (or 'test') fails then all of the runtime errors are
-- collected. 'Error' classifies these various kinds of errors.
data Error
  = MissingName        -- ^ The requested name was not defined
  | ParseError String  -- ^ The value failed to parse with an error
  deriving ( Eq, Ord, Show, Read )

-- | Look up a key in the ENV. This operation may fail if the key is
-- missing or if the value cannot be parsed according to its 'FromEnv'
-- instance.
get' :: (S.ByteString -> Either String a) -> Slot a -> Parser a
get' phi s = Parser . one $ Get s phi

-- | Look up a key in the ENV. This operation may fail if the key is
-- missing or if the value cannot be parsed according to its 'FromEnv'
-- instance.
get :: FromEnv a => Slot a -> Parser a
get = get' fromEnv

-- | Look up a key containing a JSON value in the ENV. This operation may
-- fail if the key is missing or if the value cannot be parsed according to
-- its 'Ae.FromJSON' instance.
json :: Ae.FromJSON a => Slot a -> Parser a
json = get' (fromEnv >=> Ae.eitherDecode)

-- | Look up a key containing a serialized Haskell value in the ENV. This
-- operation may fail if the key is missing or if the value cannot be
-- parsed according to its 'Read' instance.
read :: Read a => Slot a -> Parser a
read = get' go where
  go :: Read a => S.ByteString -> Either String a
  go bs = do
    str <- fromEnv bs
    case reads str of
      [(a, "")] -> Right a
      _         -> Left ("read failed: " ++ str)

-- | Compute all ENV variables which are required in order to run
-- a 'Parser'.
deps :: Parser a -> [ASlot]
deps = getConst . raise phi . unParser where
  phi g = Const [aSlot (getSlot g)]

-- | The core error handling component used in both 'parse' and 'test'
run :: ParserF b
    -> Maybe S.ByteString
    -> Maybe b
    -> Collect [(S.ByteString, Error)] b
run g Nothing   Nothing  = miss [(getKey $ getSlot g, MissingName)]
run _ Nothing   (Just a) = have a
run g (Just bs) _ =
  case unpickle g bs of
    Left err -> miss [(getKey $ getSlot g, ParseError err)]
    Right v  -> have v

-- | Runs a 'Parser' in the 'IO' monad, looking up the required environment
-- variables and using them to build the final value. In the event that
-- lookup fails this reports the name of the missing variable.
parse :: Parser a -> IO (Either [(S.ByteString, Error)] a)
parse = fmap collect . getCompose . raise phi . unParser where
  phi :: ParserF x -> (IO :.: Collect [(S.ByteString, Error)]) x
  phi g = Compose $ do
    let s = getSlot g
    x <- getEnv (getKey s)
    return (run g x (getDef s))

-- | Evaluates a 'Parser' purely in a fake environment. Compare this with
-- 'parse'. In the event that -- lookup fails this reports the name of the
-- missing variables.
test :: (S.ByteString -> Maybe S.ByteString)
     -> Parser a -> Either [(S.ByteString, Error)] a
test find (Parser p) = collect (raise phi p) where
  phi :: ParserF x -> Collect [(S.ByteString, Error)] x
  phi g =
    let s = getSlot g
    in run g (find (getKey s)) (getDef s)
