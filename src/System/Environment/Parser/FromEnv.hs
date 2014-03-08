{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  System.Environment.Parser.FromEnv
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Types which instantiate 'FromEnv' have lexical representations in the
-- environment which we'd like to take advantage of for convenience. As
-- a basic example, all of 'String', 'S.ByteString', and 'T.Text'
-- instantiate 'FromEnv' allowing us to pick any string type as
-- a configuration parameter. As a more complex example, you might build
-- a type specifying a database connection URL format and parse it directly
-- out of the environment.
--

module System.Environment.Parser.FromEnv (

  FromEnv (..)

) where

import           Control.Monad
import qualified Data.Attoparsec.Text    as At
import qualified Data.ByteString         as S
import qualified Data.ByteString.Lazy    as Sl
import           Data.Int
import           Data.Scientific
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as Te
import qualified Data.Text.Lazy          as Tl
import qualified Data.Text.Lazy.Encoding as Tle

-- | Types which instantiate 'FromEnv' can be parsed from the environment.
-- If 'fromEnv' returns 'Left' then that error is collected and reported as
-- part of the reason why an environment parse has failed.
class FromEnv a where
  fromEnv :: S.ByteString -> Either String a

instance FromEnv S.ByteString where
  fromEnv = Right

instance FromEnv Sl.ByteString where
  fromEnv = Right . Sl.fromStrict

mapl :: (a -> b) -> Either a x -> Either b x
mapl f (Left a)  = Left (f a)
mapl _ (Right x) = Right x

instance FromEnv T.Text where
  fromEnv = mapl show . Te.decodeUtf8'

instance FromEnv Tl.Text where
  fromEnv = mapl show . Tle.decodeUtf8' <=< fromEnv

instance FromEnv String where
  fromEnv = fmap T.unpack . fromEnv

integralParse :: Integral a => S.ByteString -> Either String a
integralParse = fromEnv >=> At.parseOnly (At.signed At.decimal)

fractionalParse :: Fractional a => S.ByteString -> Either String a
fractionalParse = fromEnv >=> At.parseOnly (At.signed At.rational)

instance FromEnv Int     where fromEnv = integralParse
instance FromEnv Integer where fromEnv = integralParse
instance FromEnv Int8    where fromEnv = integralParse
instance FromEnv Int64   where fromEnv = integralParse
instance FromEnv Int32   where fromEnv = integralParse
instance FromEnv Int16   where fromEnv = integralParse

instance FromEnv Double     where fromEnv = fractionalParse
instance FromEnv Float      where fromEnv = fractionalParse
instance FromEnv Scientific where fromEnv = fractionalParse

instance FromEnv At.Number where
  fromEnv = fromEnv >=> At.parseOnly (At.signed At.number)

-- | Checks to be sure that a variable is assigned, but ignores its value.
instance FromEnv () where fromEnv _ = Right ()
