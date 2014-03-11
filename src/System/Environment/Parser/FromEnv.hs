{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
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

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text    as At
import           Data.Bits
import qualified Data.ByteString         as S
import qualified Data.ByteString.Lazy    as Sl
import qualified Data.CaseInsensitive    as Ci
import           Data.Int
import           Data.Ratio
import           Data.Scientific
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as Te
import qualified Data.Text.Lazy          as Tl
import qualified Data.Text.Lazy.Encoding as Tle
import           Data.Word
import qualified Foreign.C.Types         as Cty
import qualified System.Posix.Types      as Posix

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
instance FromEnv Int16   where fromEnv = integralParse
instance FromEnv Int32   where fromEnv = integralParse
instance FromEnv Int64   where fromEnv = integralParse

-- | Try parsing as a hex numer or an decimal number
byteParse :: (Integral a, Bits a) => S.ByteString -> Either String a
byteParse = fromEnv >=> At.parseOnly byteParser where
  byteParser :: (Integral a, Bits a) => At.Parser a
  byteParser =
    At.choice [ At.string "0x" *> At.hexadecimal
              , fromIntegral <$> (At.decimal :: At.Parser Integer)
              ]

instance FromEnv Word8    where fromEnv = byteParse
instance FromEnv Word16   where fromEnv = byteParse
instance FromEnv Word32   where fromEnv = byteParse
instance FromEnv Word64   where fromEnv = byteParse

instance FromEnv Double     where fromEnv = fractionalParse
instance FromEnv Float      where fromEnv = fractionalParse
instance FromEnv Scientific where fromEnv = fractionalParse

instance FromEnv At.Number where
  fromEnv = fromEnv >=> At.parseOnly (At.signed At.number)

ratioParse :: Integral a => S.ByteString -> Either String (Ratio a)
ratioParse = fromEnv >=> At.parseOnly ratioParser where
  ratioParser :: Integral a => At.Parser (Ratio a)
  ratioParser  = (%) <$> At.signed At.decimal
                     <*> ( (At.char '/' *> At.decimal)
                            <|>
                            pure 1 )

instance FromEnv (Ratio Int)     where fromEnv = ratioParse
instance FromEnv (Ratio Integer) where fromEnv = ratioParse
instance FromEnv (Ratio Int8)    where fromEnv = ratioParse
instance FromEnv (Ratio Int16)   where fromEnv = ratioParse
instance FromEnv (Ratio Int32)   where fromEnv = ratioParse
instance FromEnv (Ratio Int64)   where fromEnv = ratioParse

instance (Ci.FoldCase s, FromEnv s) => FromEnv (Ci.CI s) where
  fromEnv = fmap Ci.mk . fromEnv

-- | Checks to be sure that a variable is assigned, but ignores its value.
instance FromEnv () where fromEnv _ = Right ()

instance FromEnv Bool where
  fromEnv = fromEnv >=> check where
    check :: Ci.CI String -> Either String Bool
    check "true"  = Right True
    check "false" = Right False
    check "yes"   = Right True
    check "no"    = Right False
    check "1"     = Right True
    check "0"     = Right False
    check s       = Left ("not a valid bool: " ++ show (Ci.foldedCase s))

instance FromEnv Posix.CNlink where
  fromEnv = fmap Posix.CNlink . fromEnv
instance FromEnv Posix.CUid where
  fromEnv = fmap Posix.CUid . fromEnv
instance FromEnv Posix.CGid where
  fromEnv = fmap Posix.CGid . fromEnv
instance FromEnv Posix.COff where
  fromEnv = fmap Posix.COff . fromEnv
instance FromEnv Posix.CPid where
  fromEnv = fmap Posix.CPid . fromEnv
instance FromEnv Posix.CDev where
  fromEnv = fmap Posix.CDev . fromEnv
instance FromEnv Posix.CIno where
  fromEnv = fmap Posix.CIno . fromEnv
instance FromEnv Posix.CMode where
  fromEnv = fmap Posix.CMode . fromEnv

instance FromEnv Cty.CChar where
  fromEnv = fmap Cty.CChar . fromEnv
instance FromEnv Cty.CSChar where
  fromEnv = fmap Cty.CSChar . fromEnv
instance FromEnv Cty.CUChar where
  fromEnv = fmap Cty.CUChar . fromEnv
instance FromEnv Cty.CShort where
  fromEnv = fmap Cty.CShort . fromEnv
instance FromEnv Cty.CUShort where
  fromEnv = fmap Cty.CUShort . fromEnv
instance FromEnv Cty.CInt where
  fromEnv = fmap Cty.CInt . fromEnv
instance FromEnv Cty.CUInt where
  fromEnv = fmap Cty.CUInt . fromEnv
instance FromEnv Cty.CLong where
  fromEnv = fmap Cty.CLong . fromEnv
instance FromEnv Cty.CLLong where
  fromEnv = fmap Cty.CLLong . fromEnv
instance FromEnv Cty.CULong where
  fromEnv = fmap Cty.CULong . fromEnv
instance FromEnv Cty.CFloat where
  fromEnv = fmap Cty.CFloat . fromEnv
instance FromEnv Cty.CDouble where
  fromEnv = fmap Cty.CDouble . fromEnv
instance FromEnv Cty.CWchar where
  fromEnv = fmap Cty.CWchar . fromEnv
instance FromEnv Cty.CSize where
  fromEnv = fmap Cty.CSize . fromEnv
instance FromEnv Cty.CClock where
  fromEnv = fmap Cty.CClock . fromEnv
instance FromEnv Cty.CTime where
  fromEnv = fmap Cty.CTime . fromEnv
instance FromEnv Cty.CUSeconds where
  fromEnv = fmap Cty.CUSeconds . fromEnv
instance FromEnv Cty.CSUSeconds where
  fromEnv = fmap Cty.CSUSeconds . fromEnv
instance FromEnv Cty.CIntPtr where
  fromEnv = fmap Cty.CIntPtr . fromEnv
instance FromEnv Cty.CUIntPtr where
  fromEnv = fmap Cty.CUIntPtr . fromEnv
