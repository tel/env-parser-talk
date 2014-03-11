{-# LANGUAGE ScopedTypeVariables #-}

module Test.Helpers where

import           Control.Applicative
import qualified Data.ByteString                   as S
import qualified Data.ByteString.Lazy              as Sl
import           Data.Ratio
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as Te
import qualified Data.Text.Lazy                    as Tl
import qualified Data.Text.Lazy.Encoding           as Tle
import qualified System.Environment.Parser         as Env
import qualified System.Environment.Parser.FromEnv as FromEnv
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.QuickCheck

data P a = P

utf8 :: Show a => a -> S.ByteString
utf8 = strUtf8 . show

strUtf8 :: String -> S.ByteString
strUtf8 = Te.encodeUtf8 . T.pack

strUtf8l :: String -> Sl.ByteString
strUtf8l = Tle.encodeUtf8 . Tl.pack

rti :: (Arbitrary a, Eq a, Show a, Env.FromEnv a)
    => proxy a -> (a -> S.ByteString) -> Property
rti _ = rt (id :: a -> a)

rt
  :: (Eq a, Show a, FromEnv.FromEnv a, Arbitrary o) =>
     (o -> a) -> (a -> S.ByteString) -> Property
rt g = rt' (g <$> arbitrary)

rt' :: (Eq a, Show a, Env.FromEnv a)
   => Gen a -> (a -> S.ByteString) -> Property
rt' g f = forAll g $ \a ->
  case FromEnv.fromEnv (f a) of
    Left err -> False
    Right a' -> a == a'

ratGen :: (Integral a, Arbitrary a) => Gen (Ratio a)
ratGen = (%) <$> arbitrary <*> arbitrary `suchThat` (/=0)

prRatio :: (Integral a, Show a) => Ratio a -> S.ByteString
prRatio r = 
  strUtf8 $ show (numerator r) ++ "/" ++ show (denominator r)
