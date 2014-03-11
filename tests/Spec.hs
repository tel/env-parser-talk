{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy        as Sl
import qualified Data.CaseInsensitive        as Ci
import           Data.Int
import           Data.Ratio
import           Data.String
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as Te
import qualified Data.Text.Lazy              as Tl
import qualified Data.Text.Lazy.Encoding     as Tle
import           Data.Word
import qualified System.Environment.Parser   as Env
import           System.Posix.Env.ByteString
import           Test.Helpers
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

-- | Generate a gibberish, @[a-zA-Z]@, and non-empty \"name\" of
-- minimally-bounded length.
arbitraryName :: Int -> Gen String
arbitraryName len = listOf (oneof [choose ('a', 'z'), choose ('A', 'Z')]) `suchThat` \n -> length n > len

arbitraryNameBS :: Int -> Gen S.ByteString
arbitraryNameBS = fmap S8.pack . arbitraryName

main :: IO ()
main = hspec $ do

  describe "Utilities" $ do
    describe "FreeSNR" $ do
      context "Properties" $ do
        it "0 + x == x" pending
        it "x + 0 == x" pending
        it "1 * x == x" pending
        it "x * 1 == x" pending
        it "0 * x == 0" pending
        it "x * 0 == 0" pending
        it "(x + y)z == xz + yz" pending
    describe "Collect" $ do
      context "Properties" $ do
        it "monoid laws" pending

  -- Parsing is the action of actually pulling information from the ENV
  -- dictionary (or something like it) in order to build the type
  -- contextually wrapped in the 'Env.Parser'.
  describe "Parsing" $ do

    -- the Mocked context tests 'Env.test', ensuring that we can mock out
    -- environment querying when desired. These are also, nicely, pure
    -- tests.
    context "Mocked" $ do
      context "it should successfully parse when" $ do
        it "the environment is complete" pending
        it "the environment is incomplete, but missing values have defaults"
          pending
      context "it should fail to parse when" $ do
        it "missing some set of variables without defaults" pending

    -- the Real context tests 'Env.parse' and thus requires actually
    -- modifying the ENV to properly test.
    context "Real" $ do
      it "should have tests" $ pendingWith "good method of testing this"


  -- Documentation is the action of pulling information in a pure and
  -- static way from a Parser context without needing access to the
  -- environment. It should provide complete information about the Parser
  -- such as all of the environment variables required to successfully
  -- gather the needed information and the documentation around each one.
  describe "Documentation" $ do
    it "should have tests"
      $ pendingWith "documentation interface design/alternative instance"
    it "should show defaults properly" pending

  -- Deserialization is "parsing", except that we already used that term.
  -- In particular, deserialization means taking a ByteString from the
  -- environment and converting it to a value of the desired type. This
  -- mostly describes the actions of built-in FromEnv instances.
  describe "Deserialization" $ do
    describe "FromEnv" $ do
      it "should have tests"
        $ pendingWith "descriptions of intended behaviors"

      describe "String-alikes" $ do
        describe "Roundtrips" $ do
          it "String"          $ rt id strUtf8
          it "ByteString"      $ rt strUtf8 id
          it "Lazy.ByteString" $ rt strUtf8l Sl.toStrict
          it "Text"            $ rt T.pack Te.encodeUtf8
          it "Lazy.Text"       $ rt Tl.pack (Sl.toStrict . Tle.encodeUtf8)

      describe "Number-alikes" $ do
        describe "Roundtrips" $ do
          it "Int"           $ rti (P      :: P Int)               utf8
          it "Integer"       $ rti (P      :: P Integer)           utf8
          it "Int8"          $ rti (P      :: P Int8)              utf8
          it "Int16"         $ rti (P      :: P Int16)             utf8
          it "Int32"         $ rti (P      :: P Int32)             utf8
          it "Int64"         $ rti (P      :: P Int64)             utf8
          it "Double"        $ rti (P      :: P Double)            utf8
          it "Float"         $ rti (P      :: P Float)             utf8
          it "Ratio Int"     $ rt' (ratGen :: Gen (Ratio Int))     prRatio
          it "Ratio Integer" $ rt' (ratGen :: Gen (Ratio Integer)) prRatio
          -- it "Ratio Int8"    $ rt' (ratGen :: Gen (Ratio Int8))    prRatio
          it "Ratio Int8"    $ pendingWith "fix overflow errors"
          it "Ratio Int16"   $ rt' (ratGen :: Gen (Ratio Int16))   prRatio
          it "Ratio Int32"   $ rt' (ratGen :: Gen (Ratio Int32))   prRatio
          it "Ratio Int64"   $ rt' (ratGen :: Gen (Ratio Int64))   prRatio
          it "Scientific"    $ pendingWith "How to display Scientific"
          it "Atto.Number"   $ pendingWith "How to display Atto.Number"

      describe "Time types" $ do
        describe "UTCTime formats" $ do
          it "should have tests"
            $ pendingWith "pick formats"
        it "NominalDiffTime in seconds" pending
        it "DiffTime in seconds"        pending
        it "DotNetTime"                 pending
        it "ZonedTime"                  pending

      describe "Bits" $ do
        describe "Numeric parse" $ do
          describe "Roundtrips" $ do
            it "Word"   pending
            it "Word8"  pending
            it "Word16" pending
            it "Word32" pending
            it "Word64" pending
        describe "Hex parse" $ do
          describe "Roundtrips" $ do
            it "Word"   pending
            it "Word8"  pending
            it "Word16" pending
            it "Word32" pending
            it "Word64" pending
          describe "Units" $ do
            it "should parse the right number of digits" $
              Env.fromEnv "0x10" `shouldBe` (Right (16 :: Word8))
            it "should not truncate parses" $
              let isLeft e = case e of { Left _ -> True; _ -> False }
              in (Env.fromEnv "0x0fff" :: Either String Word8)
                   `shouldSatisfy` isLeft

      describe "Default types" $ do
        context "()" $ do
          it "should always succeed" $ property $ \s ->
            Env.fromEnv (S8.pack s) == Right ()
        context "Bool" $ do
          context "total lexical space (case insensitive" $ do
            it "true"  $ Env.fromEnv "true"  == Right True
            it "yes"   $ Env.fromEnv "yes"   == Right True
            it "1"     $ Env.fromEnv "1"     == Right True
            it "no"    $ Env.fromEnv "no"    == Right False
            it "false" $ Env.fromEnv "false" == Right False
            it "0"     $ Env.fromEnv "0"     == Right False
        it "Char" $ pendingWith "is this even a meaningful instance?"

      describe "Special types" $ do
        it "System.ExitCode" pending
        describe "System.Posix.Types" $ do
          it "LinkCount"      pending
          it "UserID"         pending
          it "GroupID"        pending
          it "ByteCount"      pending
          it "ClockTick"      pending
          it "EpochTime"      pending
          it "FileOffset"     pending
          it "ProcessID"      pending
          it "ProcessGroupID" pending
          it "DeviceID"       pending
          it "FileID"         pending
          it "FileMode"       pending
          it "Limit"          pending

      -- The current fromEnv :: S.ByteString -> Either String a
      -- interace is too weak to make this work out... Hm.
      describe "Lists and Sets" $ do
        it "FromEnv a => [a]"   pending
        it "IntSet"             pending
        it "FromEnv a => Set a" pending

      describe "Modifiers" $ do
        it "FromEnv a => Maybe a"
          $ pendingWith "define semantics: must exist, may fail parse?"
        it "FromEnv a, b => Either a b"
          $ pendingWith "define semantics"
        context "CI" $ do
          describe "Roundtrips" $ do
            it "String"
              $ rt (Ci.mk :: String -> Ci.CI String) 
                   (strUtf8 . Ci.foldedCase)
            it "ByteString"
              $ rt (Ci.mk . strUtf8) Ci.foldedCase
            it "Lazy.ByteString"
              $ rt (Ci.mk . strUtf8l) (Sl.toStrict . Ci.foldedCase)
            it "Text"
              $ rt (Ci.mk . T.pack) (Te.encodeUtf8 . Ci.foldedCase)
            it "Lazy.Text"
              $ rt (Ci.mk . Tl.pack) (  Sl.toStrict 
                                      . Tle.encodeUtf8
                                      . Ci.foldedCase  )

      describe "Network Typs" $ do
        describe "Network.BSD" $ do
          it "PortNumber" pending
        describe "Network.Socket" $ do
          it "Family"     pending
          it "SocketType" pending
          it "SocketAddr" pending
        describe "Network.URI" $ do
          it "URI"        pending

      describe "Tuples/Bash Arrays" $ do
        it "1 tuple"  pending
        it "2 tuple"  pending
        it "3 tuple"  pending
        it "4 tuple"  pending
        it "5 tuple"  pending
        it "6 tuple"  pending
        it "7 tuple"  pending
        it "8 tuple"  pending
        it "9 tuple"  pending
        it "10 tuple" pending
        it "11 tuple" pending
        it "12 tuple" pending
        it "13 tuple" pending
        it "14 tuple" pending
        it "15 tuple" pending

      describe "DBConn" $ do
        it "should work" pending

    describe "JSON deserialization" $ do
      it "should work" pending

    describe "Read deserialization" $ do
      it "should work" pending
