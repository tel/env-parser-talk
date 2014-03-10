{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as S8
import qualified System.Environment.Parser   as Env
import           System.Posix.Env.ByteString
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Data.String

-- | Generate a gibberish, @[a-zA-Z]@, and non-empty \"name\" of
-- minimally-bounded length.
arbitraryName :: Int -> Gen String
arbitraryName len = listOf (oneof [choose ('a', 'z'), choose ('A', 'Z')]) `suchThat` \n -> length n > len

arbitraryNameBS :: Int -> Gen S.ByteString
arbitraryNameBS = fmap S8.pack . arbitraryName

main :: IO ()
main = hspec $ do

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
        it "String"          pending
        it "ByteString"      pending
        it "Lazy.ByteString" pending
        it "Text"            pending
        it "Lazy.Text"       pending

      describe "Number-alikes" $ do
        it "Int"               pending
        it "Integer"           pending
        it "Int8"              pending
        it "Int16"             pending
        it "Int32"             pending
        it "Int64"             pending
        it "Double"            pending
        it "Float"             pending
        it "Scientific"        pending
        it "Attoparsec.Number" pending
        it "Ratio Int"         pending
        it "Ratio Integer"     pending
        it "Ratio Int8"        pending
        it "Ratio Int16"       pending
        it "Ratio Int32"       pending
        it "Ratio Int64"       pending

      describe "Time types" $ do
        describe "UTCTime formats" $ do
          it "should have tests"
            $ pendingWith "pick formats"
        it "NominalDiffTime in seconds" pending
        it "DiffTime in seconds"        pending
        it "DotNetTime"                 pending
        it "ZonedTime"                  pending

      describe "Bits" $ do
        it "Word"   pending
        it "Word8"  pending
        it "Word16" pending
        it "Word32" pending
        it "Word64" pending

      describe "Default types" $ do
        it "()"   pending
        it "Bool" pending
        it "Char" pending

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
          it "Char"              pending
          it "String"            pending
          it "ByteString"        pending
          it "Lazy.ByteString"   pending
          it "Text"              pending
          it "Lazy.Text"         pending
          it "FoldText a => [a]" pending

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
