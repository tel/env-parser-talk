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

-- | Generate a random, gibberish, @[a-zA-Z]@, and non-empty \"name\" of
-- minimally-bounded length.
arbitraryName :: Int -> Gen String
arbitraryName len = listOf (oneof [choose ('a', 'z'), choose ('A', 'Z')]) `suchThat` \n -> length n > len

arbitraryNameBS :: Int -> Gen S.ByteString
arbitraryNameBS = fmap S8.pack . arbitraryName

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    describe "Basics" $ do
      it "should be able to find the \"PATH\" variable" $ do
        void (Env.parse (Env.get "PATH" :: Env.Parser Int)) `shouldReturn` ()

      -- This test is unstable since we'd like to eliminate the
      -- error-throwing failure API, but for now here it is.
      it "should not be able to find the \"da39a3ee5e6b4b0d3255bfef95601890afd80709\" variable" $ do
        Env.parse (Env.get "da39a3ee5e6b4b0d3255bfef95601890afd80709" :: Env.Parser Int)
          `shouldReturn` 
          (Left [( "da39a3ee5e6b4b0d3255bfef95601890afd80709"
                , Env.MissingName
                )])

      it "should report multiple missing variables" $ do
        Env.parse ((,) <$> Env.get "FIRST_MISSING_VALUE" 
                       <*> Env.get "SECOND_MISSING_VALUE" :: Env.Parser (Int, Int))
          `shouldReturn` 
            (Left [ ( "FIRST_MISSING_VALUE" , Env.MissingName )
                  , ( "SECOND_MISSING_VALUE", Env.MissingName )
                  ])

      it "holds that (setEnv k v >> Env.get k ===> v) for all k" $
        monadicIO $ do
          name <- pick (arbitraryNameBS 6)
          val  <- pick (arbitraryNameBS 20)
          -- overwrites random ENV variables...
          val' <- run $ do setEnv name val True
                           Env.parse (Env.get (Env.slot name) :: Env.Parser S.ByteString)
          assert (Right val == val')

      it "should find all of the needed names as a pure computation" $ do
        Env.deps ((,) <$> Env.get "FIRST_MISSING_VALUE"
                      <*> Env.get "SECOND_MISSING_VALUE" :: Env.Parser (Int, Int))
          `shouldBe` [ ("FIRST_MISSING_VALUE", Nothing)
                     , ("SECOND_MISSING_VALUE", Nothing)
                     ]

      it "holds that (Left . map fixup . Env.deps == Env.test (const Nothing))" $
        property $ forAll (arbitraryNameBS 6) $ \e ->
          let s        = Env.get (Env.slot e) :: Env.Parser Int
              depErrs  = map (\(n, _doc) -> (n, Env.MissingName)) (Env.deps s)
              testErrs = Env.test (const Nothing) s
          in Left depErrs == testErrs

    describe "Parsing types" $ do
      it "parses Strings properly" pending
      it "parses ByteStrings properly" pending
      it "parses Text chunks properly" pending
