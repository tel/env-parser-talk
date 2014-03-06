
import           Control.Applicative
import           Control.Monad
import qualified System.Environment.Parser as Env
import           System.Posix.Env
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

-- | Generate a random, gibberish, @[a-zA-Z]@, and non-empty \"name\" of
-- minimally-bounded length.
arbitraryName :: Int -> Gen String
arbitraryName len = listOf (oneof [choose ('a', 'z'), choose ('A', 'Z')]) `suchThat` \n -> length n > len

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "should be able to find the \"PATH\" variable" $ do
      void (Env.parse (Env.get "PATH")) `shouldReturn` ()

    -- This test is unstable since we'd like to eliminate the
    -- error-throwing failure API, but for now here it is.
    it "should not be able to find the \"da39a3ee5e6b4b0d3255bfef95601890afd80709\" variable" $ do
      Env.parse (Env.get "da39a3ee5e6b4b0d3255bfef95601890afd80709")
        `shouldReturn` (Left ["da39a3ee5e6b4b0d3255bfef95601890afd80709"])

    it "should report multiple missing variables" $ do
      Env.parse ((,) <$> Env.get "FIRST_MISSING_VALUE" <*> Env.get "SECOND_MISSING_VALUE")
        `shouldReturn` (Left ["FIRST_MISSING_VALUE", "SECOND_MISSING_VALUE"])

    it "should have that (setEnv k v >> Env.get k ===> v) for all k" $ monadicIO $ do
      name <- pick (arbitraryName 6)
      val  <- pick (arbitraryName 20)
      val' <- run $ do setEnv name val True -- overwrites random ENV variables...
                       Env.parse (Env.get name)
      assert (Right val == val')

