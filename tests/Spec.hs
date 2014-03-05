
import           Control.Monad
import qualified System.Environment.Parser as Env
import           Test.Hspec
import           Test.Hspec.Expectations

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "should be able to find the \"PATH\" variable" $ do
      void (Env.parse (Env.get "PATH")) `shouldReturn` ()

    -- This test is unstable since we'd like to eliminate the
    -- error-throwing failure API, but for now here it is.
    it "should not be able to find the \"da39a3ee5e6b4b0d3255bfef95601890afd80709\" variable" $ do
      Env.parse (Env.get "da39a3ee5e6b4b0d3255bfef95601890afd80709") `shouldThrow` anyIOException

