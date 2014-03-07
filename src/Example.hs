
module Example where

import           Control.Applicative
import qualified Data.ByteString           as S
import qualified System.Environment.Parser as Env



data ConfGood = ConfGood
  { path :: S.ByteString
  , term :: S.ByteString
  }
  deriving ( Show )

data ConfBad = ConfBad
  { serverName :: S.ByteString
  , timeZone   :: S.ByteString
  }
  deriving ( Show )

confGood :: Env.Parser ConfGood
confGood = ConfGood <$> Env.get "PATH" <*> Env.get "TERM"

confBad :: Env.Parser ConfBad
confBad = ConfBad <$> Env.get "SERVER_NAME" <*> Env.get "TIME_ZONE"
