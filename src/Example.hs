
module Example where

import           Control.Applicative
import qualified System.Environment.Parser as Env



data ConfGood = ConfGood
  { path :: String
  , term :: String
  }
  deriving ( Show )

data ConfBad = ConfBad
  { serverName :: String
  , timeZone   :: String
  }
  deriving ( Show )

confGood :: Env.Parser ConfGood
confGood = ConfGood <$> Env.get "PATH" <*> Env.get "TERM"

confBad :: Env.Parser ConfBad
confBad = ConfBad <$> Env.get "SERVER_NAME" <*> Env.get "TIME_ZONE"
