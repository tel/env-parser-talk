
module Example where

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
confGood = do
  pt <- Env.get "PATH"
  tm <- Env.get "TERM"
  return (ConfGood pt tm)

confBad :: Env.Parser ConfBad
confBad = do
  sN <- Env.get "SERVER_NAME"
  tZ <- Env.get "TIME_ZONE"
  return (ConfBad sN tZ)
