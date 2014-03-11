{-# LANGUAGE OverloadedStrings #-}

module Example where

import           Control.Applicative
import qualified Data.ByteString           as S
import qualified Data.Text                 as T
import qualified System.Environment.Parser as Env



data ConfGood = ConfGood
  { path :: S.ByteString
  , term :: T.Text
  }
  deriving ( Show )

data ConfBad = ConfBad
  { serverName :: T.Text
  , timeZone   :: String
  }
  deriving ( Show )

confGood :: Env.Parser ConfGood
confGood = ConfGood <$> (Env.get "PATH" <|> Env.get "path") 
                    <*> (Env.get "TERM" <|> Env.get "term")

confBad :: Env.Parser ConfBad
confBad = ConfBad <$> Env.get "SERVER_NAME" <*> Env.get "TIME_ZONE"
