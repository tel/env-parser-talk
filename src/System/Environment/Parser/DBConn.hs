
-- |
-- Module      :  System.Environment.Parser.DBConn
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Database connection URL parsing. The format is designed to be compatible
-- with the Heroku dabatase APIs as much as possible.
--
-- /Database connections/
--
-- The primary type for parsing database connections is the 'DBConn' type
-- which represents all of the information usually parsed from
-- a Heroku-style database URI. Any other database connection type can
-- instantiate 'IsDbConnection' by providing a way to instantiate a value
-- using the information in 'DBConn'. After that, these can be directly
-- requested from an 'Env.Parser' by using 'dbconn'.

module System.Environment.Parser.DBConn (

  Provider (..), providerString,
  IsDbConnection (..), dbconn

) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text               as At
import qualified Data.ByteString                    as S
import qualified Data.ByteString.Char8              as S8
import qualified Data.Map                           as Map
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import qualified Network.HTTP.Types                 as Ht
import qualified Network.URI                        as URI
import           System.Environment.Parser.FromEnv
import qualified System.Environment.Parser.Internal as Env

----------------------------------------------------------------------------
-- Heroku-style DB urls

-- | Provider indicates what kind of database provider scheme this URL
-- refers to. This provides a small amount of convenience around URL
-- detection, but if detection fails use 'providerName' to extract the raw
-- string.
data Provider = Postgres S.ByteString
              | MySQL    S.ByteString
              | AMQP     S.ByteString
              | HTTP     S.ByteString
              | Other    S.ByteString
  deriving ( Eq, Ord, Show, Read )

providerString :: Provider -> S.ByteString
providerString p = case p of
  Postgres s -> s
  MySQL    s -> s
  AMQP     s -> s
  HTTP     s -> s
  Other    s -> s

-- | Types which can be instantiated from the information in a 'DBConn'
-- type can instantiate 'IsDbConnection'. This allows them to be parsed
-- directly from the environment using 'dbconn'.
class IsDbConnection a where
  fromDBConn :: DBConn -> Either String a

-- | Look up a key containing a JSON value in the ENV. This operation may
-- fail if the key is missing or if the value cannot be parsed according to
-- its 'IsDbConnection' instance.
dbconn :: IsDbConnection a => S.ByteString -> Env.Parser a
dbconn = Env.get' (fromEnv >=> fromDBConn)

-- | A type representing the information that can be parsed from
-- a URL-style database configuration. For example consider the following
-- URLs
--
-- > postgres://user3123:passkja83kd8@ec2-117-21-174-214.compute-1.amazonaws.com:6212/db982398
-- > mysql://adffdadf2341:adf4234@us-cdbr-east.cleardb.com/heroku_db?reconnect=true
-- > mysql2://adffdadf2341:adf4234@us-cdbr-east.cleardb.com/heroku_db?reconnect=true
-- > http://user:pass@instance.ip/resourceid
-- > amqp://user:pass@ec2.clustername.cloudamqp.com/vhost
--
-- We assume that all the information in these URLs is provided---if
-- a username or password is unnecessary then it must be passed as an empty
-- string and not omitted.
--
data DBConn = DBConn
  { provider :: Provider
  , host     :: S.ByteString
  , username :: S.ByteString
  , password :: S.ByteString
  , port     :: Int
  , location :: S.ByteString
  , params   :: Map.Map S.ByteString S.ByteString
  }
  deriving ( Eq, Ord, Show, Read )

instance FromEnv DBConn where
  fromEnv = tryParse <=< fromEnv

tryParse :: String -> Either String DBConn
tryParse s = do
  uri  <- e "invalid URI format" $ URI.parseAbsoluteURI s
  auth <- e "URI authority segment missing" $ URI.uriAuthority uri
  prt <- parsePort (URI.uriPort auth)
  (usernm, passwd) <- parseUserPw (URI.uriUserInfo auth)

  return DBConn
    { provider = guessProvider (URI.uriScheme uri)
    , host     = S8.pack (URI.uriRegName auth)
    , username = usernm
    , password = passwd
    , port     = prt
    , location = S8.pack $ drop 1 $ URI.uriPath uri
    , params   = makeQueryMap (URI.uriQuery uri)
    }

  where

    guessProvider :: String -> Provider
    guessProvider x = case x of
      "postgres:" -> Postgres bs
      "mysql:"    -> MySQL    bs
      "mysql2:"   -> MySQL    bs
      "http:"     -> HTTP     bs
      "amqp:"     -> AMQP     bs
      _           -> Other    bs
      where bs = S8.pack s

    parsePort :: String -> Either String Int
    parsePort = At.parseOnly (At.char ':' *> At.decimal) . T.pack

    parseUserPw :: String -> Either String (S.ByteString, S.ByteString)
    parseUserPw = At.parseOnly ( (,) <$> (TE.encodeUtf8 <$> At.takeTill (==':') <* At.char ':')
                                     <*> (TE.encodeUtf8 <$> At.takeTill (=='@') <* At.char '@') )
                . T.pack

    makeQueryMap :: String -> Map.Map S.ByteString S.ByteString
    makeQueryMap = Map.fromList . Ht.parseSimpleQuery . S8.pack

-- -----------------------------------------------------------------------------
-- Utilities

e :: String -> Maybe a -> Either String a
e s Nothing  = Left s
e _ (Just a) = Right a
