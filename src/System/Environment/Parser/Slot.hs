{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      :  System.Environment.Parser.Slot
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 'Slot's represent variables in the environment and store information
-- about accessing that environment variable. At their simplest, 'Slot's
-- are nothing more than 'S.ByteString's representing the name of the
-- environment variable (in fact, 'Slot' instantiates 'IsString' so that
-- you can use them exactly like that if you use @OverloadedStrings@) but
-- they may also include either documentation or default values.
--
-- This module also exports a few lenses which allow for easy construction
-- of 'Slot's if you're a lens user.
--
-- > "TIME_ZONE" & Env.doc .~ "The default time zone"
-- >             & Env.def .~ "America/New_York"

module System.Environment.Parser.Slot where

import           Control.Applicative
import qualified Data.ByteString     as S
import           Data.String
import qualified Data.Text           as T

-- | A 'Slot' describes a name and value expected to be in the environment.
-- 'Slot' instantiates 'IsString' so that, at its simplest, it's possible
-- to completely ignore the existence of 'Slot's.
--
-- > "TIME_ZONE" :: Slot a
--
-- They also come packaged with a set of lenses so that they can be easily
-- constructed and modified.
--
-- > "TIME_ZONE" & Env.doc .~ "The default time zone"
-- >             & Env.def .~ "America/New_York"
data Slot a = Slot 
  { getKey :: {-# UNPACK #-} !S.ByteString   -- ^ The environment variable name
  , getDoc ::                !(Maybe T.Text) -- ^ Description of the value
  , getDef ::                !(Maybe a)      -- ^ A default value
  }
  deriving ( Eq, Ord, Show, Read, Functor )

-- | Builds a 'Slot' from its 'key' directly. This is necessary when it's
-- either desirable or necessary to avoid @OverloadedStrings@.
slot :: S.ByteString -> Slot a
slot s = Slot s Nothing Nothing

-- | Lens focusing on the 'key' representing a 'Slot', the name of the
-- environment variable.
key :: Functor f => (S.ByteString -> f S.ByteString) -> Slot a -> f (Slot a)
key inj (Slot k d a) = (\x -> Slot x d a) <$> inj k

-- | Lens focusing on the 'doc'umentation for a particular 'Slot'.
doc :: Functor f => (Maybe T.Text -> f (Maybe T.Text)) -> Slot a -> f (Slot a)
doc inj (Slot k d a) = (\x -> Slot k x a) <$> inj d

-- | Lens focusing on the 'def'ault value a particular 'Slot'.
def :: Functor f => (Maybe a -> f (Maybe b)) -> Slot a -> f (Slot b)
def inj (Slot k d a) = Slot k d <$> inj a

instance IsString (Slot a) where fromString = slot . fromString
