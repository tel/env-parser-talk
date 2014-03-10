{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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
-- Generally, the construction of 'Slot's requires some lens library as
-- that's the principle interface exported by this module. One builds
-- a basic slot using either @OverloadedStrings@ or, equivalently but more
-- flexibly, 'slot'. After that, you can modify the documentation or
-- default values using the lenses 'key', 'doc', and 'def'.
--
-- > "TIME_ZONE" & Env.doc .~ Just "The default time zone"
-- >             & Env.def .~ Just "America/New_York"
--
-- Normally, 'def'ault values must be 'Show'able so that we can later
-- analyze the kind of 'Slot' we're dealing with. If that isn't the case,
-- then the 'def\'' lens can be used which allows the user to set an
-- explicit 'String' representation of the 'def'ault value.
--
-- > "SOME_DICT" & Env.def' .~ Just ("(default)", defaultDict)
--
-- This module also exports the \"type-forgotten\" interface, 'ASlot'.
-- These are still slots in that you can view their 'key', 'doc', or 'def',
-- but they forbid access to the actual default value, hiding it with 'X'.
-- This makes it easy to handle a homogenous list of 'ASlot's which would
-- be impossible with raw 'Slot's.

module System.Environment.Parser.Slot (

  Slot, slot, slot', ASlot (ASlot)

  , def
  , IsSlot (..)
  , IsSimpleSlot (..)
  , shownDef

  , getKey
  , getDoc
  , getDef
  , getDef'

  , X

) where

import           Control.Applicative
import           Control.Arrow
import qualified Data.ByteString     as S
import           Data.String
import qualified Data.Text           as T
import           Unsafe.Coerce

class IsSimpleSlot s where
  -- | Lens focusing on the 'key' representing a slot, the name of the
  -- environment variable.
  key :: Functor f => (S.ByteString -> f S.ByteString) -> s -> f s
  -- | Lens focusing on the 'doc'umentation for a particular slot.
  doc :: Functor f => (Maybe T.Text -> f (Maybe T.Text)) -> s -> f s

class (IsSimpleSlot s, IsSimpleSlot t)
      => IsSlot s t a b | s   -> a
                        , t   -> b
                        , s b -> t
                        , t a -> s where
  -- | Lens focusing on the 'def'ault value a particular slot.
  def' :: Functor f => (Maybe (String, a) -> f (Maybe (String, b)))
                   -> s -> f t

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
data Slot a
  = Slot {-# UNPACK #-} !S.ByteString
                        !(Maybe T.Text)
                        !(Maybe (String, a))
  deriving ( Eq, Ord, Show, Read, Functor )

-- | Builds a 'Slot' from its 'key' directly. This is necessary when it's
-- either desirable or necessary to avoid @OverloadedStrings@.
slot :: S.ByteString -> Slot a
slot s = Slot s Nothing Nothing

-- | Builds a 'Slot' optionally providing documentation and default values
-- completely. This is basically useful only if one would like to avoid the
-- @Lens@ interface entirely; otherwise, using 'key', 'doc', and 'def' is
-- far nicer.
slot' :: Show a => S.ByteString -> Maybe T.Text -> Maybe a -> Slot a
slot' k d a = Slot k d (fix <$> a) where
  fix x = (show x, x)

instance IsSimpleSlot (Slot a) where
  key inj (Slot k d a) = (\x -> Slot x d a) <$> inj k
  {-# INLINE key #-}
  doc inj (Slot k d a) = (\x -> Slot k x a) <$> inj d
  {-# INLINE doc #-}

instance IsSlot (Slot a) (Slot b) a b where
  def' inj (Slot k d a) = (\x -> Slot k d x) <$> inj a
  {-# INLINE def' #-}

def :: forall f s t a b
     .  ( IsSlot s t a b
        , Functor f
        , Show b
        )
     => (Maybe a -> f (Maybe b))
     -> s -> f t
def = def' . le where
  le :: (Maybe a -> f (Maybe b))
     -> Maybe (String, a) -> f (Maybe (String, b))
  le inj p = (\b -> (show &&& id) <$> b) <$> inj (snd <$> p)

shownDef :: IsSlot s s a a => s -> Maybe String
shownDef = fmap fst . getDef'

type Lensy f s a = (a -> f a) -> s -> f s

get :: ((a -> Const a b) -> s -> Const a t) -> s -> a
get l = getConst . l Const
{-# INLINE get #-}

simply :: (Lensy f s a -> r) -> Lensy f s a -> r
simply = id
{-# INLINE simply #-}

-- | A default value
getDef :: Slot a -> Maybe a
getDef = fmap snd . getDef'

getDef' :: IsSlot s s a a => s -> Maybe (String, a)
getDef' = simply get def'

getKey :: IsSlot s s a a => s -> S.ByteString
getKey = get key

getDoc :: IsSlot s s a a => s -> Maybe T.Text
getDoc = get doc

instance IsString (Slot a) where fromString = slot . fromString

-- | A 'Slot' which has forgotten what kind of value it takes as 'def'ault.
-- This is useful for summarization of a Parser since when we break the
-- Parser apart into the constituent slots they will not, in general, share
-- types.
data ASlot = forall a . ASlot (Slot a)

instance Show ASlot where
  show (ASlot (Slot k d a)) =
    unwords [ "ASlot"
            , show k
            , show d
            , show (fst <$> a)
            ]

instance IsSimpleSlot ASlot where
  key inj (ASlot (Slot k d a)) = (\x -> ASlot (Slot x d a)) <$> inj k
  {-# INLINE key #-}
  doc inj (ASlot (Slot k d a)) = (\x -> ASlot (Slot k x a)) <$> inj d
  {-# INLINE doc #-}

-- | A constructor which holds a forgotten type. The only point of this
-- type is to ensure that you can't see inside of it. In order to return
-- one, you must be handed it, so you can think of it as a universally
-- quantified type variable.
data X = forall a . X a

instance IsSlot ASlot ASlot X X where
  def' inj (ASlot (Slot k d a)) =
    (\x -> ASlot (Slot k d (annex <$> x))) <$> inj (ex <$> a)
      where
        ex (s, x) = (s, X x)
        annex (s, X x) = (s, unsafeCoerce x)
  {-# INLINE def' #-}

