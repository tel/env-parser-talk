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

  -- * Slot contruction
  Slot, slot, slot'

  -- * Slot examination
  , pp

  -- ** Lensy construction kit
  , key
  , doc
  , def
  , def'

  -- ** Simplified getter interface
  , getKey
  , getDoc
  , getDef
  , shownDef
  , getDef'

  -- * Anonymous slots
  , ASlot, aSlot
  , X

) where

import           Control.Applicative
import           Control.Arrow
import qualified Data.ByteString         as S
import qualified Data.ByteString.Char8   as S8
import           Data.String
import qualified Data.Text               as T
import qualified Text.PrettyPrint.Leijen as Pp

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
  deriving ( Eq, Ord, Read, Functor )

instance Show (Slot a) where
  showsPrec i (Slot k d a) = showsParen (i > 10) $
    saySlot . sp . showsPrec (i + 11) k
            . sp . showsPrec (i + 11) d
            . sp . showsPrec (i + 11) (fst <$> a)

    where
      saySlot = ("Slot" ++)
      sp      = (" " ++)
      par     = ("(" ++)
      rap     = (")" ++)
      showsParen True  f = par . f . rap
      showsParen False f = f

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

-- | Pretty-prints a 'Slot' for demonstration purposes.
pp :: Slot a -> Pp.Doc
pp s = Pp.text (S8.unpack $ get key s) 
       Pp.<$>
       Pp.indent 4 (docs Pp.</> defs)

  where
    docs = case get doc s of
      Nothing -> Pp.empty
      Just d  -> 
        Pp.fillSep $ map (Pp.text . T.unpack) (T.words d)
    defs = case get def' s of
      Nothing       -> Pp.empty
      Just (str, _) -> 
        Pp.text "(default:"
        Pp.<+> 
        Pp.text str
        Pp.<+> 
        Pp.text ")"

-- | Lens focusing on the 'key' representing a slot, the name of the
-- environment variable.
key :: Functor f => (S.ByteString -> f S.ByteString)
                 -> Slot a -> f (Slot a)
key inj (Slot k d a) = (\x -> Slot x d a) <$> inj k
{-# INLINE key #-}

-- | Lens focusing on the 'doc'umentation for a particular slot.
doc :: Functor f => (Maybe T.Text -> f (Maybe T.Text))
                 -> Slot a -> f (Slot a)
doc inj (Slot k d a) = (\x -> Slot k x a) <$> inj d
{-# INLINE doc #-}

-- | Lens focusing on the 'def'ault value a particular slot, but also
-- allowing explicit choice of the value\'s 'String' representation.
def' :: Functor f => (Maybe (String, a) -> f (Maybe (String, b)))
                  -> Slot a -> f (Slot b)
def' inj (Slot k d a) = Slot k d <$> inj a
{-# INLINE def' #-}

-- | Lens focusing on the 'def'ault value a particular slot.
def :: ( Functor f, Show b ) => (Maybe a -> f (Maybe b))
                             -> Slot a -> f (Slot b)
def inj (Slot k d a) =
  (\x -> Slot k d ((show &&& id) <$> x)) <$> inj (snd <$> a)
{-# INLINE def #-}

shownDef :: Slot a -> Maybe String
shownDef = fmap fst . getDef'

type Lensy f s a = (a -> f a) -> s -> f s

get :: ((a -> Const a b) -> s -> Const a t) -> s -> a
get l = getConst . l Const
{-# INLINE get #-}

simply :: (Lensy f s a -> r) -> Lensy f s a -> r
simply = id
{-# INLINE simply #-}

getDef :: Slot a -> Maybe a
getDef = fmap snd . getDef'

getDef' :: Slot a -> Maybe (String, a)
getDef' = simply get def'

getKey :: Slot a -> S.ByteString
getKey = get key

getDoc :: Slot a -> Maybe T.Text
getDoc = get doc

instance IsString (Slot a) where fromString = slot . fromString

-- | A 'Slot' which has forgotten what kind of value it takes as 'def'ault.
-- This is useful for summarization of a Parser since when we break the
-- Parser apart into the constituent slots they will not, in general, share
-- types.
type ASlot = Slot X

aSlot :: Slot a -> ASlot
aSlot = fmap (const X)

-- | A constructor which holds a forgotten type. The only point of this
-- type is to ensure that you can't see inside of it. In order to return
-- one, you must be handed it, so you can think of it as a universally
-- quantified type variable.
data X = X
