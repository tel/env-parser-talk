{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  System.Environment.Parser.Internal.Collect
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The \"purely applicative 'Either'\", 'Collect', automatically 'mappend's
-- its summarization values.

module System.Environment.Parser.Internal.Collect where

import           Control.Applicative
import           Data.Monoid
import qualified Data.Foldable       as F
import           Data.Sequence       (Seq, (<|), (|>))
import qualified Data.Sequence       as Seq

data K b a = K { uK :: b } deriving Functor

instance Monoid b => Applicative (K b) where
  pure _ = K mempty
  K b1 <*> K b2 = K (b1 <> b2)

-- | Seminearring: a (not necessarily abelian) monoid glued to another
-- monoid such that distributivity occurs on just one side:
--
-- > (x+y) z = xz + yz
--
-- but not
--
-- > x (y+z) = xy + xz
--
-- For this definition, it's assumed that the subclassed 'Monoid' is the
-- multiplicative (distributing) element so we have
--
-- > (x <+> y) <> z = x <> z <+> y <> z
--
-- but not
--
-- > x <> ( y <+> z ) = x <> y <+> x <> z
infixl 5 <+>
class Monoid b => Seminearring b where
  sempty  :: b
  (<+>) :: b -> b -> b

instance Seminearring b => Alternative (K b) where
  empty = K sempty
  K a <|> K b = K (a <+> b)

data FreeSNR a
  = The a
  | And (Seq (FreeSNR a))
  | Or  (Seq (FreeSNR a))
  deriving (Eq, Ord, Show)

instance Monoid (FreeSNR a) where
  mempty                    = And mempty

  mappend (And as) (And bs) = And (as <> bs)
  mappend (And as) z
    | Seq.null as           = z
    | otherwise             = And (as |> z)
  mappend (Or as) z
    | Seq.null as           = Or as
    | otherwise             = Or (fmap (<> z) as)
  mappend z (And as)
    | Seq.null as           = z
    | otherwise             = And (z <| as)
  mappend z (Or as)
    | Seq.null as           = Or as
    | otherwise             = And (Seq.fromList [z, Or as])
  mappend a b               = And (Seq.fromList [a, b])

instance Seminearring (FreeSNR a) where
  sempty      = Or mempty

  Or  as <+> Or bs     = Or (as <> bs)
  Or  as <+> z
    | Seq.null as      = z
    | otherwise        = Or (as |> z)
  z      <+> Or bs
    | Seq.null bs      = z
    | otherwise        = Or (z <| bs)
  a      <+> b         = Or (Seq.fromList [a, b])

first :: FreeSNR a -> Maybe [a]
first (The a)  = Just [a]
first (And as) = F.foldMap first as
first (Or as) = case Seq.viewl as of
  Seq.EmptyL -> Nothing
  a Seq.:< _ -> first a

-- The \"purely applicative 'Either'\", 'Collect', automatically 'mappend's
-- its summarization values.
newtype Collect e a
  = Collect { collect :: Either e a }
  deriving ( Functor )

instance Monoid e => Applicative (Collect e) where
  pure a = Collect (Right a)
  Collect (Left e1) <*> Collect (Left e2) = Collect (Left (e1 <> e2))
  Collect (Right f) <*> Collect (Right x) = Collect (Right (f x))
  Collect (Left e ) <*> _                 = Collect (Left e)
  _                 <*> Collect (Left e ) = Collect (Left e)

-- | Left-biased alternation
instance Monoid e => Alternative (Collect e) where
  empty = Collect (Left mempty)
  Collect (Left _)  <|>         r        = r
  l                 <|> Collect (Left _) = l
  Collect (Right x) <|> _                = Collect (Right x)

miss :: e -> Collect e a
miss = Collect . Left

have :: a -> Collect e a
have = Collect . Right

-- | Uses 'maybe' to convert a 'Maybe' to a 'Collect'.
collMay :: e -> (a -> b) -> Maybe a -> Collect e b
collMay e f = maybe (miss e) (have . f)

-- | Right-to-left composition of functors. The composition of applicative
-- functors is always applicative, but the composition of monads is not
-- always a monad.
newtype (f :.: g) a
  = Compose { getCompose :: f (g a) }
  deriving Functor

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

-- | Pushes the alternation into the innermost layer
instance (Applicative f, Alternative g) => Alternative (f :.: g) where
  empty                   = Compose (pure empty)
  Compose l <|> Compose r = Compose (liftA2 (<|>) l r)
