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

import Control.Applicative
import Data.Monoid

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
