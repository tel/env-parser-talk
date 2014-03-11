{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

-- |
-- Module      :  System.Environment.Parser.Internal.FreeA
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable (ExistentialQuantification, RankNTypes)
--
-- Free Alternatives over a Functor. This presentation was taken from
--
-- /Free Applicative Functors./ Paolo Capriotti, Ambrus Kaposi.
-- http://arxiv.org/abs/1403.0749
--
-- and then adapted to include the 'Alternative' behavior by using the
-- method in @free@.
--
-- <http://hackage.haskell.org/package/free-4.5/docs/Control-Alternative-Free.html>
--

module System.Environment.Parser.Internal.FreeA where

import           Control.Applicative
import           Data.Monoid

-- | The "free alternative functor over a functor". 'FreeA' as a type
-- constructor can be seen as a functor from the category of Haskell
-- endofunctors to the category of Haskell alternative functors which is
-- left-adjoint to the forgetful functor.
data FreeA f a
  = Pure a
  | Alt [FreeA f a]
  | forall b . f (b -> a) :$: FreeA f b
infixr 4 :$:

instance Functor f => Functor (FreeA f) where
  fmap g (Pure x)  = Pure (g x)
  fmap g (h :$: x) = fmap (g .) h :$: x
  fmap g (Alt as)  = Alt (map (fmap g) as)

instance Functor f => Applicative (FreeA f) where
  pure = Pure
  Pure g    <*> y = fmap g y
  (h :$: x) <*> y = fmap uncurry h :$: ((,) <$> x <*> y)
  -- "left distribution" law that matches the definition of the
  -- seminearring in Collect.hs
  Alt as    <*> y = Alt (map (<*> y) as) 

instance Functor f => Alternative (FreeA f) where
  empty = Alt []
  -- mempty laws
  Alt [] <|> r      = r
  l      <|> Alt [] = l
  Alt as <|> Alt bs = Alt (as <> bs)
  l      <|> r      = Alt [l, r]

instance Functor f => Monoid (FreeA f a) where
  mempty  = empty
  mappend = (<|>)
  mconcat as = fromList (as >>= toList)
    where
      toList (Alt xs) = xs
      toList x       = [x]
      fromList [x] = x
      fromList xs  = Alt xs

-- | Embed a signature functor into the free alternative over that
-- signature.
one :: Functor f => f a -> FreeA f a
one f = fmap const f :$: Pure ()

-- | Lift a functor morphism over the signature functor of a free
-- alternative
liftT :: (Functor f, Functor g) => (forall x . f x -> g x) -> FreeA f a -> FreeA g a
liftT _ (Pure x ) = Pure x
liftT k (h :$: x) = k h :$: liftT k x
liftT k (Alt as)  = Alt (map (liftT k) as)

-- | Given a step-by-step morphism, @k@, from the signature functor to a target
-- 'Alternative', @raise k@ is the unique morphism which interprets the
-- free alternative as that target.
--
-- 'raise' also forms witness to the adjunction of 'FreeA'.
raise :: (Functor f, Alternative g) => (forall x . f x -> g x) -> FreeA f a -> g a
raise _ (Pure x ) = pure x
raise k (g :$: x) = k g <*> raise k x
raise k (Alt as ) = foldr (\a b -> raise k a <|> b) empty as
