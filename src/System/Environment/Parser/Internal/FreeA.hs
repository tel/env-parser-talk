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
-- Free Applicatives over a Functor. This presentation was taken from
--
-- /Free Applicative Functors./ Paolo Capriotti, Ambrus Kaposi.
-- http://arxiv.org/abs/1403.0749
--

module System.Environment.Parser.Internal.FreeA where

import           Control.Applicative

-- | The "free applicative functor over a functor". 'FreeA' as a type
-- constructor can be seen as a functor from the category of Haskell
-- endofunctors to the category of Haskell applicative functors which is
-- left-adjoint to the forgetful functor.
data FreeA f a
  = Pure a
  | forall b . f (b -> a) :$: FreeA f b
infixr 4 :$:

instance Functor f => Functor (FreeA f) where
  fmap g (Pure x)  = Pure (g x)
  fmap g (h :$: x) = fmap (g .) h :$: x

instance Functor f => Applicative (FreeA f) where
  pure = Pure
  Pure g    <*> y = fmap g y
  (h :$: x) <*> y = fmap uncurry h :$: ((,) <$> x <*> y)

-- | Embed a signature functor into the free applicative over that
-- signature.
one :: Functor f => f a -> FreeA f a
one f = fmap const f :$: Pure ()

-- | Lift a functor morphism over the signature functor of a free
-- applicative.
liftT :: (Functor f, Functor g) => (forall x . f x -> g x) -> FreeA f a -> FreeA g a
liftT _ (Pure x ) = Pure x
liftT k (h :$: x) = k h :$: liftT k x

-- | Given a step-by-step morphism, @k@, from the signature functor to a target
-- 'Applicative', @raise k@ is the unique morphism which interprets the
-- free applicative as that target.
--
-- 'raise' also forms witness to the adjunction of 'FreeA'.
raise :: (Functor f, Applicative g) => (forall x . f x -> g x) -> FreeA f a -> g a
raise _ (Pure x ) = pure x
raise k (g :$: x) = k g <*> raise k x
