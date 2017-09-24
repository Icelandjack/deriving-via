{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}
module DeriveFunctor where

import Data.Bifunctor
import Data.Coerce

newtype Flip p a b = Flip { runFlip :: p b a }

instance Bifunctor p => Bifunctor (Flip p) where
  bimap f g = Flip . bimap g f . runFlip

instance Bifunctor p => Functor (Flip p a) where
  fmap f = Flip . first f . runFlip

-----

newtype Foo a = Foo (Either a Int)

instance Functor Foo where
  fmap = coerce @(forall a b. (a -> b) -> Flip Either Int a -> Flip Either Int b)
                @(forall a b. (a -> b) -> Foo a             -> Foo b)
                fmap
