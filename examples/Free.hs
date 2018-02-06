{-# Language ConstraintKinds, RankNTypes, ScopedTypeVariables, GADTs, TypeFamilies, AllowAmbiguousTypes, DeriveFunctor, InstanceSigs, FlexibleInstances, DerivingVia #-}

module Free where

import Control.Applicative

newtype Free cls a = Free (forall xx. cls xx => (a -> xx) -> xx)

instance Semigroup (Free Semigroup a) where
  (<>) :: Free Semigroup a -> Free Semigroup a -> Free Semigroup a
  Free free <> Free free' = Free (liftA2 (<>) free free')

instance Semigroup (Free Monoid a) where
  (<>) :: Free Monoid a -> Free Monoid a -> Free Monoid a
  Free free <> Free free' = Free (liftA2 (<>) free free')

instance Monoid (Free Monoid a) where
  mempty :: Free Monoid a
  mempty = Free (pure mempty)

instance Foldable (Free Monoid) where
  foldMap :: Monoid m => (a -> m) -> Free Monoid a -> m
  foldMap f (Free free) = free f

newtype List a = List (forall x. Monoid x => (a -> x) -> x)
  deriving (Semigroup, Monoid)
    via (Free Monoid a)
  deriving (Foldable)
    via (Free Monoid)
