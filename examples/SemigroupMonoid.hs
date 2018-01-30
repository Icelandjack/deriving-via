{-# Language GeneralizedNewtypeDeriving, DerivingStrategies, DeriveFunctor, InstanceSigs, StandaloneDeriving, TypeApplications, ScopedTypeVariables, TypeOperators, FlexibleInstances, DerivingVia #-}

module SemigroupMonoid where

import Control.Applicative
import Data.Semigroup ((<>))
import Data.Functor.Compose

----------------------------------------------------------------------
-- Applicative Lift Adapter
----------------------------------------------------------------------

newtype f $ a = App (f a)
  deriving newtype
    (Functor, Applicative, Show)

instance (Applicative f, Semigroup a) => Semigroup (f $ a) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (f $ a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

instance (Applicative f, Num a) => Num (f $ a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = fmap negate
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance (Applicative f, Fractional a) => Fractional (f $ a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Applicative f, Floating a) => Floating (f $ a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

-- Works for many types of
--
-- A Prey But Not Greedy Printer (Functional Pearl)
--
-- JEAN-PHILIPPE BERNARDY
-- http://jyp.github.io/pdf/Prettiest.pdf

----------------------------------------------------------------------
-- Deriving
----------------------------------------------------------------------

newtype Foo f = Foo { run :: f () }
  deriving (Semigroup, Monoid)
    via (f $ ())

newtype Bar a b = Bar [(a, b)]
  deriving (Semigroup, Monoid, Num, Fractional, Floating, Show)
    via (([] `Compose` (,) a) $ b)

