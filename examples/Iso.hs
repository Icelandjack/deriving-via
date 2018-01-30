{-# Language ConstraintKinds, TypeInType, RankNTypes, TypeOperators, TypeApplications, ScopedTypeVariables, GADTs, TypeFamilies, AllowAmbiguousTypes, DeriveFunctor, InstanceSigs, ViewPatterns, DerivingStrategies, DeriveGeneric, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, StandaloneDeriving, DeriveAnyClass, DerivingVia #-}

module Iso where

-- This can be used to determine the associated type family of a
-- class:
-- https://www.reddit.com/r/haskell/comments/7jx8wc/blog_witnessing_monoid_actions_semigroup_monoid/drjys8d/

import Data.Profunctor
import Data.Kind
import Data.Functor.Contravariant
import Data.Bifunctor
import Data.Coerce
import Data.Functor.Rep
import Data.Functor.Product
import Data.Functor.Identity
import Data.Distributive
import qualified Data.Constraint as C
import qualified Data.Constraint.Forall as Forall
import qualified GHC.Generics as Generics
import qualified Generics.SOP as SOP

type f ~> g = forall xx. f xx -> g xx

type a <-> b = (a, b) -> Type

data ID         :: a<->a
data (·)        :: a<->b  -> b<->c -> a<->c
data BIMAP bif  :: a<->a' -> b<->b' -> bif a b<->bif a' b'
data COERCE a b :: a<->b
data CONTRA f   :: a<->a' -> f a<->f a'
data DIMAP f    :: a<->a' -> b<->b' -> f a b<->f a' b'
data CURR       :: ((a, b) -> c)<->(a -> b -> c)
data FLIP       :: (a -> b -> c)<->(b -> a -> c)
data ENUM a     :: Int<->a
data FROM       :: a<->b -> b<->a
data MAPPING f  :: a<->a' -> f a<->f a'
data SOP        :: a<->code

class Isomorphism (tag::a<->b) where
  to   :: a -> b
  from :: b -> a

-- id :: Iso' a a
instance Isomorphism ID where
  to   = id
  from = id

-- (.) :: Iso' a b -> Iso' b c -> Iso' a c
instance (Isomorphism f, Isomorphism g) => Isomorphism (f · g) where
  to   = to   @_ @_ @g . to   @_ @_ @f
  from = from @_ @_ @f . from @_ @_ @g

-- bimapping :: Bifunctor bif => Iso' a a' -> Iso' b b' -> Iso' (bif a b) (bif a' b')
instance (Isomorphism f, Isomorphism g, Bifunctor bif) => Isomorphism (BIMAP bif f g) where
  to   = bimap (to   @_ @_ @f) (to   @_ @_ @g)
  from = bimap (from @_ @_ @f) (from @_ @_ @g)

-- coerced :: Coercible a b => Iso' a b
instance Coercible a b => Isomorphism (COERCE a b) where
  to   = coerce
  from = coerce

-- contramapping :: Contravariant f => Iso' a a' -> Iso' (f a) (f a')
instance (Contravariant f, Isomorphism a) => Isomorphism (CONTRA f a) where
  to   = contramap (from @_ @_ @a)
  from = contramap (to   @_ @_ @a)

-- curried :: Iso' ((a, b) -> c) (a -> b -> c)
instance Isomorphism CURR where
  to   = curry
  from = uncurry

-- dimapping :: (Profunctor p, Profunctor q) => Iso' a a' -> Iso' b b' -> Iso' (p s b) (p a' b')
instance (Profunctor f, Isomorphism a, Isomorphism b) => Isomorphism (DIMAP f a b) where
  to   = dimap (from @_ @_ @a) (to   @_ @_ @b)
  from = dimap (to   @_ @_ @a) (from @_ @_ @b)

-- enum :: Enum a => Iso' Int a
instance Enum a => Isomorphism (ENUM a) where
  to   = toEnum
  from = fromEnum

-- flipped :: Iso' (a -> b -> c) (b -> a -> c)
instance Isomorphism FLIP where
  to   = flip
  from = flip

-- from :: Iso' a a' -> Iso' a a'
instance Isomorphism iso => Isomorphism (FROM iso) where
  to   = from @_ @_ @iso
  from = to   @_ @_ @iso

-- mapping :: Functor f => Iso' a a' -> Iso' (f a) (f a')
instance (Functor f, Isomorphism a) => Isomorphism (MAPPING f a) where
  to   = fmap (to   @_ @_ @a)
  from = fmap (from @_ @_ @a)

instance (SOP.Generic a, code ~ SOP.Rep a) => Isomorphism (SOP :: a <-> code) where
  to :: a -> SOP.Rep a
  to = SOP.from

  from :: SOP.Rep a -> a
  from = SOP.to

type ToRepFromRep a a' = ((SOP · SOP) :: a <-> a')

data PairFn :: (a, a)<->(Bool -> a)

instance Isomorphism PairFn where
  from f = (f False, f True)

  to (a, _) False = a
  to (_, b) True  = b

-- | ISOMORPHISM1

data Functors :: (Type -> Type) -> (Type -> Type) -> Type

type a <~> b = Functors a b -> Type

data (··)        :: a<~>b -> b<~>c -> a<~>c
data REP1 f      :: f <~> (->) (Rep f)
data PRODIDID1   :: Product Identity Identity <~> PAIR
data FROM1       :: a<~>b -> b<~>a
data GENERICS1 f :: Generics.Rep1 f<~>f
data COERCE1 f g :: f<~>g

class Isomorphism1 (tag::a<~>b) where
  to1   :: a ~> b
  from1 :: b ~> a

instance (Isomorphism1 f, Isomorphism1 g) => Isomorphism1 (f ·· g) where
  to1   = to1   @_ @_ @g . to1   @_ @_ @f
  from1 = from1 @_ @_ @f . from1 @_ @_ @g

instance Representable f => Isomorphism1 (REP1 f) where
  to1 :: f ~> (->) (Rep f)
  to1 = index

  from1 :: (->) (Rep f) ~> f
  from1 = tabulate

instance Isomorphism1 PRODIDID1 where
  to1 :: Product Identity Identity ~> PAIR
  to1 (Pair (Identity a) (Identity b)) = PAIR (a, b)

  from1 :: PAIR ~> Product Identity Identity
  from1 (PAIR (a, b)) = Pair (Identity a) (Identity b)

instance Isomorphism1 iso1 => Isomorphism1 (FROM1 iso1) where
  to1   = from1 @_ @_ @iso1
  from1 = to1   @_ @_ @iso1

instance Generics.Generic1 g => Isomorphism1 (GENERICS1 g) where
  to1   = Generics.to1
  from1 = Generics.from1

instance CoercibleOver' f g => Isomorphism1 (COERCE1 f g) where
  to1 :: forall xx. f xx -> g xx
  to1 = coerce
    C.\\ Forall.inst @(CoercibleOver f g) @xx

  from1 :: forall xx. g xx -> f xx
  from1 = coerce
    C.\\ Forall.inst @(CoercibleOver f g) @xx

-- type CoercibleOver' f g = (forall xx. f xx `Coercible` g xx)
class    (f a `Coercible` g a, g a `Coercible` f a) => CoercibleOver f g a
instance (f a `Coercible` g a, g a `Coercible` f a) => CoercibleOver f g a

type CoercibleOver' f g = Forall.Forall (CoercibleOver f g)

newtype StolenVia f g :: f <~> g -> Type -> Type where
  Stolen :: f a -> StolenVia f g tag a
  deriving newtype Show

instance (Isomorphism1 iso1, Functor g) => Functor (StolenVia f g iso1) where
  fmap f (Stolen fa) = Stolen (from1 @_ @_ @iso1 (fmap f (to1 @_ @_ @iso1 fa)))

instance (Isomorphism1 iso1, Applicative g) => Applicative (StolenVia f g (iso1::f<~>g)) where
  pure :: a -> StolenVia f g iso1 a
  pure a = Stolen (from1 @_ @_ @iso1 (pure @g a))

  -- (<*>) :: forall a a'. StolenVia f g iso1 (a -> a') -> (StolenVia f g iso1 a -> StolenVia f g iso1 a')
  Stolen (to1 @_ @_ @iso1 -> mf) <*> Stolen (to1 @_ @_ @iso1 -> mx) = Stolen (from1 @_ @_ @iso1 ((<*>) @g mf mx))

-- BASE
newtype PAIR a = PAIR (a, a) deriving stock (Functor, Show)
instance Applicative PAIR where pure a = PAIR (a, a); PAIR (f, g) <*> PAIR (x, y) = PAIR (f x, g y)

newtype Pair a = P (a, a)
  deriving newtype Show
  deriving (Functor, Applicative) via
    (StolenVia Pair PAIR (COERCE1 Pair PAIR))
  -- deriving newtype (Generics.Generic1)

-- Pair -> REP Pair -> REP PAIR -> PAIR -> Product Id Id ->

instance Distributive Pair where
  distribute = distributeRep

instance Representable Pair where
  type Rep Pair = Bool

  index :: Pair ~> (->) Bool
  index (P (a, _)) False = a
  index (P (_, b)) True  = b

  tabulate :: (->) Bool ~> Pair
  tabulate generate = P (generate False, generate True)

par :: Pair Char
par = pure @Pair 'a'

data FOO a = FOO a a a deriving stock (Functor, Generics.Generic1)
data BAR a = BAR a a a deriving stock Generics.Generic1

-- deriving via (StolenVia BAR FOO (FROM1 (GENERICS1 BAR) ·· COERCE1 (Generics.Rep1 BAR) (Generics.Rep1 FOO) ·· GENERICS1 FOO)) instance Functor BAR

data BOOL = FALSE | TRUE
  deriving stock Generics.Generic
  deriving anyclass SOP.Generic
  deriving Eq
