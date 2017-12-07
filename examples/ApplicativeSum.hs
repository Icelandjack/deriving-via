{-# LANGUAGE TypeOperators, TypeInType, GADTs, MultiParamTypeClasses,
             AllowAmbiguousTypes, TypeFamilies, ScopedTypeVariables,
             RebindableSyntax, ConstraintKinds,
             FlexibleContexts, LambdaCase, EmptyCase, FlexibleInstances, 

             TemplateHaskell, TypeApplications, UndecidableInstances, 
             InstanceSigs, DerivingStrategies, RankNTypes, DeriveFunctor, 
             PatternSynonyms, GeneralizedNewtypeDeriving #-}

module ApplicativeSum where

-- import Data.Singletons
-- import Data.Singletons.TH
import Data.Functor.Identity
import Control.Monad.Reader
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Compose
import Control.Applicative
import Data.Kind
import Data.Monoid hiding (Sum(..), Product(..))
import Prelude
import Linear (V1(..), V3(..))

-- sneezy.cs.nott.ac.uk/darcs/term/Idiomatics.lhs (dead link?)
--
-- "There's a fairly predictable notion of homomorphism between
-- Applicative functors: just pick any natural transformation which
-- respects pure and <*>. There are far too many of these
-- homomorphisms to leave them entirely automatic, so let's try having
-- type-level proxies for them, aka named instances."

-- It is hard to give a context to =Applicative (Sum f g)=.
--
-- "We can construct sums in special cases, such as adding the
-- identity functor to another lax monoidal functor", 
-- 
--   type Lift = Sum Identity
-- 
-- also known as 'Lift' from 'Control.Applicative.Lift':
--
--   data Lift f a = Return a | Other (f a)
-- 
-- http://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Applicative-Lift.html#Lift

type f ~> g = forall xx. f xx -> g xx

-- It's not obvious, here is one encoding:

data TagFunctors :: (Type -> Type) -> (Type -> Type) -> Type

-- Same trick as _Singletons_ use, for typed tags that are extensible
type f .~> g = TagFunctors f g -> Type

data IdHom    :: f           .~> f
data InitHom  :: Identity    .~> f
data MonHom   :: f           .~> Const m
data Fst      :: Product f g .~> f
data Snd      :: Product f g .~> g
data Head     :: V3          .~> V1
data Compose1 :: g           .~> Compose f g
data Compose2 :: f           .~> Compose f g
data Prod1    :: (f .~> g) -> (f       .~> Product f g)
data Prod2    :: (g .~> f) -> (g       .~> Product f g)
data Sum1     :: (g .~> f) -> (SUM (tag::f .~> g) .~> f)

data Bimap :: (f .~> f') -> (g .~> g') -> (Product f g .~> Product f' g')
data Comp  :: (a .~> b) -> (b .~> c) -> (a .~> c)

-- Reflect applicative morphism from tag
class (Applicative f, Applicative g) => AppHom (tag::f .~> g) where
  appHom :: f ~> g

instance Applicative f => AppHom (InitHom::Identity .~> f) where
  appHom :: Identity ~> f
  appHom (Identity a) = pure a

instance Applicative f => AppHom (IdHom::f .~> f) where
  appHom :: f ~> f
  appHom = id

instance (Applicative f, Monoid m) => AppHom (MonHom::f .~> Const m) where
  appHom :: f ~> Const m
  appHom _ = Const mempty

instance (Applicative f, Applicative g) => AppHom (Fst::Product f g .~> f) where
  appHom :: Product f g ~> f
  appHom (Pair fa _) = fa

instance (Applicative f, Applicative g) => AppHom (Snd::Product f g .~> g) where
  appHom :: Product f g ~> g
  appHom (Pair _ ga) = ga

instance (AppHom f, AppHom g) 
  => 
  AppHom (Comp (f::a .~> b) (g::b .~> c)::a .~> c) where
  appHom :: a ~> c
  appHom = appHom @_ @_ @g . appHom @_ @_ @f

instance (AppHom one, AppHom two) 
  => 
  AppHom (Bimap (one::f .~> f') (two::g .~> g')::Product f g .~> Product f' g') where
  appHom :: Product f g ~> Product f' g'
  appHom (Pair fa ga) = Pair
    (appHom @_ @_ @one fa)
    (appHom @_ @_ @two ga)

instance (Applicative f, Applicative g) => AppHom (Compose1::g .~> Compose f g) where
  appHom :: g ~> Compose f g
  appHom = Compose . pure

instance (Applicative f, Applicative g) => AppHom (Compose2::f .~> Compose f g) where
  appHom :: f ~> Compose f g
  appHom = Compose . fmap pure

instance AppHom nat => AppHom (Prod1 nat::f .~> Product f g) where
  appHom :: f ~> Product f g
  appHom x = Pair x (appHom @_ @_ @nat x)

instance AppHom nat => AppHom (Prod2 nat::g .~> Product f g) where
  appHom :: g ~> Product f g
  appHom x = Pair (appHom @_ @_ @nat x) x

instance (AppHom nat, AppHom tag) => AppHom (Sum1 nat::SUM (tag::f .~> g) .~> f) where
  appHom :: SUM tag ~> f
  appHom = \case
    L fa -> appHom @_ @_ @nat fa
    R ga -> ga

instance AppHom (Head::V3 .~> V1) where
  appHom :: V3 ~> V1
  appHom (V3 a _ _) = V1 a

-- Let's define 
newtype SUM (tag::g .~> f) a = SUM (Sum f g a)
  deriving stock Functor

{-# Complete L, R #-}
pattern L :: f a -> SUM (tag::g .~> f) a
pattern L a = SUM (InL a)

pattern R :: g a -> SUM (tag::g .~> f) a
pattern R a = SUM (InR a)

instance (AppHom tag) => Applicative (SUM (tag::f .~> g)) where
  pure = R . pure

  R f <*> R s = R (f <*> s)
  R f <*> L s = L (appHom @_ @_ @tag f <*> s)
  L f <*> R s = L (f <*> appHom @_ @_ @tag s)
  L f <*> L s = L (f <*> s)

-- Control.Applicative.Lift.Lift
newtype Lift f a = Lift (Sum f Identity a)
  deriving newtype Functor

  -- TODO
  -- 
  -- deriving Applicative via
  --   (SUM (InitHom :: Identity .~> f))

{-# Complete Return, Other #-}
pattern Return :: a -> Lift f a
pattern Return a = Lift (InR (Identity a))

pattern Other :: f a -> Lift f a
pattern Other fa = Lift (InL fa)

-- >>> liftA2 (+) (TheV3 10 20 30) (TheV1 1)
-- TheV1 676
newtype HeadOfV3 a = HeadOfV3 (Sum V1 V3 a)
  deriving newtype (Show, Functor)

  deriving Applicative via
    (SUM (Head::V3 .~> V1))

{-# Complete TheV1, TheV3 #-}
pattern TheV1 :: a -> HeadOfV3 a
pattern TheV1 a = HeadOfV3 (InL (V1 a))

pattern TheV3 :: a -> a -> a -> HeadOfV3 a
pattern TheV3 a b c = HeadOfV3 (InR (V3 a b c))

type Formlet mo err env a = Product (Const mo) (Compose (Reader env) (Sum (Const err) Identity)) a
