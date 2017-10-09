{-# Language DerivingStrategies, RankNTypes, FlexibleContexts, UndecidableInstances, KindSignatures, PolyKinds, PatternSynonyms, TypeFamilies, TypeOperators, ScopedTypeVariables, GADTs, MultiParamTypeClasses, ViewPatterns, ConstraintKinds, EmptyCase, InstanceSigs, FlexibleInstances, TypeApplications, DeriveGeneric, AllowAmbiguousTypes, DeriveFunctor #-}

import GHC.Generics
import Data.Kind
import Data.Coerce
import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Data.Type.Coercion
import qualified Data.Semigroup as S
import Control.Applicative

import Constraint
import Forall

-- -- import Data.Constraint
-- data Dict c where Dict :: c => Dict c
-- newtype a :- b = Sub (a => Dict b)
-- (\\) :: a => (b => r) -> (a :- b) -> r
-- r \\ Sub Dict = r
-- instance Category (:-) where
--   id = Sub Dict 
--   f . g = Sub $ Dict \\ f \\ g

-- -- import Data.Constraint.Forall
-- class Forall (p :: k -> Constraint) 
-- inst :: Forall p :- p a
-- inst = undefined 

----------------------------------------------------------------------
-- With {-# Language QuantifiedConstraints #-}
-- 
--   type SameRep  a b = forall xx. Rep  a xx `Coercible` Rep  b xx
--   type SameRep1 f f = forall xx. Rep1 f xx `Coercible` Rep1 g xx
----------------------------------------------------------------------

class    (Rep a x `Coercible` Rep b x) => SameRep_ a b x
instance (Rep a x `Coercible` Rep b x) => SameRep_ a b x

class    (Rep1 f x `Coercible` Rep1 g x) => SameRep1_ f g x
instance (Rep1 f x `Coercible` Rep1 g x) => SameRep1_ f g x

type SameRep  a b = Forall (SameRep_  a b)
type SameRep1 f g = Forall (SameRep1_ f g)

expand :: SameRep_ a b x :- (Rep a x `Coercible` Rep b x)
expand = Sub Dict

expand1 :: SameRep1_ f g x :- (Rep1 f x `Coercible` Rep1 g x)
expand1 = Sub Dict

swap :: Coercible a b :- Coercible b a
swap = Sub Dict

sameRep :: forall a b x. SameRep a b :- (Rep a x `Coercible` Rep b x)
sameRep = 
  inst @(SameRep_ a b) @x
  >>>
  expand

sameRep1 :: forall f g x. SameRep1 f g :- (Rep1 f x `Coercible` Rep1 g x)
sameRep1 = 
  inst @(SameRep1_ f g) @x 
  >>>
  expand1

type a <~>  b = (SameRep  a b, Generic  a, Generic  b)
type f <~~> g = (SameRep1 f g, Generic1 f, Generic1 g)

type f ~> g = forall xx. f xx -> g xx

----------------------------------------------------------------------
-- Convenience functions
----------------------------------------------------------------------
back :: forall a b. a <~> b => a -> b
back = id
  >>> from
  >>> coerce'
  >>> to

  where
    coerce' :: forall x. Rep a x -> Rep b x
    coerce' = coerce \\ sameRep @a @b @x

forth :: forall b a. a <~> b => b -> a
forth = id
  >>> from
  >>> coerce'
  >>> to

  where
    coerce' :: forall x. Rep b x -> Rep a x
    coerce' = coerce \\ swap @(Rep a x) @(Rep b x) \\ sameRep @a @b @x 

back1 :: forall f g x. f <~~> g => f ~> g 
back1 = id
  >>> from1
  >>> coerce'
  >>> to1

  where
    coerce' :: forall x. Rep1 f x -> Rep1 g x
    coerce' = coerce \\ sameRep1 @f @g @x

forth1 :: forall f g x. f <~~> g => g x -> f x
forth1 = id
  >>> from1
  >>> coerce'
  >>> to1

  where
    coerce' :: Rep1 g x -> Rep1 f x 
    coerce' = coerce \\ swap @(Rep1 f x) @(Rep1 g x) \\ sameRep1 @f @g @x 

----------------------------------------------------------------------
-- Deriving via Rep1
----------------------------------------------------------------------

newtype Generically1 f a = Generically1 { generically1 :: f a }

instance (Functor (Rep1 f), Generic1 f) => Functor (Generically1 f) where
  fmap f = Generically1 . to1 . fmap f . from1 . generically1

instance (Applicative (Rep1 f), Generic1 f) => Applicative (Generically1 f) where
  pure = Generically1 . to1 . pure

  Generically1 f <*> Generically1 x = Generically1 $ to1 $ 
    from1 f <*> from1 x

instance (Monad (Rep1 m), Generic1 m) => Monad (Generically1 m) where
  Generically1 m >>= f = Generically1 $ to1 $
    from1 m >>= from1 . generically1 . f 

data Product f g h a = Product { fs :: f (g (f a)), sn :: h (f (g a)) }
  deriving stock (Generic1)
    
  deriving (Functor, Applicative)
    via
      (Generically1 (Product f g h))

----------------------------------------------------------------------
-- Deriving via type with shared Rep
----------------------------------------------------------------------

data family GenericallyAs :: k -> k -> k

-- GenericallyAs :: Type -> Type -> Type
newtype instance (a `GenericallyAs` other)   = GenericallyAs  { genericallyAs  :: a   }

-- GenericallyAs :: (Type -> Type) -> (Type -> Type) -> (Type -> Type)
newtype instance (f `GenericallyAs` other) a = GenericallyAs1 { genericallyAs1 :: f a }

pattern Hel :: a<~>other => other -> GenericallyAs a other
pattern Hel a <- (GenericallyAs (back -> a))
  where Hel a  = (GenericallyAs (forth a))

pattern Hel1 :: f<~~>other => other a -> GenericallyAs f other a
pattern Hel1 a <- (GenericallyAs1 (back1 -> a))
  where Hel1 a  = (GenericallyAs1 (forth1 a))

instance (a<~>other, Semigroup other) => Semigroup (a `GenericallyAs` other) where
  Hel a <> Hel b = Hel (a S.<> b)

instance (a<~>other, Monoid other) => Monoid (a `GenericallyAs` other) where
  mempty = Hel mempty

  Hel a `mappend` Hel b = Hel (a S.<> b)

instance (a<~>other, Show other) => Show (a `GenericallyAs` other) where
  show :: (a `GenericallyAs` other) -> String
  show (Hel a) = show a

instance (a<~>other, Num other) => Num (a `GenericallyAs` other) where
  (+) :: a `GenericallyAs` other -> a `GenericallyAs` other -> a `GenericallyAs` other
  Hel a + Hel b = Hel (a + b)

  (*) :: a `GenericallyAs` other -> a `GenericallyAs` other -> a `GenericallyAs` other
  Hel a * Hel b = Hel (a * b)

  (-) :: a `GenericallyAs` other -> a `GenericallyAs` other -> a `GenericallyAs` other
  Hel a - Hel b = Hel (a - b)

  abs :: a `GenericallyAs` other -> a `GenericallyAs` other
  abs (Hel a) = Hel (abs a)

  signum :: a `GenericallyAs` other -> a `GenericallyAs` other
  signum (Hel a) = Hel (signum a)

  fromInteger :: Integer -> a `GenericallyAs` other
  fromInteger int = Hel (fromInteger int)

-- So it naively bounces around a lot
--
--     F A
--   ={ from1 }
--     Rep1 F A
--   ={ coerce }
--     Rep1 Other A
--   ={ to1 }
--     Other A
--   ={ fmap f }
--     Other A'
--   ={ from1 }
--     Rep1 Other A'
--   ={ coerce }
--     Rep1 F A'
--   ={ to1 }
--     F A'
instance (f<~~>other, Functor other) => Functor (f `GenericallyAs` other) where
  fmap :: forall a a'. (a -> a') -> ((f `GenericallyAs` other) a -> (f `GenericallyAs` other) a')
  fmap f (Hel1 xs) = Hel1 (fmap f xs)

-- From /linear/
data V2 a = V2 !a !a
  deriving stock (Generic, Generic1)
  deriving stock (Show, Functor)

  deriving (Applicative)
    via (Generically1 V2)

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

----------------------------------------------------------------------
-- Here we have an existing 'V2' data type from some library
-- 
-- 'Generic1' establishes an isomorphism between 'Pair a' and 'V2 a'
-- allowing us to steal 

-- This uses 'Functor' and 'Show' (taken from 'V2')
-- 
-- >>> (+ 5) <$> 10:#20
-- V2 15 25

-- This uses 'Semigroup', 'Monoid' from (a, a)
--
-- >>> ("hi" :# "world") <> ("!" :# "?!?")
-- V2 "hi!" "world?!?"

-- And this uses 'Num' from (V2 a)
-- 
-- >>> ((10 :# 20) + (1111 :# 2222))
-- V2 1121 2242
data Pair a = a :# a
  deriving stock (Generic, Generic1)

  deriving (Semigroup, Monoid)
    via (Pair a `GenericallyAs` (a, a))

  deriving (Num, Show)
    via (Pair a `GenericallyAs` V2 a)

  deriving Functor
    via (Pair `GenericallyAs` V2)

data Foo a = F a a a a a
  deriving (Generic)
  deriving (Semigroup, Monoid)
    via (Foo a `GenericallyAs` (a, a, a, a, a))

