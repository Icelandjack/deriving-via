{-# Language DerivingStrategies, RankNTypes, FlexibleContexts, UndecidableInstances, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, ScopedTypeVariables, GADTs, MultiParamTypeClasses, ConstraintKinds, EmptyCase, InstanceSigs, FlexibleInstances, TypeApplications, DeriveGeneric #-}

import GHC.Generics
import Data.Kind
import Data.Coerce
import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Data.Type.Coercion

-- import Data.Constraint
data Dict c where Dict :: c => Dict c
newtype a :- b = Sub (a => Dict b)
(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r
instance Category (:-) where
  id = Sub Dict 
  f . g = Sub $ Dict \\ f \\ g

-- import Data.Constraint.Forall
class Forall (p :: k -> Constraint) 
inst :: Forall p :- p a
inst = undefined 

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

-- We define a type (F :: Type -> Type) 

data family GenericallyAs :: k -> k -> k

newtype instance (a `GenericallyAs` other)   = GenericallyAs  { genericallyAs  :: a   }
newtype instance (f `GenericallyAs` other) a = GenericallyAs1 { genericallyAs1 :: f a }

instance (SameRep a other, Generic a, Generic other, Show other) => Show (a `GenericallyAs` other) where

  show :: (a `GenericallyAs` other) -> String
  show = 
    show @other . to . coerce' . from . genericallyAs where

      coerce' :: forall x. Rep a x -> Rep other x
      coerce' = coerce
        \\ sameRep @a @other @x

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
instance (SameRep1 f other, Generic1 f, Generic1 other, Functor other) => Functor (f `GenericallyAs` other) where
  fmap :: forall a a'. (a -> a') -> ((f `GenericallyAs` other) a -> (f `GenericallyAs` other) a')
  fmap f = 
    genericallyAs1
    >>>
    from1
    >>>
    coerce 
      \\ sameRep1 @f @other @a
    >>>
    to1
    >>>
    fmap f
    >>>
    from1 @_ @other
    >>>
    coerce 
      \\ swap @(Rep1 f a') @(Rep1 other a') 
      \\ sameRep1 @f @other @a'
    >>>
    to1
    >>>
    GenericallyAs1
