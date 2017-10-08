{-# Language GeneralizedNewtypeDeriving, DerivingStrategies #-}

import Control.Applicative
import Data.Semigroup ((<>))

newtype App f a = App (f a)
  deriving newtype
    (Functor, Applicative)

instance (Applicative f, Semigroup a) => Semigroup (App f a) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty = pure mempty

  mappend = liftA2 mappend

newtype Foo f = Foo (f ()) 
  deriving (Semigroup, Monoid)
    via (App f ())
