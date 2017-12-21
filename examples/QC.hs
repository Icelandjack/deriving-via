{-# LANGUAGE TypeOperators, TypeInType, GADTs, MultiParamTypeClasses,
             AllowAmbiguousTypes, TypeFamilies, ScopedTypeVariables,
             RebindableSyntax, ConstraintKinds,
             FlexibleContexts, LambdaCase, EmptyCase, FlexibleInstances, 

             TemplateHaskell, TypeApplications, UndecidableInstances, 
             InstanceSigs, DerivingStrategies #-}

module QC where

import Data.List (sort)
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Prelude
import Data.Kind

import Test.QuickCheck

newtype ArbBounded a = ArbBounded a deriving (Enum, Bounded) via (a)

instance (Bounded a, Enum a) => Arbitrary (ArbBounded a) where
  arbitrary :: Gen (ArbBounded a)
  arbitrary = arbitraryBoundedEnum

singletons [d|
  data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Show, Bounded, Enum)

    deriving Arbitrary via (ArbBounded Day)

  isWeekend :: Day -> Bool
  isWeekend day = day `elem` [Sat, Sun]

  isWeekday :: Day -> Bool
  isWeekday day = day `elem` [Mon, Tue, Wed, Thu, Fri]
  |]

instance SingI IsWeekendSym0 where
  sing :: Sing IsWeekendSym0
  sing = singFun1 @IsWeekendSym0 sIsWeekend

instance SingI IsWeekdaySym0 where
  sing :: Sing IsWeekdaySym0
  sing = singFun1 @IsWeekdaySym0 sIsWeekday

----------------------------------------------------------------------
-- Adapter that generates an arbitrary values satisfying a predicate
newtype SuchThat :: (a ~> Bool) -> Type where
  SuchThat :: a -> SuchThat (predicate::a ~> Bool)

-- From Richard's Effects library.
type Good a = (Demote a ~ a, SingKind a)

instance (Arbitrary a, Good a, SingI predicate) => Arbitrary (SuchThat (predicate::a ~> Bool)) where
  arbitrary :: Gen (SuchThat predicate)
  arbitrary = SuchThat <$> suchThat arbitrary 
    (fromSing (sing :: Sing predicate))

-- >>> sample (arbitrary @Weekend)
-- Sat
-- Sun
-- Sun
-- Sat
-- Sun
-- Sat
-- Sun
-- Sat
-- Sun
-- Sat
-- Sun
newtype Weekend = Weekend Day 
  deriving Show via Day
  deriving (Arbitrary) via 
    (SuchThat IsWeekendSym0)
      -- type IsWeekend = FlipSym2 ElemSym0 '[ 'Sat, 'Sun ]

-- >>> sample (arbitrary @Weekday)
-- Thu
-- Fri
-- Thu
-- Wed
-- Tue
-- Mon
-- Wed
-- Fri
-- Wed
-- Mon
-- Wed
newtype Weekday = Weekday Day 
  deriving Show via Day
  deriving Arbitrary via 
    SuchThatWeekday
      -- type IsWeekday = FlipSym2 ElemSym0 '[ 'Mon, 'Tue, 'Wed, 'Thu, 'Fri ]
      -- type IsWeekday = NotSym0 :.$$$ IsWeekendIsWeekdaySym0

type SuchThatWeekday = SuchThat IsWeekdaySym0

----------------------------------------------------------------------

-- We can hopefully derive the modifiers from QuickCheck
singletons [d|
  ordered :: Ord a => [a] -> Bool
  ordered xs = sort xs == xs
  |]

-- TODO
-- 
newtype OrderedList a = OL [a]
  deriving Show via
    ([a])
--   deriving Arbitrary via (SuchThat OrderedSym0)
    

{-
newtype OrderedList  a  = OL [a]    deriving Arbitrary via (SuchThat (\xs -> sort xs == xs)
newtype NonEmptyList a  = NE [a]    deriving Arbitrary via (SuchThat (not . null))
newtype Positive     a  = P  a      deriving Arbitrary via (SuchThat (> 0))
newtype NonZero      a  = NZ a      deriving Arbitrary via (SuchThat (/= 0))
newtype NonNegative  a  = NN a      deriving Arbitrary via (SuchThat (>= 0))
newtype PrintableString = PS String deriving Arbitrary via (SuchThat (all isPrint))
-}


