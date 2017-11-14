% Stolen Instances Taste Just Fine
% Baldur Blöndal / `@Iceland_jack` / `/u/Iceland_jack`
% October, 2017

# Thanks to 

+ Ryan Scott and Andres Löh

+ Hans Höglund

# Boilerplate & Patterns & Laws

Some yummy boilerplate

```haskell
instance Monoid b => Monoid (a -> b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate

```haskell
instance Monoid b => Monoid ((a ->) b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate

```haskell
instance Monoid a => Monoid (IO a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate

```haskell
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate

```haskell
instance (Monoid a, Monoid b) => Monoid ((a ,) b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate

```haskell
instance (Applicative f, Monoid a) => Monoid (f a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

. . .

> `Applicative`s, `Traversable`s and `Monoid`s give us the basic
> building blocks for a lot of **routine programming**. Every
> `Applicative` *f* can be used to lift monoids, ...
>
> — Conor McBride

+ Same behaviour for all instances (overlaps with `Monoid [a]`!)

. . .

More options...

    ```haskell
    instance Alternative f => Monoid (f a) where
      mempty  = zero
      mappend = (<|>)
    ```

# “It's not so ba—”

```haskell
instance (Applicative f, Num a) => Num (f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = liftA  negate
  abs         = liftA  abs
  signum      = liftA  signum
  fromInteger = pure . fromInteger
```

# “It's not so ba—.. oh.”

```haskell
instance (Applicative f, Num a) => Num (f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = liftA  negate
  abs         = liftA  abs
  signum      = liftA  signum
  fromInteger = pure . fromInteger

instance (Applicative f, Fractional a) => Fractional (f a) where
  recip        = liftA recip
  fromRational = pure . fromRational
```


# “It's not so ba—.. oh s”

```haskell
instance (Applicative f, Num a) => Num (f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = liftA  negate
  abs         = liftA  abs
  signum      = liftA  signum
  fromInteger = pure . fromInteger

instance (Applicative f, Fractional a) => Fractional (f a) where
  recip        = liftA recip
  fromRational = pure . fromRational

instance (Applicative f, Floating a) => Floating (f a) where
  pi    = pure  pi
  sqrt  = liftA sqrt
  exp   = liftA exp
  log   = liftA log
  sin   = liftA sin
  cos   = liftA cos
  asin  = liftA asin
  atan  = liftA atan
  acos  = liftA acos
  sinh  = liftA sinh
  cosh  = liftA cosh
  asinh = liftA asinh
  atanh = liftA atanh
  acosh = liftA acosh
```

# How do we capture this? 

`CPP`?

# How do Haskellers ..
#### .. give different behaviour to the same representation?

We introduce a newtype, attach behaviour to it

```haskell
newtype App f a = App (f a)




```

We attach the “**routine programming**” pattern to `App`

```haskell
instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty                    = App (pure   mempty)
  mappend (App fa) (App fb) = App (liftA2 mappend fa fb)
```

# How do Haskellers ..
#### .. give different behaviour to the same representation?

We introduce a newtype, attach behaviour to it

```haskell
newtype App f a = App (f a)
  deriving newtype
    (Functor, Applicative)
```

We attach the “**routine programming**” pattern to `App`

```haskell
instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

- What is the in-memory representation of `App (Either A) B`?

# Philosophy

- ### “It’s good to think of **types**

- #### as not just the **representation** of data

- ## but the equipment of **data with structure**.”

- ### — Conor McBride

# `{-# Language DerivingVia #-}`

Enter `DerivingVia` 

```haskell
data Foo a = ... 
  deriving (Semigroup, Monoid)
    via (App Foo a)
```

# `{-# Language DerivingVia #-}`

Enter `DerivingVia` 

```haskell
data Foo a = ... 
  deriving (Semigroup, Monoid, Num, Floating, Fractional)
    via (App Foo a)
```

* Translates into

    ```haskell
    instance Semigroup a => Semigroup (Foo a) where
      (<>) = coerce ((<>) @(App Foo a))
    
    instance Monoid a => Monoid (Foo a) where
      mempty = coerce (mempty @(App Foo a))
    
    instance Num a => Num (Foo a) where
      (+) = coerce ((+) @(App Foo a))
      -- ...
    
    instance Floating a => Floating (Foo a) where
      pi = coerce (pi @(App Foo a))
      -- ...
    ```

# `Coercible`

We can coerce between newtypes and the values they wrap,

```haskell
coerce :: Coercible a b => a -> b
```

. . .

```haskell
instance Monoid a => Monoid (F a) where
  mempty :: F a
  mempty = coerce 
    (mempty  :: App F a)

  mappend :: F a -> F a -> F a
  mappend = coerce 
    (mappend :: App F a -> App F a -> App F a)
```

. . .

```haskell
newtype App f a = App (f a)

coerce :: App F a -> F a

coerce :: (App F a -> App F a -> App F a) -> (F a -> F a -> F a)
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically?

```haskell
-- genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a

instance Arbitrary Person where
  arbitrary :: Gen Person
  arbitrary = genericArbitrary
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`?

```haskell
-- arbitrarySizedIntegral :: Integral a => Gen a

instance Arbitrary Int where
  arbitrary :: Gen Int
  arbitrary = arbitrarySizedIntegral
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`?

```haskell
-- arbitrarySizedIntegral :: Integral a => Gen a

instance Arbitrary Integer where
  arbitrary :: Gen Integer
  arbitrary = arbitrarySizedIntegral
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`? `Bounded` `Integral`? 

```haskell
-- arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a

instance Arbitrary Int8 where
  arbitrary :: Gen Int8
  arbitrary = arbitrarySizedBoundedIntegral
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`? `Bounded` `Integral`? 

```haskell
-- arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a

instance Arbitrary Int16 where
  arbitrary :: Gen Int16
  arbitrary = arbitrarySizedBoundedIntegral
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`? `Bounded` `Integral`? 

```haskell
-- arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a

instance Arbitrary Int32 where
  arbitrary :: Gen Int32
  arbitrary = arbitrarySizedBoundedIntegral
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`? `Bounded` `Integral`? 

```haskell
-- arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a

instance Arbitrary Int64 where
  arbitrary :: Gen Int64
  arbitrary = arbitrarySizedBoundedIntegral
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`? `Bounded` `Integral`? `Fractional`?

```haskell
-- arbitrarySizedFractional :: Fractional a => Gen a

instance Arbitrary Float where
  arbitrary :: Gen Float
  arbitrary = arbitrarySizedFractional
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`? `Bounded` `Integral`? `Fractional`?

```haskell
-- arbitrarySizedFractional :: Fractional a => Gen a

instance Arbitrary Double where
  arbitrary :: Gen Double
  arbitrary = arbitrarySizedFractional
```

# Default Deriving / Vocabulary (**QuickCheck**)

Defining `Arbitrary`

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Generically? Via `Integral`? `Bounded` `Integral`? `Fractional`?

```haskell
-- arbitrarySizedFractional :: Fractional a => Gen a

instance Integral a => Arbitrary (Ratio a) where
  arbitrary :: Gen (Ratio a)
  arbitrary = arbitrarySizedFractional
```

# Many valid instances!

We can codify each of them as an instance 

```haskell
newtype ArbitraryGeneric a = ArbitraryGeneric a
    
instance (Generic a, GArbitrary (Rep a)) => Arbitrary (ArbitraryGeneric a) where
  arbitrary :: Gen (ArbitraryGeneric a)
  arbitrary = ArbitraryGeneric <$> genericArbitrary
```

. . .

that we can pick at deriving site

```haskell
data Foo = ... 
  deriving (Arbitrary)
    via (ArbitraryGeneric Foo)

  deriving stock (Generic)
```

# Many valid instances!

We can codify each of them as an instance 

```haskell
newtype ArbitraryIntegral a = ArbitraryIntegral a

instance Integral a => Arbitrary (ArbitraryIntegral a) where
  arbitrary :: Gen (ArbitraryIntegral a)
  arbitrary = ArbitraryIntegral <$> arbitrarySizedIntegral
```

that we can pick at deriving site

```haskell
data Foo = ... 
  deriving (Arbitrary)
    via (ArbitraryIntegral Foo)

instance Integral Foo
```

# Many valid instances!

We can codify each of them as an instance 

```haskell
newtype ArbitraryBoundedIntegral a = ArbitraryBoundedIntegral a

instance (Bounded a, Integral a) => Arbitrary (ArbitraryBoundedIntegral a) where
  arbitrary :: Gen (ArbitraryBoundedIntegral a)
  arbitrary = ArbitraryBoundedIntegral <$> arbitrarySizedBoundedIntegral
```

that we can pick at deriving site

```haskell
data Foo = ... 
  deriving (Arbitrary)
    via (ArbitraryBoundedIntegral Foo)

instance Bounded  Foo
instance Integral Foo
```

# Composing Existing Modifiers

```haskell





```

**QuickCheck** has many modifiers

```haskell
newtype Nums = Nums [Int]
  deriving Arbitrary
    via [Int]

> sample (arbitrary @Nums)
[]
[0,-2]
[]
[0,-2,-3]
[5,4,-5,5]
[9,0]
[-5,1,-5,2,11]
```
# Composing Existing Modifiers

```haskell
newtype NonEmptyList a = NonEmpty [a] -- Only generates non-empty lists




```

**QuickCheck** has many modifiers

```haskell
newtype Nums = Nums [Int]
  deriving Arbitrary 
    via (NonEmptyList Int)

> sample (arbitrary @Nums)
[2,1]
[1]
[-3,2]
[-6,3,-4,6]
[-1,6,7,4,-3]
[2,10,9,-7,8,-9,-7,4,4]
[12,5,5,9,10]
```

# Composing Existing Modifiers

```haskell
newtype NonEmptyList a = NonEmpty [a] -- Only generates non-empty lists
newtype Positive     a = Positive a   -- x > 0



```

**QuickCheck** has many modifiers

```haskell
newtype Nums = Nums [Int]
  deriving Arbitrary
    via (NonEmptyList (Positive Int))

> sample (arbitrary @Nums)
[2]
[1,2]
[3,4]
[2,5]
[1]
[8,2,4,3,4,5,1,7]
[10,6,2,11,10,3,2,11,12]
```

# Composing Existing Modifiers

```haskell
newtype NonEmptyList a = NonEmpty [a] -- Only generates non-empty lists
newtype Positive     a = Positive a   -- x > 0
newtype Large        a = Large    a   -- large numbers
```

**QuickCheck** has many modifiers

```haskell
newtype Nums = Nums [Int]
  deriving Arbitrary 
    via (NonEmptyList (Positive (Large Int)))

> sample (arbitrary @Nums)
[2]
[2,1]
[2,7,8,4]
[11,13]
[8,40,17,57,16,51,88,58]
[249,27]
[511,642]
```

# Uses

- Kill boilerplate 

- No more (distinguished) default methods

- Generalizes `GeneralizedNewtypeDeriving`

- Captures patterns, optimisations 

- Deriving serialisation with options

    ```haskell
    deriving (ToJSON, FromJSON, Show, Read, ...)
      via (SerializeAs OmitNothingField (StripPrefix "scb") ...)
    ```

- Transfer behaviour (*instances*) between types of same *representation*

- Prefer `Monoidal` formulation of `Applicative`? sorted m8

    ```haskell
    class Functor f => Monoidal f where
      unit :: f ()
      mult :: f a -> f b -> f (a, b)

    instance Monoidal f => Applicative (W f)
    ```

# The End

- Slides: https://rawgit.com/Icelandjack/deriving-via/master/mypresentation.html (goo.gl/fsnZnq)

- **Goal**: Create vocabulary of derivability

- Thank You

# Example of isomorphisms

```haskell
newtype StealFrom  a other   = StealFrom a
newtype StealFrom1 f other a = StealFrom (f a)
```

We steal from phantom type `other`.

. . .

```haskell
type HasSameRep  a b = (Generic  a, Generic  b, forall xx. Coercible (Rep  a xx) (Rep  b xx))
type HasSameRep1 f g = (Generic1 f, Generic1 g, forall xx. Coercible (Rep1 f xx) (Rep1 g xx))

instance (HasSameRep  a other, Semigroup   other) => Semigroup   (a `StealFrom`  other) 
instance (HasSameRep  a other, Monoid      other) => Monoid      (a `StealFrom`  other) 
instance (HasSameRep  a other, Show        other) => Show        (a `StealFrom`  other) 
instance (HasSameRep  a other, Num         other) => Num         (a `StealFrom`  other) 
instance (HasSameRep1 f other, Functor     other) => Functor     (f `StealFrom1` other) 
instance (HasSameRep1 f other, Applicative other) => Applicative (f `StealFrom1` other) 
```

# as well as entire catalogues from other types

```haskell
data Pair a = a :# a
  deriving stock (Generic, Generic1)

  deriving (Semigroup, Monoid)
    via (Pair a `StealingFrom` (a, a))

  deriving ( Num, Show, Bounded, Ix
           , Storable, Binary, Serial, Hashable, 
           , Unbox, Ixed)
    via (Pair a `StealingFrom` V2 a)

  deriving ( Functor, Applicative, Monad, Distributive
           , Representable, MonadFix, MonadZip
           , Serial1, Traversable1, Apply, Bind, Additive,
           , Metric, Finite, R1, R2, Trace, Affine)
    via (Pair `StealingFrom1` V2)

  deriving (Floating, Fractional)
    via (App Pair a)
```

# Associated type family

```haskell
class Functor f => Representable f where
  type Rep f :: Type

  index    :: f a -> (Rep f -> a)
  tabulate :: f a <- (Rep f -> a)
```

gives us `Applicative`, `Monad`, `Distributive`, `MonadReader (Rep f)`, `FunctorWithIndex`, `FoldableWithIndex`, `TraversableWithIndex` ....

# Deriving `hask` 

```haskell
newtype Wrap cat a b = Wrap (cat a b)

instance Category cat => Functor (Wrap cat a) where
  type Dom (Wrap cat a) = cat
  type Cod (Wrap cat a) = (->)

  fmap :: forall b b'. cat b b' -> (Wrap cat a b -> Wrap cat a b')
  fmap = coerce ((.) @_ @cat @b @b' @a)
```

replaces

```haskell
instance Functor (a ->) where
  type Dom (a ->) = (->)
  type Cod (a ->) = (->)
  fmap = (.)
```

## Well known: `fmap = (.)`

# Deriving `hask`

```haskell
newtype Wrap cat a b = Wrap (cat a b)

instance Category cat => Functor (Wrap cat a) where
  type Dom (Wrap cat a) = cat
  type Cod (Wrap cat a) = (->)

  fmap :: forall b b'. cat b b' -> (Wrap cat a b -> Wrap cat a b')
  fmap = coerce ((.) @_ @cat @b @b' @a)
```

replaces

```haskell
instance (Category p, Category q) => Functor (Nat p q f) where
  type Dom (Nat p q f) = Nat p q
  type Cod (Nat p q f) = (->)
  fmap = (.)
```

# Deriving `hask`

```haskell
newtype Wrap cat a b = Wrap (cat a b)

instance Category cat => Functor (Wrap cat a) where
  type Dom (Wrap cat a) = cat
  type Cod (Wrap cat a) = (->)

  fmap :: forall b b'. cat b b' -> (Wrap cat a b -> Wrap cat a b')
  fmap = coerce ((.) @_ @cat @b @b' @a)
```

replaces

```haskell
instance (Category cat, Op cat ~ Y cat) => Functor (Y cat a) where
  type Dom (Y cat a) = Y cat
  type Cod (Y cat a) = (->)
  fmap = (.)
```

# Deriving `hask`

```haskell
newtype Wrap cat a b = Wrap (cat a b)

instance Category cat => Functor (Wrap cat a) where
  type Dom (Wrap cat a) = cat
  type Cod (Wrap cat a) = (->)

  fmap :: forall b b'. cat b b' -> (Wrap cat a b -> Wrap cat a b')
  fmap = coerce ((.) @_ @cat @b @b' @a)
```

replaces

```haskell
instance Functor (a :-) where
  type Dom (a :-) = (:-)
  type Cod (a :-) = (->)
  fmap = (.)
```

# Deriving `hask`

```haskell
newtype Wrap cat a b = Wrap (cat a b)

instance Category cat => Functor (Wrap cat a) where
  type Dom (Wrap cat a) = cat
  type Cod (Wrap cat a) = (->)

  fmap :: forall b b'. cat b b' -> (Wrap cat a b -> Wrap cat a b')
  fmap = coerce ((.) @_ @cat @b @b' @a)
```

replaces

```haskell
instance Functor (a :~:) where
  type Dom (a :~:) = (:~:)
  type Cod (a :~:) = (->)
  fmap = (.)
```

# Deriving `hask`

```haskell
newtype Wrap cat a b = Wrap (cat a b)

instance Category cat => Functor (Wrap cat a) where
  type Dom (Wrap cat a) = cat
  type Cod (Wrap cat a) = (->)

  fmap :: forall b b'. cat b b' -> (Wrap cat a b -> Wrap cat a b')
  fmap = coerce ((.) @_ @cat @b @b' @a)
```

replaces

```haskell
instance Functor (Coercion a) where
  type Dom (Coercion a) = Coercion
  type Cod (Coercion a) = (->)
  fmap = (.)
```

<!--- pandoc -f markdown -t slidy -i -s --self-contained -o mypresentation.html Presentation.md --->