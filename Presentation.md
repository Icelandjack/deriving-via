% Stolen Instances Taste Sweet
% Baldur Blöndal / `@Iceland_jack`
% October, 2017

# What I care about....

# What I ~~care about~~ enjoy....

- ... capturing **patterns** as a library 

    - ~~lore~~

- ... free code

    - ~~boilerplate~~

- ... **properties**, not representation in memory

- ... being able to explore **quickly**

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```haskell
instance Monoid b => Monoid (a -> b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```haskell
instance Monoid b => Monoid ((a ->) b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```haskell
instance Monoid a => Monoid (IO a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```haskell
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```haskell
instance (Monoid a, Monoid b) => Monoid ((a ,) b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

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

. . . 

This gives same behaviour for all instances! (we definitely don't
always want that)


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

# What do Haskellers do ..
#### .. when they want to separate behaviour from representation? 

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

# What do Haskellers do ..
#### .. when they want to separate behaviour from representation? 

We introduce a newtype, attach behaviour to it

```haskell
newtype App f a = App (f a)
  deriving newtype
    (Functor, Applicative)
```

We attach the “**routine programming**” pattern to `App`

```haskell
instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty                    = pure mempty
  mappend                   = liftA2 mappend
```

- What is the in-memory representation of `App (Either A) B`?

# `Coercible`

We can coerce between newtypes and the values they wrap,

```haskell
coerce :: Coercible a b => a -> b
```

```haskell
instance Monoid a => Monoid (IO a) where
  mempty :: IO a
  mempty = coerce 
    (mempty  :: App IO a)

  mappend :: IO a -> IO a -> IO a
  mappend = coerce 
    (mappend :: App IO a -> App IO a -> App IO a)
```

. . .

```haskell
newtype App f a = App (f a)

coerce :: App IO a 
       -> IO a

coerce :: (App IO a -> App IO a -> App IO a)
       -> (IO     a -> IO     a -> IO     a)
```

# `{-# Language DerivingVia #-}`

This is where `DerivingVia` comes in

```haskell
data Foo a = ... 
  deriving Monoid 
    via (App Foo a)
```

# `{-# Language DerivingVia #-}`

This is where `DerivingVia` comes in

```haskell
data Foo a = ... 
  deriving (Semigroup, Monoid)
    via (App Foo a)
```

# `{-# Language DerivingVia #-}`

This is where `DerivingVia` comes in

```haskell
data Foo a = ... 
  deriving (Semigroup, Monoid, Num, Floating, Fractional)
    via (App Foo a)
```

* Translates into

    ```haskell
    instance Semigroup a => Semigroup (Foo a) where
      (<>) = coerce ((<>) @(Foo a))
    
    instance Monoid a => Monoid (Foo a) where
      mempty = coerce (mempty @(Foo a))
    
    instance Num a => Num (Foo a) where
      (+) = coerce ((+) @(Foo a))
      -- ...
    
    instance Floating a => Floating (Foo a) where
      pi = coerce (pi @(Foo a))
      -- ...
    ```

# Default Deriving / Vocabulary (**QuickCheck**)

**Problem**: There are many valid `Arbitrary` you can derive

```haskell
arbitrarySizedIntegral        :: Integral a              => Gen a
arbitrarySizedFractional      :: Fractional a            => Gen a
arbitraryBoundedRandom        :: (Bounded a, Random a)   => Gen a
arbitraryBoundedEnum          :: (Bounded a, Enum a)     => Gen a
```

* We can codify each of them as an instance 

    ```haskell
    newtype BoundedRandom a = BR a
    
    instance (Bounded a, Random a) => Arbitrary (BoundedRandom a) where
      arbitrary = BR <$> arbitraryBoundedRandom
    ```

* `DerivingVia` lets you pick at deriving site

    ```haskell
    data Foo = ... deriving (Arbitrary)
      via (BoundedRandom Foo)

    instance Bounded Foo where ..
    instance Random  Foo where ..
    ```

# Composing Existing Modifiers

```haskell





```

**QuickCheck** has many modifiers

```haskell
newtype Nums a = Nums [a]
  deriving newtype
    Arbitrary

> sample (arbitrary @(Nums Int))
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
newtype Nums a = Nums [a]
  deriving 
    Arbitrary via (NonEmptyList a)

> sample (arbitrary @(Nums Int))
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
newtype Nums a = Nums [a]
  deriving 
    Arbitrary via (NonEmptyList (Positive a))

> sample (arbitrary @(Nums Int))
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
newtype Nums a = Nums [a]
  deriving 
    Arbitrary via (NonEmptyList (Positive (Large a)))

> sample (arbitrary @(Nums Int))
[2]
[2,1]
[2,7,8,4]
[11,13]
[8,40,17,57,16,51,88,58]
[249,27]
[511,642]
```

# Way of Thinking 

> “It’s good to think of **types**
> as not just the **representation** of data
> but the equipment of **data with structure**.”
>
> — Conor McBride

- One representation, multiple structures

    ```haskell
    instance (Applicative f, Monoid a) => Monoid (f a) where
      mempty  = pure   mempty
      mappend = liftA2 mappend

    instance Alternative f => Monoid (f a) where
      mempty  = zero
      mappend = (<|>)
    ```

- Get reified as instances

    ```haskell
    instance (Applicative f, Monoid a) => Monoid (App f a)
    instance (Alternative f)           => Monoid (Alt f a)
    ```

# JSON

* **Problem**: Only *one* default method.

* **Problem**: What if the default method changes?

* **Problem**: What if you want to derive with options?

    ```haskell
    data State = State
      { scbColor      :: Maybe Double 
      , scbBrightness :: Maybe Double
      } 
      deriving (ToJSON, FromJSON, Show, Read, ...)
        via (SerializeAs OmitNothingField (StripPrefix "scb") State)
    ```

- Could work to derive Haskell serialization

    ```haskell
    >>> State Nothing Nothing
    State { }
    >>> State Nothing (Just 4.4)
    State { Brightness = 4.4 }
    ```

# The Main Idea

- All of this is 

    - *no-op*

    - *zero-cost* operation

    - *safe*

- Transfer behaviour (*structure*) between types of same *representation*

- Works with associated type families

    - Can swap it out for isomorphic type

# Uses

- Reduces boilerplate 

- No more default methods

    - Not tied to a class declaration 

    - No single distinguished derivation

- Captures patterns

    - Groups patterns, keeps in sync

    - Captures optimisations

- Any isomorphism (`Generic`, `Representable`) lets us carry over structure 

    - From representation

    - From anything sharing a representation

- Prefer `Monoidal` formulation of `Applicative`? Sorted m8.

# The End

* Thank You

# ADDENDUM: Isomorphic Structures

- So far: we have stuck to a single representation

- We can steal any instance as long as we have an isomorphism

- ## `GHC.Generics`

- ## `Iso1` (your own)

- ## `Representable`

# Example of isomorphisms

```haskell
newtype StealFrom  a other   = StealFrom a
newtype StealFrom1 f other a = StealFrom (f a)

type a <~>  b = (forall xx. Rep  a xx `Coercible` Rep  b xx, Generic  a, Generic  b)
type f <~~> g = (forall xx. Rep1 f xx `Coercible` Rep1 g xx, Generic1 f, Generic1 g)

instance (a <~>other, Semigroup   other) => Semigroup   (a `StealFrom`  other) 
instance (a <~>other, Monoid      other) => Monoid      (a `StealFrom`  other) 
instance (a <~>other, Show        other) => Show        (a `StealFrom`  other) 
instance (a <~>other, Num         other) => Num         (a `StealFrom`  other) 
instance (f<~~>other, Functor     other) => Functor     (f `StealFrom1` other) 
instance (f<~~>other, Applicative other) => Applicative (f `StealFrom1` other) 
```

- Can derive

    ```haskell
    data Foo a = F { foobs :: [a], foobOfHonour :: Maybe a }
      deriving (Generic1)
      deriving (Functor, Applicative, Alternative)
           via (Foo `StealingFrom1` P.Product ZipList (Alt Maybe))
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

- And sometimes `Comonad`

    ```haskell
    instance (Representable f, Monoid (Rep f)) => Comonad (Co f)
    ```

# 

We also have complete control over the type family.

- If GHC defaults to `Either () ()`

- We can map it to `Bool`

<!--- pandoc -f markdown -t slidy -i -s --self-contained -o mypresentation.html Presentation.md --->

