% Stolen Instances Taste Sweet
% Baldur Blöndal / `@Iceland_jack`
% October, 2017

# I care about....

- ... avoiding **boilerplate**

    - free code

- ... capturing **patterns**

    - as a library (not as lore)

- ... big from small

- ... properties, not representation in memory

- ... being able to explore **quickly**
    
# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```
instance Monoid b => Monoid (a -> b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```
instance Monoid a => Monoid (IO a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns & Laws

Some yummy boilerplate:

```
instance (Applicative f, Monoid a) => Monoid (f a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

same behaviour for all types!

. . .

> `Applicative`s, `Traversable`s and `Monoid`s give us the basic
> building blocks for a lot of **routine programming**. Every
> `Applicative` *f* can be used to lift monoids, ...
>
> — Conor McBride

# “It's not so ba—.. oh.”

```
instance (Applicative f, Num a) => Num (f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance (Applicative f, Fractional a) => Fractional (f a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Applicative f, Floating a) => Floating (f a) where
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
```

# How do we capture this? 

`CPP`?

# What do Haskellers do ..
#### .. when they want different behavior? 

```
newtype App f a = App (f a)



```

We attach the “**routine programming**” pattern to `App`

```
instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty                    = App (pure   mempty)
  mappend (App fa) (App fb) = App (liftA2 mappend fa fb)
```

# What do Haskellers do ..
#### .. when they want different behavior? 

```
newtype App f a = App (f a)
  deriving newtype
    (Functor, Applicative)
```

We attach the “**routine programming**” pattern to `App`

```
instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty                    = pure mempty
  mappend                   = liftA2 mappend
```

- What is the in-memory representation of `App Maybe [a]`?

# `Coercible`

We can coerce between newtypes and the values they wrap,

```
instance Monoid a => Monoid (IO a) where
  mempty :: IO a
  mempty = coerce 
    (mempty  :: App IO a)

  mappend :: IO a -> IO a -> IO a
  mappend = coerce 
    (mappend :: App IO a -> App IO a -> App IO a)
```

# `{-# Language DerivingVia #-}`

This is where `DerivingVia` comes in

```
data IO a = ... 
  deriving Monoid 
    via (App IO a)
```

# `{-# Language DerivingVia #-}`

This is where `DerivingVia` comes in

```
data IO a = ... 
  deriving (Semigroup, Monoid)
    via (App IO a)
```

# `{-# Language DerivingVia #-}`

This is where `DerivingVia` comes in

```
data IO a = ... 
  deriving (Semigroup, Monoid, Num, Floating, Fractional)
    via (App IO a)
```

* But it doesn't stop there..

# Default Deriving / Vocabulary (**QuickCheck**)

**Problem**: There are many valid `Arbitrary` you can derive

```
arbitrarySizedIntegral        :: Integral a              => Gen a
arbitrarySizedFractional      :: Fractional a            => Gen a
arbitraryBoundedRandom        :: (Bounded a, Random a)   => Gen a
arbitraryBoundedEnum          :: (Bounded a, Enum a)     => Gen a
```

* We can codify each of them as an instance 

    ```
    newtype BoundedRandom a = BR a
    
    instance (Bounded a, Random a) => Arbitrary (BoundedRandom a) where
      arbitrary = BR <$> arbitraryBoundedRandom
    ```

* `DerivingVia` lets you pick at deriving site

    ```
    data Foo = ... deriving (Arbitrary)
      via (BoundedRandom Foo)

    instance Bounded Foo where ..
    instance Random  Foo where ..
    ```

# More **QuickCheck**

```




```

**QuickCheck** has many modifiers

```
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
# More **QuickCheck**

```
newtype NonEmptyList a = NonEmpty [a] -- Only generates non-empty lists



```

**QuickCheck** has many modifiers

```
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

# More **QuickCheck**

```
newtype NonEmptyList a = NonEmpty [a] -- Only generates non-empty lists
newtype Positive     a = Positive a   -- x > 0


```

**QuickCheck** has many modifiers

```
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

# More **QuickCheck**

```
newtype NonEmptyList a = NonEmpty [a] -- Only generates non-empty lists
newtype Positive     a = Positive a   -- x > 0
newtype Large        a = Large    a   -- large numbers
```

**QuickCheck** has many modifiers

```
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

    ```
    instance (Applicative f, Monoid a) => Monoid (f a) where
      mempty  = pure   mempty
      mappend = liftA2 mappend

    instance Alternative f => Monoid (f a) where
      mempty  = zero
      mappend = (<|>)
    ```

- Get reified as instances

    ```
    instance (Applicative f, Monoid a) => Monoid (App f a)
    instance (Alternative f)           => Monoid (Alt f a)
    ```

# Mention JSON deriving shortly

* **Problem**: Only *one* default method.

* **Problem**: What if the default method changes?

* **Problem**: What if you want to derive with options?

    ```haskell
    data State = State
      { scbColor      :: Maybe Double 
      , scbBrightness :: Maybe Double
      } 
      deriving ToJSON 
        via (AesonOptions OmitNothingField (StripPrefix "scb") State)
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

    - Groups patterns

- Any isomorphism (`Generic`, `Representable`) lets us carry over structure 

    - From representation

    - From anything sharing a representation

- Prefer `Monoidal` formulation of `Applicative`? Sorted m8.

# The End

* Thank You

# Isomorphic Structures

- So far: only used instances of coercible structures

- ## `GHC.Generics`

- ## `Iso1`


- No actually

# Associated type family

```haskell
class Functor f => Representable f where
  type Rep f :: Type

  index    :: f a -> (Rep f -> a)
  tabulate :: f a <- (Rep f -> a)
```

gives us `Applicative`, `Monad`, `Distributive`, `MonadReader (Rep f)`, `FunctorWithIndex`, `FoldableWithIndex`, `TraversableWithIndex` ....

- And sometimes `Comonad`

    ```
    instance (Representable f, Monoid (Rep f)) => Comonad (Co f)
    ```

# 

- First derive `Representable Pair` for `Pair = Either () ()`.

- We get a representation `Either () ()`

- This is `Bool` but we can call it `newtype PairRep = PRep (Either () ())`

- > "Okay but we cannot coerce from `PairRep` to `Bool`, checkmate

<!--- pandoc -f markdown -t slidy -i -s --self-contained -o mypresentation.html Presentation.md --->

