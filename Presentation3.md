% Stolen Instances Taste Sweet
% Baldur Blöndal
% September, 2017

# Content

- READ SIMON'S https://www.cis.upenn.edu/~sweirich/icfp-plmw15/slides/peyton-jones.pdf

# I'll tell you what I care about....

- I care about capturing patterns (capturing them in a library and reuse reuse reuse)

- I care about laws (that I don't have to prove..)

- I care about class instances (that I don't have to write..)

- I care about being precise

# Boilerplate & Patterns

A nice example of boilerplate:

```
instance Monoid a => Monoid ((->) a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns

A nice example of boilerplate:

```
instance Monoid a => Monoid (IO a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns

A nice example of boilerplate:

```
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

# Boilerplate & Patterns

A nice example of boilerplate:

```
instance (Applicative f, Monoid a) => Monoid (f a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
```

Works for any `Applicative`.. 

> `Applicative`s, `Traversable`s and `Monoid`s give us the basic
> building blocks for a lot of **routine programming**. Every
> `Applicative` *f* can be used to lift monoids, ...
>
> — Conor McBride

# The Name is the Thing

### What do Haskellers do ..
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

# The Name is the Thing

### What do Haskellers do ..
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
# The Name is the Thing

### What do Haskellers do ..
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

# QuickCheck

```
arbitrarySizedIntegral   :: Integral a => Gen a
arbitrarySizedFractional :: Fractional a => Gen a
Generates a fractional number. The number can be positive or negative and its maximum absolute value depends on the size parameter.

arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a
Source
#

Generates an integral number from a bounded domain. The number is chosen from the entire range of the type, but small numbers are generated more often than big numbers. Inspired by demands from Phil Wadler.

arbitraryBoundedIntegral :: (Bounded a, Integral a) => Gen a
Source
#

Generates an integral number. The number is chosen uniformly from the entire range of the type. You may want to use arbitrarySizedBoundedIntegral instead.

arbitraryBoundedRandom :: (Bounded a, Random a) => Gen a
Source
#

Generates an element of a bounded type. The element is chosen from the entire range of the type.

arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
Source
#

Generates an element of a bounded enumeration.

# Foo

- **QuickCheck** has many modifiers

```
newtype NonEmptyList a = NonEmpty [a]

-- Only generates non-empty lists
instance Arbitrary a => Arbitrary (NonEmptyList a) 
```

# 

- **QuickCheck** has many modifiers

```
newtype NonEmptyList a = NonEmpty [a]  -- not . null
newtype Positive     a = Positive a    -- x > 0
newtype Large        a = Large    a    -- large numbers
```

If we have our own 



# How we think

- > “It’s good to think of **types**
- > as not just the **representation** of data
- > but the equipment of **data with structure**.”
- >
- > — Conor McBride

We think of

# Rant about `newtype`s

- `coerce :: Coercible a b => a -> b` **safely** coerces between same in memory
    - Is a **NOOP**
    - Nests

- `newtype USD = USD Int` 

- `coerce :: Int -> USD`

- `coerce :: [[[(Int, USD)]]] -> [[[(USD, Int)]]]`

- **TODO** Re-read Richard's paper

# How we think 

- ### Thinking of the same "representation" through different lenses

    #### Reducing boilerplate

- ##### Capturing a pattern

- Types each have their own flavour, their own personality

- Adorning them with newtypes lets you tailor their personalities like in Sims

    - "add a bit of this"

- Reducing boilerplate / capturing a pattern / 

# The Main Idea

- The same representation can carry different **structure**

- `coerce` lets us freely transfer **structure** between types with the same representation

- Zero cost

- Safely

- When defining a `newtype`: We have full control

    - We can replace associated type families with our own, passed as arguments by `newtype` or computed
    
    - 

# Mention JSON deriving shortly

# Associated type family

- First derive `Representable Pair` for `Pair = Either () ()`.

- We get a representation `Either () ()`

- This is `Bool` but we can call it `newtype PairRep = PRep (Either () ())`

- > "Okay but we cannot coerce from `PairRep` to `Bool`, checkmate

- No actually

# Isomorphic Structures

- So far: only used instances of coercible structures

- ## `GHC.Generics`

- ## `Iso1`

# Uses

## Getting rid of boilerplate 

## Avoids default methods

### Not tied to a class declaration 

### Arbitrarily many

## If you implement 

## Capturing patterns `instance (Applicative f, Monoid m) => Applicative (f m)`

## Any isomorphism (=Generic=, =Representable=) lets us carry over structure 

### From representation

### From anything sharing a representation

<!--- pandoc -f markdown -t slidy -i -s --self-contained -o mypresentation.html Presentation.md --->

