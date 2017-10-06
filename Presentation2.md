# Philosophy

> “These types we write down they're not just names for data
> representations in memory, they're tags that queue in mathematical
> structures that **we exploit**.”
>
> — Conor McBride

- Construct functionality from small components

- 

# “Routine programming”

> `Applicative`s, `Traversable`s and `Monoid`s give us the basic
> building blocks for a lot of routine programming. Every
> `Applicative` *f* can be used to lift monoids, as follows
>
> ```haskell
> instance (Applicative f, Semigroup m) => Semigroup (f m) where
>   (<>) :: f m -> f m -> f m
>   (<>) = liftA2 (<>)
>
> instance (Applicative f, Monoid m) => Monoid (f m) where
>   mempty :: f m
>   mempty = pure mempty
> ```
>
> —Conor McBride


Idioms, IFunctors and Monoids give us the basic building blocks for a
lot of routine programming. Every =Idiom= /i/ can be used to lift
monoids, as follows

: instance (Idiom i, Monoid m) => Monoid (i o) where
:   mempty = liftA  mempty
:   (<>)   = liftA2 (<>)

although we shall have to choose individually the idioms /i/ for which
we apply this scheme. If we let =IO= lift monoids in this way, then we
acquire the sequential composition of commands for the price of the
trivial monoid:

: instance Monoid () where
:   mempty = ()
:   _ <> _ = ()


# Foo

## Getting up

- Turn off alarm
```haskell
qsort :: [Int] -> [Int]
qsort = \case
  []   -> []
  x:xs -> qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)
```

## Breakfast

- Eat eggs
- Drink coffee

# In the evening

## Dinner

- Eat spaghetti
- Drink wine

# First Slide, Section1

## First Slide, Section1, Subsection1

- first point
- second point

## First Slide, Section1, Subsection2

- first point in subsection2
- second point in subsection2

# First Slide, Section2

## First Slide, Section2, Subsection1

- first point
- second point

-----------------

## Second Slide, Section1

- first point
- second point

