% Stolen Instances Taste Sweet
% Baldur Blöndal
% September, 2017

# Content

- READ SIMON'S https://www.cis.upenn.edu/~sweirich/icfp-plmw15/slides/peyton-jones.pdf

# How we think

- “It’s good to think of **types**
- as not just the **representation** of data
- but the equipment of **data with structure**.”
-
- — Conor McBride

We think of 

# Rant about `newtype`s

- `coerce :: Coercible a b => a -> b` **safely** coerces between same in memory
    - Is a **NOOP**

- `newtype USD = USD Int` 

- `coerce :: Int -> USD`

- `coerce :: [[[(Int, USD)]]] -> [[[(USD, Int)]]]`

- **TODO** Re-read Richard's paper

- ### Thinking of the same "representation" through different lenses

    #### Reducing boilerplate

- ##### Capturing a pattern

# How we think 


- Types each have their own flavour, their own personality

- Adorning them with newtypes lets you tailor their personalities like in Sims

    - "add a bit of this"


- Reducing boilerplate / capturing a pattern / 

# Mention JSON deriving shortly

# Associated type family

- First derive `Representable Pair` for `Pair = Either () ()`.

- We get a representation `Either () ()`

- This is `Bool` but we can call it `newtype PairRep = PRep (Either () ())`

- > "Okay but we cannot coerce from `PairRep` to `Bool`, checkmate

- No actually

# Isomorphic Structures

- So far: only used instances of coercible structures

- 

- ## `GHC.Generics`

- ## `Iso1`

<!--- pandoc -f markdown -t slidy -i -s --self-contained -o mypresentation.html Presentation.md --->

