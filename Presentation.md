% Stolen Instances Taste Sweet
% Baldur Blöndal
% September, 2017

# Content

# Rant about `newtype`s

- `newtype`s let us 

- *TODO* Discuss `coerce`

- *TODO* Re-read Richard's paper

- ### Thinking of the same "representation" through different lenses

    #### Reducing boilerplate

- ##### Capturing a pattern

# How we think 

- “It’s good to think of types 
- as not just the representation of data
- but the equipment of data with structure.”
-
- — Conor McBride

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

