# `deriving via` specification

This attempts to provide a relatively thorough examination of how `deriving via` (and other deriving strategies) are typechecked and the process by which they generate code. Although the focus is on `deriving via`, we will also draw examples from other deriving strategies when convenient.

# Typechecking

Our running example will be:

```haskell
data {instance} D a_1 ... a_p = ...
  deriving (C c_1 ... c_(z-1), ...)
           via (V v_1 ... v_k)
```

Where the class `C` is defined as:

```haskell
class C c_1 ... c_(z-1) c_z where
  type T1 t1_1 ... c_z ... t1_m
  ...
  type Ts ts_1 ... c_z ... ts_n

  m1 :: mty1_1 ... c_z ... mty1_o
  ...
  mf :: mtyf_1 ... c_z ... mtyf_p
```

(Note that `c_z` must appear in the types of `T1`, ..., `Ts`, `m1`, ..., and `m_f`.)

Typechecking begins by first typechecking the following types (in isolation):

1. The data definition of `D`
2. `C c_1 ... c_(z-1)`
3. `V v_1 ... v_k`

We must then validity-check several properties of `D` and `C`.

1. `C c_1 ... c_(z-1)` must have kind `k -> Constraint`.
2. The kind of `D`, the kind of `V v_1 ... v_k`, and the kind of the first argument in the kind of `C c_1 ... c_(z-1)` must all unify. For example, `data Foo = MkFoo deriving Functor` would not pass this check, as the kind of `Foo` is `*` but the first argument in the kind of `Functor` is `* -> *`.

The remaining validity checks concern properties of the eta-reduced types of `D`, which are determined by the first argument in the kind of `C c_1 ... c_(z-1)`. The number of types from `D` to eta reduce is computed by finding the number of kind arguments in the first argument in the kind of `C c_1 ... c_(z-1)`. Call this `n_eta`.

For instance, if `deriving Eq`, then `n_eta` = 0, since `Eq :: * -> Constraint`. Thus, `D` would not have to eta reduce any types in a derived `Eq` instance (i.e., and instance of the form `Eq (D a_1 ... a_p)` would be generated). If `deriving Functor`, however, then `n_eta = 1`, since `Functor :: (* -> *) -> Constraint`. Thus the last type of `D` would be eta reduced in a derived `Functor` instance (i.e., `Functor (D a_1 ... a_(p-1))` would be generated).

3. The eta-reduced types must all be type variables. For vanilla data types, this property always holds true, but it might not for data family instances. For instance, `data instance T a Int = ... deriving Functor` should be rejected, as we cannot eta reduce `Int`.
4. Moreover, the eta-reduced types must all be _distinct_. Once again, this can only happen with data family instances. We must reject something of the form `data instance S a a = ... deriving Functor`, for exaple, since `instance Functor (S a)` would match the type `S a b`, not `S a a` like we want.
5. No types in `C c_1 ... c_(z-1)` or `V v_1 ... v_k` can mention any of the eta-reduced types. For example, `newtype T a s = ... deriving (MonadState s)` must be rejected, as `instance MonadState s (T a)` would match the type `T a s1`, where `s1` is distinct from `s`, which is not what we want.

### A design question

An interesting consequence of the fact that the kind of `V v_1 ... v_k` must unify with the `k` in `C c_1 ... c_(z-1) :: k -> Constraint` means that the following will not be accepted:

```haskell
newtype MyMaybe a = MyMaybe (Maybe a)
  deriving (Show, Functor)
    via (WrappedApplicative Maybe a)
```

This is because the kind of `WrappedApplicative Maybe a` is `*`, but we have that `Functor :: (* -> *) -> Constraint`, and `*` does not unify with `* -> *`. Rather, `Show` and `Functor` must be derived with two separate (but very similar) `via` types:

```haskell
newtype MyMaybe a = MyMaybe (Maybe a)
  deriving Show
    via (WrappedApplicative Maybe a)
  deriving Functor
    via (WrappedApplicative Maybe)
```

This might seem tedious, but there's a good reason it has to be this way: `StandaloneDeriving`. After all, what should go in place of `???` in the following standalone-derived instance?

```haskell
deriving via (???) instance Functor MyMaybe
```

It certainly can't be `WrappedApplicative Maybe a`, because no variable named `a` is in scope! The only sensible choice here is `WrappedApplicative Maybe`, so for the sake of consistency, we adopt the same design consideration for `deriving` clauses.

### More `StandaloneDeriving` oddities

A little-known feature of type class instances is that they support explicit `forall` syntax:

```haskell
deriving instance forall a. Show a => Show (Foo a)
```

How does this mesh with standalone-derived instances using `via`? Not very neatly, as it happens:

```haskell
deriving via (Identity a) instance forall a. Show a => Show (Foo a)
```

Yuck—`a` is used before it's bound by a `forall`! I'm not sure how to make this better, though.

## Code generation

Provided that all of the validity checks above are met, GHC will generate the following instance:

```haskell
instance (C c_1 ... c_(z-1) (V v_1 ... v_k)) => C c_1 ... c_(z-1) (T a_1 ... a_p) where
  type T1 t1_1 ... (T a_1 ... a_p) ... t1_m = T1 t1_1 ... (V v_1 ... v_k) ... t1_m
  ...
  type Ts ts_1 ... (T a_1 ... a_p) ... ts_n = Ts ts_1 ... (V v_1 ... v_k) ... ts_n

  m1 = coerce @(mty1_1 ... (V v_1 ... v_k) ... mty1_o)
              @(mty1_1 ... (T a_1 ... a_p) ... mty1_o)
              m1
  ...
  mf = coerce @(mtyf_1 ... (V v_1 ... v_k) ... mtyf_p)
              @(mtyf_1 ... (T a_1 ... a_p) ... mtyf_p)
              mf
```

Generated code for associated type families uses the corresponding type family instance for the `via` type `V v_1 ... v_k`. Similarly, generated code for methods uses `coerce` to cast from the `C c_1 ... c_(z-1)` dictionary for `V v_1 ... v_k` to produce a new dictionary for `T a_1 ... a_p`.

## Typechecking the generated code

To typecheck this code, GHC must temporarily enable several language extensions (if they are not already enabled):

* `KindSignatures`: `instance (C c_1 ... c_(z-1) (V v_1 ... v_k)) => C c_1 ... c_(z-1) (T a_1 ... a_p)` may very well mention an explicit kind signature.
* `ScopedTypeVariables`: we need to ensure that the free variables in `coerce @(..) @(...)` are bound by `instance (C c_1 ... c_(z-1) (V v_1 ... v_k)) => C c_1 ... c_(z-1) (T a_1 ... a_p)`.
* `TypeApplications`: due to the visible type applications in `coerce @(...) @(...)`.
* `RankNTypes` and `ImpredicativeTypes`: to typecheck higher-rank uses of `coerce`, such as `coerce @(forall a b. (a -> b) -> f a -> f b) @(forall a b. (a -> b) -> Wrap f a -> Wrap f b)`. Don't be frightened by the use of `ImpredicativeTypes` here: it's a very safe subset of the extension.

GHC also temporarily disables the `RebindableSyntax` extension to avoid any subversive effects it may have on the generated code.

After doing all of this, the odds are high that the generated code itself will pass through GHC's typechecker unscathed. The only remaining risk is that `V v_1 ... v_k` will not be inter-`Coercible` with `T a_1 ... a_p`. In such a scenario, the user will be greeted with a (perhaps somewhat confusing) type error.
