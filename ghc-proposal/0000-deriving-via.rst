Deriving Via
============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

We propose ``DerivingVia``, a new
`deriving strategy <https://downloads.haskell.org/~ghc/8.4.1/docs/html/users_guide/glasgow_exts.html#extension-DerivingStrategies>`_
that significantly increases the expressive power of Haskell's ``deriving`` construct.

One example of a use case for this is the following: ::

    newtype T = MkT Int
      deriving Monoid via (Sum Int)

One can interpret this as "derive a ``Monoid`` instance for ``T`` by *reusing*
the existing ``Monoid`` instance for ``Sum Int``".

The paper `Deriving Via; or, How to Turn Hand-Written Instances into an Anti-Pattern
<https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf>`_ describes this feature
in great detail, with many accompanying examples, and so is a primary reference source
for this proposal.

There is a prototype implementation already available in our GHC fork
`here <https://github.com/RyanGlScott/ghc/tree/deriving-via-8.5>`_.

Motivation
==========
TODO: Baldur

Here is a list of other sources about this idea:

* The `original blog post <https://gist.github.com/Icelandjack/d258b88a0e0b3be2c0b3711fdd833045>`_ proposing this idea, and the `accompanying Reddit discussion <https://www.reddit.com/r/haskell/comments/6ksr76/rfc_part_1_deriving_instances_of/>`_.

* A `Reddit post <https://www.reddit.com/r/haskell/comments/8aa81q/deriving_via_or_how_to_turn_handwritten_instances/>`_ discussing the paper.

Proposed Change Specification
=============================
Specify the change in precise, comprehensive yet concise language. Avoid words like should or could. Strive for a complete definition. Your specification may include,

* grammar and semantics of any new syntactic constructs
* the types and semantics of any new library interfaces
* how the proposed change interacts with existing language or compiler features, in case that is otherwise ambiguous

Note, however, that this section need not describe details of the implementation of the feature. The proposal is merely supposed to give a conceptual specification of the new feature and its behavior.

Language extension name
-----------------------
We propse a new language extension, ``DerivingVia``. ``DerivingVia`` will imply
``DerivingStrategies``.

Syntax changes
--------------
Currently, there are three deriving strategies in GHC: ``stock``, ``newtype``,
and ``anyclass``. For example, one can use the ``stock`` strategy in a
``deriving`` clause like so: ::

    data Foo = MkFoo
      deriving stock Eq

Or in a standalone ``deriving`` declaration: ::

    deriving stock instance Eq Foo

We propose a fourth deriving strategy, which requires enabling the
``DerivingVia`` extension to use. This deriving strategy is indicated by using
the ``via`` keyword. Unlike other deriving strategies, ``via`` requires
specifying a type (referred to as the ``via`` type) in addition to a derived
class. For instance, here is how one would use ``via`` in a ``deriving``
clause: ::

    newtype T = MkT Int
      deriving Monoid via (Sum Int)

Or in a standalone ``deriving`` declaration: ::

    deriving via (Sum Int) instance Monoid T

As is the case with ``stock`` and ``anyclass``, the ``via`` identifier is
only treated specially in the context of ``deriving`` syntax. One will still
be able to use ``via`` as a variable name in other contexts, even if the
``DerivingVia`` extension is enabled.

Note that in ``deriving`` clauses, we put the ``via`` keyword *after* the
derived class instead of before it. We do so primarly because we find it
makes the distinction between the derived class and the ``via`` type more
obvious. If we had put the ``via`` type *before* the derived class, as
in the following two examples: ::

    deriving via X (Y Z)
    deriving via (X Y) Z

Then the distinction is harder to see from a glance.

Code generation
---------------
The process by which ``DerivingVia`` generates instances is a strict
generalization of ``GeneralizedNewtypeDeriving``. For instance, the
following ``Age`` newtype, which has an underlying representation type
of ``Int``: ::

    newtype Age = MkAge Int
      deriving newtype Enum

Would generate the following instance: ::

    instance Enum Age where
      toEnum   = coerce @(Int -> Int)   @(Int -> Age)   toEnum
      fromEnum = coerce @(Int -> Int)   @(Age -> Int)   fromEnum
      enumFrom = coerce @(Int -> [Int]) @(Age -> [Age]) enumFrom
      ...

Here, each method of ``Enum`` is derived by taking the implementation of
the method in the ``Enum Int`` instance and coercing all occurrences of
``Int`` to ``Age`` using the ``coerce`` function from
`Data.Coerce <http://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Coerce.html>`_.

This algorithm need only be tweaked slightly to describe how ``DerivingVia``
generates code. In ``GeneralizedNewtypeDeriving``:

1. We start with an instance for the representational type.
2. GHC coerces it to an instance for the newtype.

In ``DerivingVia``, however:

1. We start with an instance for a ``via`` type.
2. GHC coerces it to an instance for the data type.

For instance, this earlier example: ::

    newtype T = MkT Int
      deriving Monoid via (Sum Int)

Would generate the following instance: ::

    instance Monoid T where
      mempty  = coerce @(Sum Int) @T mempty
      mappend = coerce @(Sum Int -> Sum Int -> Sum Int)
                       @(T       -> T       -> T)
                       mappend

To make it evident that ``DerivingVia`` is in fact a generalization of
``GeneralizedNewtypeDeriving``, note that this: ::

    newtype Age = MkAge Int
      deriving newtype Enum

Is wholly equivalent to this: ::

    newtype Age = MkAge Int
      deriving Enum via Int

Note that while ``GeneralizedNewtypeDeriving`` has a strict requirement that
the data type for which we're deriving an instance must be a newtype, there
is no such requirement for ``DerivingVia``. For example, this is a perfectly
valid use of ``DerivingVia``: ::

    newtype BoundedEnum a = BoundedEnum a
    instance (Bounded a, Enum a) => Arbitrary (BoundedEnum a) where ...

    data Weekday = Mo | Tu | We | Th | Fr | Sa | Su
      deriving (Enum, Bounded)
      deriving Arbitrary via (BoundedEnum Weekday)

``DerivingVia`` only imposes the requirement that the generated code
typechecks. (See the "Typechecking generated code" section for more on this.)

Renaming source syntax
----------------------
``DerivingVia`` introduces a new place where types can go (the ``via`` type),
and as a result, introduces a new place where type variables can be bound. To
understand how this works, consider the following example that uses a
``deriving`` clause: ::

    data Foo a = ...
      deriving (Baz a b c) via (Bar a b)

* ``a`` is bound by ``Foo`` itself in the declaration ``data Foo a``.
  ``a`` scopes over both the ``via`` type, ``Bar a b``,
  as well as the derived class, ``Baz a b c``.
* ``b`` is bound by the ``via`` type ``Bar a b``. Note that ``b`` is bound
  here but ``a`` is not, as it was bound earlier by the ``data`` declaration.
  ``b`` also scopes over the derived class ``Baz a b c``.
* ``c`` is bound by the derived class ``Baz a b c``, as it was not bound
  earlier.

For ``StandaloneDeriving``, the scoping works similarly.
In the following example: ::

    deriving via (V a) instance C a (D a b)

* ``a`` is bound by the ``via`` type ``V a``, and scopes over the instance
  type ``C a (D a b)``.
* ``b`` is bound the instance type ``C a (D a b)``, as it was not bound
  earlier.

Note that ``DerivingVia`` requires that all type variables bound by a ``via``
type must be used in each derived class (for ``deriving`` clauses) or
in the instance type (for ``StandaloneDeriving``). If a ``via`` type binds
a type variable and does not use it accordingly, then it is *floating*,
and rejected with an error. To see why this is the case, consider the
following example: ::

  data Quux
    deriving Eq via (Const a Quux)

This would generate the following instance: ::

  instance Eq Quux where
    (==) = coerce @(Quux         -> Quux         -> Bool)
                  @(Const a Quux -> Const a Quux -> Bool)
                  (==)
    ...

This instance is ill-formed, as the ``a`` in ``Const a Quux`` is unbound! One
could conceivably "fix" this by explicitly quantifying the ``a`` at the top
of the instance: ::

  instance forall a. Eq Quux where ...

But this would not be much better, as now the ``a`` is ambiguous. We avoid
these complications by making floating type variables in ``via`` types an
explicit error.

Typechecking source syntax
--------------------------
In this example: ::

  newtype Age = MkAge Int
    deriving Eq

GHC requires that the kind of the argument to the class must unify with the
kind of the data type. (In this example, both of these kinds are ``Type``, so
it passes this check.) This is done to ensure that the generated code makes
sense. For instance, one could not derive ``Functor`` for ``Age``, as the
kind of the argument to ``Functor`` is ``Type -> Type``, which does not
unify with ``Age``'s kind (``Type``).

``DerivingVia`` extends this check ever-so-slightly. In this example: ::

  newtype Age = MkAge Int
    deriving Eq via (Sum Int)

Not only must the kind of the argument to ``Eq`` unify with the kind of
``Age``, it must also be the case that those two kinds unify with the kind
of the ``via`` type, ``Sum Int``. (``Sum Int :: Type``, so it passes that
check.)

``DerivingVia`` also supports higher-kinded scenarios, such as: ::

  newtype I a = MkI a
    deriving Functor via Identity

For more details on how this featurette works, refer to Section 3.1.2
of `the paper <https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf>`_.

Typechecking generated code
---------------------------
Once ``DerivingVia`` generates instances, they are fed back into GHC's
typechecker as one final sanity check. In order for the generated code to
typecheck, the original data type and the ``via`` type must have the same
runtime representations. The use of ``coerce`` is what guarantees this.

For instance, if a user tried to derive ``via`` a type that was not
representationally equal to the original data type, as in this example: ::

    newtype UhOh = UhOh Char
      deriving Ord via Int

Then GHC will give an error message stating as such: ::

    • Couldn't match representation of type ‘Char’ with that of ‘Int’
        arising from the coercion of the method ‘compare’
          from type ‘Int -> Int -> Ordering’
            to type ‘UhOh -> UhOh -> Ordering’
    • When deriving the instance for (Ord UhOh)

Fortunately, GHC has invested considerable effort into making error messages
involving ``coerce`` easy to understand, so ``DerivingVia`` benefits from this
as well.

Effect and Interactions
=======================
Other ``deriving``-related language extensions, such as
``GeneralizedNewtypeDeriving`` and ``DeriveAnyClass``, are selected
automatically in certain cases, even without the use of explicit ``newtype``
or ``anyclass`` deriving strategy keywords. This is not the case with
``DerivingVia``, however. One *must* use the ``via`` keyword to make use of
``DerivingVia``. That is to say, GHC will never attempt to guess a ``via``
type, making this extension strictly opt-in.

As a result, ``DerivingVia`` has the nice property that it is orthogonal to
other language features. No existing code will break because of
``DerivingVia``, as programmers must consciously choose to make use of it.

Costs and Drawbacks
===================
There are currently no known drawbacks to this feature. Implementing this
feature was a straightforward extension of the machinery already in place
to support ``deriving``, so it will not impose significant maintenance costs.
(Moreover, the maintainer of this part of the codebase,
`@RyanGlScott <https://github.com/RyanGlScott>`_, is also the person who wrote
much of the code for ``DerivingVia``.)

Alternatives
============
The closest existing alternatives to this feature are various preprocessor hacks
that people have cooked up to "copy-and-paste" code patterns in various places,
such as in Conal Elliott's
`applicative-numbers <http://hackage.haskell.org/package/applicative-numbers>`_
package. But this is far from a satisfying solution to the problem.

Unresolved questions
====================

Implementation Plan
===================
There is feature is fully implemented in our GHC fork
`here <https://github.com/RyanGlScott/ghc/tree/deriving-via-8.5>`_. I
(`@RyanGlScott <https://github.com/RyanGlScott>`_) volunteer to work to get
this fork into GHC proper.
