Proposal title
==============

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

Here you should write a short abstract motivating and briefly summarizing the proposed change.


Motivation
==========
Give a strong reason for why the community needs this change. Describe the use case as clearly as possible and give an example. Explain how the status quo is insufficient or not ideal.


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

We propose a fourth deriving strategy, which uses the ``via`` keyword. Unlike
other deriving strategies, ``via`` requires specifying a type
(referred to as the ``via`` type) in addition to a derived class.
For instance, here is how one would use ``via`` in a ``deriving`` clause: ::

    newtype T = MkT Int
      deriving Monoid via (Sum Int)

Or in a standalone ``deriving`` declaration: ::

    deriving via (Sum Int) instance Show T

(TODO RGS: How much about the discrepancy between the two syntaxes should we
mention?)

As is the case with ``stock`` and ``anyclass``, the ``via`` identifier is
only treated specially in the context of ``deriving`` syntax. One will still
be able to use ``via`` as a variable name in other contexts, even if the
``DerivingVia`` extension is enabled.

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
``Int`` to ``Age`` using the ``coerce`` function from ``Data.Coerce``.
(TODO RGS: Provide a link here.)

This algorithm need only be tweaked slightly to describe how ``DerivingVia``
generates code. In ``GeneralizedNewtypeDeriving``:

1. We start with an instance for the representational type.
2. GHC ``coerce``s it to an instance for the newtype.

In ``DerivingVia``, however:

1. We start with an instance for a ``via`` type.
2. GHC ``coerce``s it to an instance for the data type.

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

    TODO RGS Data type example here

``DerivingVia`` only imposes the requirement that the generated code
typechecks. (See the "Typechecking generated code" section for more on this.)

Typechecking source syntax
--------------------------
TODO RGS

Typechecking generated code
---------------------------
TODO RGS

Effect and Interactions
=======================
Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features.

(TODO RGS: Should we just merge this with the previous section?)

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
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.


Alternatives
============
List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.


Unresolved questions
====================
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
===================
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?

TODOs
=====
* Add a link to the users' guide section on ``DerivingStrategies`` somewhere.
