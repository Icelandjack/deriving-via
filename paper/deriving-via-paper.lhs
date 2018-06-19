\documentclass[%
  %format=acmsmall,% 1-column format used by PACMPL
  format=sigplan,% 2-column format used by other SIGPLAN conferences
  review=true,% review mode / enables line numbers
  anonymous=false,% enable to remove author names
  timestamp=true,% adds timestamps to the pages
  %authordraft=true,% development mode
  ]{acmart}

% disable the watermark
% \SetWatermarkText{}%

\usepackage{booktabs}
\usepackage{hyperref}
\usepackage{xspace}
\usepackage[silent]{polytable}

%include general.fmt

% macros
\newcommand\extension[1]{\Conid{#1}}
\newcommand\DerivingVia{Deriving Via\xspace}
\newcommand\GND{{\smaller GND}\xspace}
\newcommand\GHC{{\smaller GHC}\xspace}
\newcommand\DefaultSignatures{default signatures\xspace}
\newcommand\StandaloneDeriving{|StandaloneDeriving|\xspace}
\newcommand\Package[1]{\textsf{#1}}
\newcommand\QuickCheck{\Package{QuickCheck}\xspace}

% comments
%let comments = True
%if comments
\colorlet{bbnote}{blue}
\colorlet{alnote}{orange}
\colorlet{rsnote}{red}
\newcommand\bbnote[1]{\footnote{\color{bbnote}[BB: #1]}}
\newcommand\alnote[1]{\footnote{\color{alnote}[AL: #1]}}
\newcommand\rsnote[1]{\footnote{\color{rsnote}[RS: #1]}}
%else
\newcommand\bbnote[1]{}%
\newcommand\alnote[1]{}%
\newcommand\rsnote[1]{}%
%endif

\setcopyright{none}

% Data to be filled in later:

%\acmDOI{...}
%\acmISBN{...}
\acmConference[Submitted to Haskell]{Haskell Symposium}{09/2018}{St.~Louis, MO, USA}
%\acmYear{...}
%\copyrightyear{...}
%\acmPrice{...}

%if style == newcode

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DefaultSignatures #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE DerivingStrategies #-}
> {-# LANGUAGE DerivingVia #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> import Control.Applicative
> import Control.Monad
> import Control.Monad.Identity
> import Control.Monad.ST
> import Data.Coerce
> import Data.Kind
> import Data.Proxy
> import GHC.Generics hiding (C, C1, Rep)
> import qualified GHC.Generics as GHC
> import GHC.TypeLits
> import Test.QuickCheck hiding (NonNegative, Large)
> import qualified Test.QuickCheck as QC

> main :: IO ()
> main = return ()

%endif

%if style /= newcode
%format Monoid2 = Monoid
%format mempty2 = mempty
%format mappend2 = mappend
%format Monoid3 = Monoid
%format mempty3 = mempty
%format mappend3 = mappend
%format Monoid4 = Monoid
%format mempty4 = mempty
%format mappend4 = mappend
%format overlapping (x) = x
%format Constraint = "\ki{Constraint}"
%format TYPE = "\ki{*}"
%format DOTS = "\textrm{\dots} "
%format DDOTS = DOTS
%else
%format TYPE = "*"
%format DOTS = undefined
%format DDOTS =

> class Monoid2 m where
>
>   mempty2 :: m
>   mappend2 :: m -> m -> m

> class Monoid3 m where
>
>   mempty3 :: m
>   mappend3 :: m -> m -> m

> class Monoid4 m where
>
>   mempty4 :: m
>   mappend4 :: m -> m -> m

%format overlapping (x) = " {-# OVERLAPPING #-} " x
%endif

\begin{document}

\title{Deriving Via}
\subtitle{or, How to Turn Hand-Written Instances into an Anti-Pattern}
\author{Baldur Blöndal}
\affiliation{
}
\author{Andres Löh}
\affiliation{
  \institution{Well-Typed LLP}
}
\author{Ryan Scott}
\affiliation{
  \institution{Indiana University}
}

\begin{abstract}

Haskell's |deriving| construct is a cheap and cheerful way to quickly generate
instances of type classes that follow common patterns. But at present, there
is only a subset of such type class patterns that |deriving| supports, and
if a particular class lies outside of this subset, then one cannot derive it
at all, with no alternative except for laboriously declaring the instances by hand.

To overcome this deficit, we introduce \DerivingVia, an extension to |deriving|
that enables programmers to compose instances from named programming
patterns, thereby turning |deriving| into a high-level domain-specific language for
defining instances. \DerivingVia leverages newtypes---an already
familiar tool of the Haskell trade---to declare recurring patterns in a
way that both feels natural and allows a high degree of abstraction.

% In this paper, we describe how to capture such rules as Haskell
% newtypes, and we introduce \DerivingVia, a new language extension
% that allows us to use |deriving| on any instance that can be
% constructed using a rule. In this way, we
% vastly increase the fraction of type classes for which we
% can use |deriving|. This not only saves work, but also explains
% the intention behind the code better, as we can give names to
% recurring patterns.

% Haskell instance declarations fall into one of two categories:
% either we can use the |deriving| construct and get the instance
% generated for us for free, or we have to write the instance by
% hand, providing explicit implementations of the class methods.
% There is nothing in between.
%
% Many instances, however, can be defined for a reason, and that
% reason can be captured as a program. There might be a general
% rule that if a type is an instance of some classes, it can be
% made an instance of another class. Or there might be a rule that
% says that if we can define a class instance in a particular way,
% we can also define an instance of the same class in a slightly
% different way.
%
% In this paper, we describe how to capture such rules as Haskell
% newtypes, and we introduce \DerivingVia, a new language extension
% that allows us to use |deriving| on any instance that can be
% constructed using a rule. In this way, we
% vastly increase the fraction of type classes for which we
% can use |deriving|. This not only saves work, but also explains
% the intention behind the code better, as we can give names to
% recurring patterns.

\end{abstract}

% CCSXML to be inserted later:
%
% \begin{CCSXML}
% \end{CCSXML}
%
% \ccsdesc[...]{...}
%
% \keywords{...}

\maketitle

% ``These types we write down they're not just names for data
% representations in memory, they're tags that queue in mathematical
% structures that we exploit.''\footnote{Taken from unknown position:
% https://www.youtube.com/watch?v=3U3lV5VPmOU}

\vspace*{\bigskipamount}

\section{Introduction}
%if style /= newcode
%format via = "\keyw{via}"
%format Foo = "\ty{Foo}"
%format MkFoo = "\con{Foo}"
%format Flip = "\ty{Flip}"
%format Monoid = "\cl{Monoid}"
%format Semigroup = "\cl{Semigroup}"
%format mempty = "\id{mempty}"
%format mappend = "\id{mappend}"
%format liftA = "\id{liftA}"
%format liftA2 = "\id{liftA2}"
%format empty = "\id{empty}"
%format Alternative = "\cl{Alternative}"
%format Wrap = "\ty{Wrap}"
%format Wrap1 = "\ty{Wrap1}"
%format MkWrap = "\con{Wrap}"
%format MkWrap1 = "\con{MkWrap}"
%format App = "\ty{App}"
%format Alt = "\ty{Alt}"
%format MkApp = "\con{MkApp}"
%format MkAlt = "\con{MkAlt}"
%format Endo = "\ty{Endo}"
%format MkEndo = "\con{MkEndo}"
%format coerce = "\id{coerce}"
%format ap = "\id{ap}"
%format ST = "\ty{ST}"
%endif

In Haskell, type classes capture common interfaces. When defining
class instances, we often discover repeated patterns where different
instances have the same definition. For example, the following
instances appear in the \Package{base} library of
the Glasgow Haskell Compiler (\GHC):

> instance Monoid a => Monoid2 (IO a) where
>   mempty2   =  pure mempty
>   mappend2  =  liftA2 mappend
>
> instance Monoid a => Monoid2 (ST s a) where
>   mempty2   =  pure mempty
>   mappend2  =  liftA2 mappend

These have completely identical instance bodies. The underlying pattern works
not only for |IO| and |ST s|, but for any applicative functor~|f|.

\pagebreak

It is tempting to avoid this obvious repetition by defining an
instance for all such types in one fell swoop:

> instance (Applicative f, Monoid a)
>   => Monoid2 (f a) where
>   mempty2   =  pure mempty
>   mappend2  =  liftA2 mappend

Unfortunately, this general instance is undesirable as it overlaps
with all other |(f a)|-instances. Instance resolution will match the
instance head first before considering the context, whether |f| is
applicative or not. Once \GHC has commited to an instance, it will never
backtrack. Consider:

> newtype Endo a = MkEndo (a -> a) -- Data.Monoid

Here, |Endo| is not an applicative functor, but it still admits a perfectly valid
|Monoid| instance that overlaps with the general instance above:

> instance overlapping (Monoid2 (Endo a)) where
>   mempty2 = MkEndo id
>   mappend2 (MkEndo f) (MkEndo g) = MkEndo (f . g)

Moreover, even if we have an applicative functor~|f| on our hands, there is no
guarantee that this is the definition we want. Notably, lists are the
\emph{free monoid} (i.e, the most `fundamental' monoid) but that instance
does not coincide with the rule above and in particular, imposes no
|(Monoid a)| constraint:

> instance Monoid2 [a] where
>   mempty2   =  []
>   mappend2  =  (++)

In fact, the monoid instance for lists is captured by a
\emph{different} rule based on |Alternative|:

> instance Alternative f => Monoid3 (f a) where
>   mempty3   =  empty
>   mappend3  =  (<|>)

Because instance resolution never backtracks, we cannot define these two
distinct rules for~|Monoid (f a)| at the same time, even with overlapping instances.

% (TODO) It is worth noting that the monoid instance for |(Endo a)| is captured by a slightly different rule based on |Category|:
%
% < instance Category cat => Monoid3 (cat a a) where
% <   mempty3  =  id
% <   mempty3  =  (.)

The only viable workaround using the Haskell type class system is to
write the instances for each data type by hand, each one with an
identical definition (like the instances for |IO a| and |ST s a|),
which is extremely unsatisfactory:

\pagebreak

\begin{itemize}
\item It is not obvious that we are instantiating a general principle.
\item Because the general principle is not written down in code
  with a name and documentation, it has to be communicated through folklore
  or in comments and is difficult to discover and search for. Our code
  has lost a connection to its origin.
\item There are many such rules, some quite obvious, but
  others more surprising and easy to overlook.
\item While the work required to define instances manually for
  |Monoid|---which only has two methods---is perhaps acceptable,
  it quickly becomes extremely tedious and error-prone for classes
  with many methods.
\end{itemize}

As an illustration of the final point, consider |Num|. There is a way
to lift a |Num| instance through any applicative
functor:\footnote{Similarly for |Floating| and |Fractional|, numeric type
classes with a combined total of 25 methods (15 for a minimal
definition).}

%{
%if style == newcode
%format Num = Num2
%format + = .+
%format - = .-
%format * = .*
%format negate = negate2
%format abs = abs2
%format signum = signum2
%format fromInteger = fromInteger2

> class Num a where
>   (+) :: a -> a -> a
>   (-) :: a -> a -> a
>   (*) :: a -> a -> a
>   negate :: a -> a
>   abs :: a -> a
>   signum :: a -> a
>   fromInteger :: Integer -> a

%endif

> instance (Applicative f, Num a) => Num (f a) where
>   (+)  =  liftA2 (+)
>   (-)  =  liftA2 (-)
>   (*)  =  liftA2 (*)
>
>   negate   =  liftA negate
>   abs      =  liftA abs
>   signum   =  liftA signum
>
>   fromInteger = pure . fromInteger

%}
Defining such boilerplate instances manually for concrete type constructors
is so annoying that Conal Elliott introduced a preprocessor~\cite{applicative-numbers}
for this particular use case several years ago.
% \alnote{And Conal is by no means alone: see
% https://gist.github.com/Icelandjack/e1ddefb0d5a79617a81ee98c49fbbdc4\#a-lot-of-things-we-can-find-with-define
% We cannot put a gist dump like this into a paper. We might want to make a selection,
% or just describe the situation in words.}

\subsection{Deriving}

Readers familiar with Haskell's deriving mechanism may wonder why we
cannot simply derive all the instances we just discussed.
Unfortunately, our options are very limited.

To start, |Monoid| is not one of the few blessed type classes that GHC has
built-in support to derive. It so happens that |(IO a)|, |(ST s a)|
and |(Endo a)| are all newtypes, so they are in principle eligible for
\emph{generalized newtype deriving} (\GND), in which their instances
could be derived by reusing the instances of their underlying
types~\cite{zero-cost-coercions}. However, this would give us the
wrong definition in all three cases.
% (even if it worked for the first two, which it
% doesn't).

Our last hope is that the the |Monoid| type class has a suitable
generic default implementation~\cite{gdmfh}. If that were the case,
we could use a deriving clause in conjunction with the
@DeriveAnyClass@ extension, and thereby get the compiler to generate
an instance for us.

However, there is no generic default for |Monoid|, a standard class from the
\Package{base} library (which would be difficult to change). But even if
a generic instance existed, it would still capture a \emph{single}
rule over all others, so we couldn't ever use it to derive both the
monoid instance for lists and that for \mbox{|ST s a|}.

We thus have no other choice but to write some
instances by hand. This means that we have to provide explicit
implementations of at least a minimal subset of the class methods.
There is no middle ground here, and the additional work required
compared to |deriving| can be
drastic---especially if the class has many methods---so the option of
using |deriving| remains an appealing alternative.

\subsection{Introducing \DerivingVia}\label{sec:introducingdv}

We are now going to address this unfortunate lack of abstraction
and try to bridge the gap between manually defined instances and
the few available |deriving| mechanisms we have at our disposal.

Our approach has two parts:
\begin{enumerate}
\item We capture general rules for defining new instances using
  newtypes.
\item We introduce \DerivingVia, a new language construct
  that allows us to use such newtypes to explain to the compiler
  exactly how to construct the instance without having to write
  it by hand.
\end{enumerate}

As a result, we are no longer limited to a fixed set of predefined
ways to define particular class instances, but can instead teach the
compiler new rules for deriving instances, selecting the one we want
using a high-level description.
%if style /= newcode
%format App = "\ty{App}"
%format Alt = "\ty{Alt}"
%format MkApp = "\con{App}"
%format MkAlt = "\con{Alt}"
%endif

Let us look at examples.
For the \emph{first part},
we revisit the rule that explains how to lift a monoid
instance through an applicative functor. We can turn the problematic
generic and overlapping instance for |Monoid (f a)| into an entirely
unproblematic instance by defining a suitable adapter
newtype~\cite{iterator-pattern} and wrapping
the instance head in it:

> newtype App f a = MkApp (f a)
>
> instance (Applicative f, Monoid4 a)
>   => Monoid4 (App f a) where
>   mempty4 = MkApp (pure mempty4)
>   mappend4 (MkApp f) (MkApp g) = MkApp (liftA2 mappend4 f g)

Since \GHC\ 8.4, we also need a |Semigroup| instance, because it is now
a superclass of |Monoid|\footnote{See Section~\ref{sec:superclasses} for
a more detailed discussion of this aspect.}:

> instance (Applicative f, Semigroup a)
>   => Semigroup (App f a) where
>   MkApp f <> MkApp g = MkApp (liftA2 (<>) f g)


The \emph{second part} is to now use such a rule in our new form
of |deriving| statement.
We can do this when defining a new data type, such as in
%{
%if style == newcode
%format Maybe = Maybe2
%format Nothing = Nothing2
%format Just = Just2

> instance Applicative Maybe where
>   pure = Just
>   Nothing <*> _ = Nothing
>   _ <*> Nothing = Nothing
>   Just f <*> Just x = Just (f x)
>
> instance Functor Maybe where
>   fmap f x = pure f <*> x

%endif

> data Maybe a = Nothing | Just a
>   deriving Monoid4 via (App Maybe a)

This requires that we independently have an |Applicative| instance
for |Maybe|, but then we obtain the desired |Monoid4| instance nearly
for free.
%}

In the deriving clause, |via| is a new language construct that
explains \emph{how} \GHC\
should derive the instance, namely by reusing the |Monoid| instance
already available for the |via| type, |App Maybe a|.
It should be easy to see why this works:
due to the use of a newtype, |App Maybe a| has the same internal
representation as |Maybe a|, and any instance available on one type can
be made to work on the other by suitably wrapping or unwrapping a newtype.
In more precise language, |App Maybe a| and |Maybe a| are
representationally equal.

The |MODULE Data.Monoid| module defines many further
adapters that can readily be used with \DerivingVia. For example,
the rule that obtains a |Monoid| instance from an |Alternative|
instance is already available through the |Alt| newtype:%
% \footnote{The |Monoid4| and |Semigroup| instance for |App| and |Alt|
% can be made even more concise if additionally employ
% \emph{generalized newtype deriving}, but we refrain from
% this here for clarity. There is more discussion about the relation
% to GND in Section~\ref{sec:gnd}.}

> newtype Alt f a = MkAlt (f a)
>
> instance Alternative f => Monoid4 (Alt f a) where
>   mempty4                        =  MkAlt empty
>   mappend4  (MkAlt f) (MkAlt g)  =  MkAlt (f <|> g)
>
> instance Alternative f => Semigroup (Alt f a) where
>   (<>) = mappend4

Using adapters such as |App| and |Alt|, a vast
amount of |Monoid| instances that currently have to be defined
by hand can instead be derived using the |via|
construct.

\subsection{Contributions and structure of the paper}

The paper is structured as follows:
%
In Section~\ref{sec:quickcheck}, we use the \QuickCheck\ library
as a case study to explain in more detail how \DerivingVia can
be used, and how it works.
%
In Section~\ref{sec:typechecking}, we explain in detail how to
typecheck and translate \DerivingVia\ clauses.
%
In Section~\ref{sec:usecases}, we discuss several additional
applications of \DerivingVia.
%
We discuss related ideas in Section~\ref{sec:related}, describe
the current status of our extension in Section~\ref{sec:status}
and conclude in Section~\ref{sec:conclusions}.

Our extension is fully implemented in a \GHC\
branch\footnote{\url{https://github.com/RyanGlScott/ghc/tree/deriving-via}},
and we are working on a proposal to incorporate it into \GHC proper,
so it will hopefully be available in a future release of \GHC.

The idea of \DerivingVia\ is surprisingly simple, yet it has
a number of powerful and equally surprising properties:
\begin{itemize}
\item It further generalizes the \emph{generalized newtype deriving}
  extension. (Section~\ref{sec:gnd}).

\item It additionally generalizes the concept of \emph{default signatures}.
  (Section~\ref{sec:defaultsignatures}).

\item It provides a possible solution to the problem of
  introducing additional boilerplate code when introducing
  new superclasses (such as |Applicative| for |Monad|,
  Section~\ref{sec:superclasses}).

\item It allows for reusing instances not just between
  representationtally equal types,
  but also between isomorphic or similarly related
  types (Section~\ref{sec:isomorphisms}).
\end{itemize}

\section{Case study: \QuickCheck}\label{sec:quickcheck}
%if style /= newcode
%format Arbitrary = "\cl{Arbitrary}"
%format arbitrary = "\id{arbitrary}"
%format shrink = "\id{shrink}"
%endif

\QuickCheck~\cite{quickcheck} is a well-known Haskell library for randomized
property-based testing.  At the core of \QuickCheck's test-case generation
functionality is the |Arbitrary| class. Its primary method is |arbitrary|, which
describes how to generate suitable random values of a given size and type. It
also has a method |shrink| that is used to try to shrink failing
counterexamples of test properties.

Many standard Haskell types, such as |Int| and lists, are already instances of
|Arbitrary|. This can be very convenient, because many properties
involving these types can be quick-checked without any extra work.

On the other hand, there are often additional constraints imposed on the
actual values of a type that are not sufficiently expressed in their types.
Depending on the context and the situation, we might want to guarantee that we
generate positive integers, or non-empty lists, or even sorted lists.

The \QuickCheck\ library provides a number of newtype-based adapters
(called \emph{modifiers} in the library) for this purpose. As an example,
\QuickCheck\ defines:
%if style /= newcode
%format Positive = "\ty{Positive}"
%format NonNegative = "\ty{NonNegative}"
%format MkNonNegative = "\con{NonNegative}"
%format getNonNegative = "\id{getNonNegative}"
%format Age = "\ty{Age}"
%format Duration = "\ty{Duration}"
%format MkDuration = "\con{Duration}"
%endif

> newtype NonNegative a =
>   MkNonNegative { getNonNegative :: a }

which comes with a predefined instance of the form

> instance (Num a, Ord a, Arbitrary a)
>   => Arbitrary (NonNegative a)

%if style == newcode

>   where
>   arbitrary = coerce @(Gen (QC.NonNegative a)) @(Gen (NonNegative a)) arbitrary

%endif
that explains how to generate and shrink non-negative numbers. A user who wants
a non-negative integer can now use |NonNegative Int| rather than |Int| to make this
obvious.

This approach, however, has a drastic disadvantage: we have to wrap each value
in an extra constructor, and the newtype and constructor are \QuickCheck-specific.
An implementation detail (the choice of testing library) leaks into the data model
of an application. While we might be willing to use domain-specific newtypes for
added type safety, such as |Age| or |Duration|, we might not be eager to add
\QuickCheck\ modifiers everywhere. And what if we need more than one modifier?
And what if other libraries export their own set of modifiers as well? We certainly
do not want to change the actual definition of our data types (and corresponding
code) whenever we start using a new library.

With \DerivingVia, we have the option to reuse the existing infrastructure of
modifiers without paying the price of cluttering up our data type definitions.
We can choose an actual domain-specific newtype such as
%if style /= newcode
%format ?? = "\hsindent{2}"
%else
%format ?? = "  "
%endif

> newtype Duration = MkDuration Int -- in seconds

and now specify exactly how the |Arbitrary| should be derived for this:
%if style == newcode
The simplest option is to derive via |Int| itself:

> ??deriving Arbitrary via Int

This declaration has exactly the same effect as using the
@GeneralizedNewtypeDeriving@ extension to derive the instance: because
|Int| and |Duration| have the same run-time representation, we can reuse
the instance for |Int|, but this allows negative durations.

If we want to restrict ourselves to non-negative durations, we replace this by
%endif
%if style == newcode
%format Duration = Duration2
%format MkDuration = MkDuration2

> newtype Duration = MkDuration Int

%endif

> ??deriving Arbitrary via (NonNegative Int)

This yields an |Arbitrary| instance that generates only non-negative
integers. Only the deriving clause changes, not the data type itself. If we later decide we want
only positive integers as durations, we replace |NonNegative| with |Positive|
in the deriving clause. Again, the data type itself is unaffected. In particular,
we do not have to change any constructor names anywhere in our code.

\subsection{Composition}
%if style /= newcode
%format Large = "\ty{Large}"
%format MkLarge = "\con{Large}"
%format getLarge = "\id{getLarge}"
%endif

Multiple modifiers can be combined. For example, there is another modifier
called |Large| that will scale up the size of integral values being produced
by a generator. It is defined as

> newtype Large a = MkLarge { getLarge :: a }

%if style == newcode

>   deriving (Eq, Ord, Num) via a

%endif
with a corresponding |Arbitrary| instance:
%{
%if style /= newcode
%format => = "\mathbin{\texttt{=>}}"
%endif

> instance (Integral a, Bounded a) => Arbitrary (Large a)

%}
%if style == newcode

>   where
>   arbitrary = coerce @(Gen (QC.Large a)) @(Gen (Large a)) arbitrary

%endif
For our |Duration| type, we can easily write
%if style == newcode
%format Duration = Duration3
%format MkDuration = MkDuration3

> newtype Duration = MkDuration Int

%endif

> ??deriving Arbitrary via (NonNegative (Large Int))

and derive an instance which only generates |Duration| values that are both
non-negative \emph{and} large.
This works because |Duration| still shares the same runtime representation as
|NonNegative (Large Int)| (namely, that of~|Int|), so the latter's
|Arbitrary| instance can be reused.

\subsection{Adding new modifiers}

Of course, we can add add our own modifiers if the set of predefined modifiers
is not sufficient. For example, it is difficult to provide a completely generic
|Arbitrary| instance that works for all data types, simply because there are too
many assumptions about what makes good test data that need to be taken into account.

But for certain groups of data types, there are quite reasonable strategies of
coming up with generic instances. For example, for enumeration types, one
strategy is to desire a uniform distribution of the finite set of values.
\QuickCheck\ even offers such a generator, but it does not expose it as a newtype
modifier:
%if style /= newcode
%format arbitraryBoundedEnum2 = arbitraryBoundedEnum
%format arbitraryBoundedEnum = "\id{arbitraryBoundedEnum}"
%format BoundedEnum = "\ty{BoundedEnum}"
%format MkBoundedEnum = "\con{BoundedEnum}"
%endif

> arbitraryBoundedEnum2 :: (Bounded a, Enum a) => Gen a

%if style == newcode

> arbitraryBoundedEnum2 = arbitraryBoundedEnum

%endif
But from this, we can easily define our own:

> newtype BoundedEnum a = MkBoundedEnum a
>
> instance (Bounded a, Enum a)
>   => Arbitrary (BoundedEnum a) where
>   arbitrary = MkBoundedEnum <$> arbitraryBoundedEnum

We can then use this functionality to derive |Arbitrary| for a new
enumeration type:
%if style /= newcode
%format Weekday = "\ty{Weekday}"
%format Mo = "\con{Mo}"
%format Tu = "\con{Tu}"
%format We = "\con{We}"
%format Th = "\con{Th}"
%format Fr = "\con{Fr}"
%format Sa = "\con{Sa}"
%format Su = "\con{Su}"
%endif

> data Weekday = Mo | Tu | We | Th | Fr | Sa | Su
>   deriving (Enum, Bounded)
>   deriving Arbitrary via (BoundedEnum Weekday)

\subsection{Parameterized modifiers}

Sometimes, we might want to parameterize a generator with extra data.
We can do so by defining a modifier that has extra arguments and using
those extra arguments in the associated |Arbitrary| instance.

An extreme case that also makes use of type-level programming features
in \GHC\ is a modifier that allows us to specify a lower and an upper bound
of a generated natural number.
%if style /= newcode
%format Between = "\ty{Between}"
%format MkBetween = "\con{Between}"
%format KnownNat = "\cl{KnownNat}"
%format choose = "\id{choose}"
%format natVal = "\id{natVal}"
%format MkProxy = "\con{Proxy}"
%format (TYAPP t) = "\texttt{@}" t
%format Year = "\ty{Year}"
%format MkYear = "\con{Year}"
%format Nat = "\ki{Nat}"
%else
%format (TYAPP t) = "@" t
%format MkProxy = Proxy
%endif

> newtype Between (l :: Nat) (u :: Nat) = MkBetween Integer
>
> instance (KnownNat l, KnownNat u)
>   => Arbitrary (Between l u) where
>   arbitrary =  MkBetween <$>
>     choose (  natVal (TYAPP l) MkProxy,  natVal (TYAPP u) MkProxy)

(Note that this instance makes use of visible type application~\cite{vta} in
|natVal (TYAPP l)| and |natVal (TYAPP u)|.)

We can then equip an application-specific type for years with
a generator that lies within a plausible range:

> newtype Year = MkYear Integer
>   deriving Show
>   deriving Arbitrary via (Between 1900 2100)

%if style == newcode

> deriving instance Show (Between l u)

%endif

In general, we can use this technique of adding extra parameters to a newtype
to support additional ways to configure the behavior of derived instances.

\section{Typechecking and translation}\label{sec:typechecking}

Seeing enough examples of \DerivingVia\ can give the impression that it is
a somewhat magical feature. In this section, we aim to explain the magic
underlying \DerivingVia\ by giving a more precise description of:
\begin{itemize}
 \item How \DerivingVia\ clauses are typechecked.
 \item What code \DerivingVia\ generates behind the scenes.
 \item How to determine the scoping of type variables in \DerivingVia\ clauses.
\end{itemize}

%{
%if style /= newcode
%format (sub (x) (i)) = x "_{" i "}"
%format D = "\ty{D}"
%format y_1
%format DOTS = "\textrm{\dots} "
%format T1 = "\ty{T1}"
%format Ts = "\ty{Ts}"
%format m = "\mathit{m}"
%format n = "\mathit{n}"
%format p = "\mathit{p}"
%format i = "\mathit{i}"
%format r = "\mathit{r}"
%format m1 = "\id{m1}"
%format mf = "\id{mf}"
%format C = "\cl{C}"
%endif

% RGS: Commented out, since this is likely more confusing than not.
%
% Throughout this section, we will refer to two groups of examples. One example,
% shown below, is intended to be as general as possible:
%
% < data D (sub x 1) DOTS (sub x d) = DOTS
% <   deriving (C (sub y 1) DOTS (sub y (c - 1)))
% <            via (V (sub z 1) DOTS (sub z v))
% <
% < class C (sub y 1) DOTS (sub y (c-1)) (sub y c) where
% <   type T1 (sub t1 1) DOTS (sub y c) DOTS (sub t1 m)
% <   DOTS
% <   type Ts (sub ts 1) DOTS (sub y c) DOTS (sub ts n)
% <
% <   m1 :: (sub mty1 1) DOTS (sub y c) DOTS (sub mty1 o)
% <   DOTS
% <   mf :: (sub mtyf 1) DOTS (sub y c) DOTS (sub mtyf p)
%
% In other words, |D| is a data type with |d| type parameters, |C| is a type
% class with |c| type parameters, and |V| is some type with |v| type parameters.
% Moreover, |C| has |s| associated type families and |f| class methods, each
% with different kinds and types, but all of which mentioning the last type
% parameter of |C|, |sub y c|.

To avoid clutter, we assume that all types have monomorphic kinds. However, it
is easy to incorporate kind polymorphism~\cite{haskell-promotion}, and our
implementation of these ideas in \GHC\ does so.

\subsection{Well-typed uses of \DerivingVia}

\DerivingVia\ grants the programmer the ability to put extra types in her programs,
but the flip side to this is that it is possible for her to accidentally put total nonsense
into a \DerivingVia\ clause, such as:
%if style /= newcode
%format S = "\ty{S}"
%format MkS = "\con{S}"
%endif

< newtype S = MkS Char
<   deriving Eq via Maybe

In this section, we describe a general algorithm for when a \DerivingVia\ clause should
typecheck, which will allow us to reject ill-formed examples like the one above.

\subsubsection{Aligning kinds} \label{sec:kinds}

Suppose we are deriving the following instance:

%if style /= newcode
%format (sub (x) (i)) = x "_{" i "}"
%format D = "\ty{D}"
%format y_1
%format DOTS = "\textrm{\dots} "
%format T1 = "\ty{T1}"
%format Ts = "\ty{Ts}"
%format m1 = "\id{m1}"
%format mf = "\id{mf}"
%format C = "\cl{C}"
%format V = "\ty{V}"
%format 1 = "1"
%endif


< data D (sub d 1) DOTS (sub d m)
<   deriving (C (sub c 1) DOTS (sub c n)) via (V (sub v 1) DOTS (sub v p))

In order for this declaration to typecheck, we must check the \emph{kinds} of each type.
In particular, the following conditions must hold:

%{
%if style /= newcode
%format -> = "\mathop{\texttt{->}}\linebreak[2]"
%endif
\begin{enumerate}
 \item
   The type |C (sub c 1) DOTS (sub c n)| must be of kind
   |((sub k 1) -> DOTS -> (sub k r) -> TYPE) -> Constraint| for some kinds
   |(sub k 1), DOTS, (sub k r)|.
   The reason is that the instance we must generate,

< instance C (sub c 1) DOTS (sub c n) (D (sub d 1) DOTS (sub d i)) where DOTS

   requires that we can apply |C (sub c 1) DOTS (sub c n)| to another type
   |D (sub d 1) DOTS (sub d i)| (where \(i \leq m\), see Section~\ref{sec:eta}).
   Therefore, it would be nonsense to try to derive an instance of |C (sub c 1) DOTS (sub c n)|
   if it had kind, say, |Constraint|.

 \item
   The kinds |V (sub v 1) DOTS (sub v p)| and |D (sub d 1) DOTS (sub d i)|, and the
   kind of the argument to |C (sub c 1) DOTS (sub c n)| must all unify.
   This check rules out the above example of |deriving Eq via Maybe|, as it does
   not even make sense to talk about
   reusing the |Eq| instance for |Maybe|---which is of kind |(TYPE -> TYPE)|---as |Eq| instances
   can only exist for types of kind |TYPE|.
\end{enumerate}

\subsubsection{Eta-reducing the data type}\label{sec:eta}

Note that in the conditions above, we specify
|D (sub d 1) DOTS (sub d i)| (for some |i|), instead of
|D (sub d 1) DOTS (sub d m)|. That is because in general, the kind of the argument to
|C (sub c 1) DOTS (sub c n)| is allowed to be different from the kind of
|D (sub d 1) DOTS (sub d m)|! For instance, the following example is perfectly legitimate:
%if style == newcode
%format DDOTS =
%else
%format DDOTS = DOTS
%format Functor2 = Functor
%endif

> class Functor2 (f :: TYPE -> TYPE) where DDOTS
>
> data Foo a = MkFoo a a
>   deriving Functor

despite the fact that |Foo a| has kind |TYPE| and the argument to |Functor|
has kind |(TYPE -> TYPE)|. This is
because the code that actually gets generated has the following shape:

< instance Functor Foo where DOTS

To put it differently, we have \emph{eta-reduced} away the |a| in |Foo a| before applying
|Functor| to it. The power to eta-reduce variables from the data type is part of what
makes deriving clauses so flexible.

To determine how many variables to eta-reduce,
we must examine the kind of
|C (sub c 1) DOTS (sub c n)|, which by condition (1) is of the form
|(((sub k 1) -> ... -> (sub k r) -> *) -> Constraint)| for some kinds
|(sub k 1), DOTS, (sub k r)|. Then the number of variables to eta-reduce is simply \(r\),
so to compute the \(i\) in |D (sub d 1) DOTS (sub d i)|, we take \(i = m - r\).

This is better explained by example, so consider the following two scenarios,
both of which typecheck:
%if style /= newcode
%format A = "\ty{A}"
%format MkA = "\con{A}"
%format B = "\ty{B}"
%format MkB = "\con{B}"
%format Identity = "\ty{Identity}"
%endif

> newtype A a = MkA a deriving Eq       via (Identity a)
> newtype B b = MkB b deriving Functor  via Identity

In the first example, we have the class |Eq|, which is of kind |TYPE -> Constraint|. The argument
to |Eq|, which is of kind |TYPE|, does not require that we eta-reduce any variables. As a result,
we check that |A a| is of kind |TYPE|, which is the case.

In the second example, we have the class |Functor|, which is of kind |(TYPE -> TYPE) -> Constraint|.
The argument to |Functor| is of kind |(TYPE -> TYPE)|, which requires that we eta-reduce one variable
from |B b| to obtain |B|. We then check that |B| is kind of |(TYPE -> TYPE)|, which is true.
%}
%}

\subsection{Code generation}

Once the typechecker has ascertained that a |via| type is fully compatible with the data type
and the class for which an instance is being derived, \GHC\ proceeds with generating the code
for the instance itself. This generated code is then fed \emph{back} into the typechecker,
which acts as a final sanity check that \GHC\ is doing the right thing under the hood.

\subsubsection{Generalized newtype deriving (\GND)} \label{sec:gnd}

The process by which \DerivingVia\ generates code is heavily based off of the approach that
the \GND\ takes, so it is informative to first explain how
\GND\ works. From there, \DerivingVia\ is a straightforward
generalization---so much so that \DerivingVia\ could be thought of as
``generalized \GND''.

Our running example in this section will be the newtype |Age|, which is a simple
wrapper around |Int| (which we will call the \emph{representation type}):
%if style /= newcode
%format Age = "\ty{Age}"
%format MkAge = "\con{MkAge}"
%endif

> newtype Age = MkAge Int
>   deriving Enum

A na{\"i}ve way to generate code would be to manually wrap and unwrap the |MkAge| constructor
wherever necessary, such as in the code below:
%if style == newcode
%format Age = Age2
%format MkAge = MkAge2

> newtype Age = MkAge Int

%endif

> instance Enum Age where
>   toEnum i = MkAge (toEnum i)
>   fromEnum (MkAge x) = fromEnum x
>   enumFrom (MkAge x) = map MkAge (enumFrom x)

This works, but is somewhat unsatisfying. After all, a newtype is intended to be a zero-cost
abstraction that acts identically to its representation type at runtime. Accordingly, any
function that mentions a newtype in its type signature should be able to be converted to
a new function with all occurrences of the newtype in the type signature replaced with the
representation type, and moreover, that new function should behave identically to the old one
at runtime.

Unfortunately, the implementation of |enumFrom| may not uphold this guarantee. While wrapping
and unwrapping the |MkAge| constructor is certain to be a no-op, the |map| function is
definitely \emph{not} a no-op, as it must walk the length of a list. But the fact that we
need to call |map| in the first place feels rather silly, as all we are doing is wrapping
a newtype at each element.

Luckily, there is a convenient solution to this problem: the safe
|coerce| function~\cite{zero-cost-coercions}:
%if style /= newcode
%format Coercible = "\protect\cl{Coercible}"
%format unsafeCoerce = "\id{unsafeCoerce}"
%endif

< coerce :: Coercible a b => a -> b

Operationally, |coerce| can be thought of as behaving like its wily cousin, |unsafeCoerce|,
which takes a value of one type as casts it to a value at a another type. Unlike |unsafeCoerce|,
which can break programs if used carelessly, |coerce| is completely type-safe due to its
use of the |Coercible| constraint. We explain |Coercible| in more detail later, but for now,
it suffices to say that a |Coercible a b| constraint witnesses the fact that two types |a|
and |b| have the same representation at runtime, and thus any value of type |a| can be
safely cast to type |b|.

Armed with |coerce|, we can show what code \GND\ would actually
generate for the |Enum Age| instance above:
%if style == newcode
%format Age = Age3
%format MkAge = MkAge3

> newtype Age = MkAge Int

%endif

> instance Enum Age where
>   toEnum =
>     coerce (TYAPP (Int -> Int)) (TYAPP (Int -> Age)) toEnum
>   fromEnum =
>     coerce (TYAPP (Int -> Int)) (TYAPP (Age -> Int)) fromEnum
>   enumFrom =
>     coerce (TYAPP (Int -> [Int])) (TYAPP (Age -> [Age])) enumFrom

Now we have a strong guarantee that the |Enum| instance for |Age| has exactly the same
runtime characteristics as the instance for |Int|. As an added benefit, the code ends up
being simpler as every method can be implemented as a straightforward application of
|coerce|.
The only interesting part is generating the two explicit type arguments~\cite{vta}
that are being used to specify the source type (using the representation type)
and the target type (using the newtype) of |coerce|.

\subsubsection{The |Coercible| constraint} \label{sec:coercible}

A |Coercible| constraint can be thought of as evidence that \GHC\ can use to
cast between two types. |Coercible| is not a type class, so it is impossible to write
a |Coercible| instance by hand. Instead, \GHC\ can generate and solve |Coercible| constraints
automatically as part of its built-in constraint solver, much like it can solve equality
constraints. (Indeed, |Coercible| can be thought of as a broader notion of equality among
types.)

As mentioned in the previous section, a newtype can be safely cast to and from its
representation type, so \GHC\ treats them as inter-|Coercible|. Continuing our earlier example,
this would mean that \GHC\ would be able to conclude that:

< instance Coercible Age Int
< instance Coercible Int Age

But this is not all that |Coercible| is capable of. A key property is that \GHC's constraint
solver can look inside of other type constructors when determining if two types are
inter-|Coercible|. For instance, both of these statements hold:

< instance Coercible (Age -> [Age]) (Int -> [Int])
< instance Coercible (Int -> [Int]) (Age -> [Age])

This demonstrates the ability to cast through the function and list type constructors. This
ability is important, as our derived |enumFrom| instance would not typecheck otherwise!

Another crucial fact about |Coercible| that we rely on is that it is transitive: if
|Coercible a b| and |Coercible b c| hold, then |Coercible a c| also holds. This is perhaps
unsurprising if one views |Coercible| as an equivalence relation, but it a fact that is worth
highlighting, as the transitivity of |Coercible| is what allows us to |coerce|
\emph{between newtypes}. For instance, if we have these two newtypes:
%if style /= newcode
%format A = "\ty{A}"
%format MkA = "\con{A}"
%format B = "\ty{B}"
%format MkB = "\con{B}"
%else
%format A = A2
%format MkA = MkA2
%format B = B2
%format MkB = MkB2
%endif

> newtype A a  =  MkA [a]
> newtype B    =  MkB [Int]

then \GHC\ is able to conclude that |Coercible (A Int) B| holds, because we have the following
|Coercible| rules

< instance Coercible (A Int) [Int]
< instance Coercible [Int] B

as well as transitivity. As we will discuss momentarily, \DerivingVia\
in particular makes heavy use of the transitivity of |Coercible|.

\subsubsection{From \GND\ to \DerivingVia}

As we saw in Section \ref{sec:gnd}, the code which \GND\ generates
relies on |coerce| to do the heavy lifting. In this section, we generalize this
technique slightly to give us a way to generate code for \DerivingVia.

Recall the following \GND-derived instance:

< newtype Age = MkAge Int deriving Enum

As stated above, it generates the following code for |enumFrom|:

< instance Enum Age where
<   DDOTS
<   enumFrom =
<     coerce (TYAPP (Int -> [Int])) (TYAPP (Age -> [Age])) enumFrom

Here, there are two crucially important types: the representation type, |Int|, and the
original newtype itself, |Age|. The implementation of |enumFrom| simply sets up an
invocation of |coerce enumFrom|, with explicit type arguments to indicate that we should
reuse the existing |enumFrom| implementation for |Int| and reappropriate it for |Age|.

The only difference in the code that \GND\ and \DerivingVia\ generate
is that in the former strategy, \GHC\ always picks the representation type for you, but in
\DerivingVia, the \emph{user} has the power to choose this type. For example,
if a programmer had written this instead:
%if style /= newcode
%format T = "\ty{T}"
%format MkT = "\con{T}"
%endif

< newtype T = MkT Int
< instance Enum T where DDOTS
<
< newtype Age = MkAge Int deriving Enum via T

then the following code would be generated:

< ??  enumFrom =
<       coerce (TYAPP (T -> [T])) (TYAPP (Age -> [Age])) enumFrom

This time, \GHC\ coerces from an |enumFrom| implementation for |T| (the |via| type) to
an implementation for |Age|. (Recall from Section \ref{sec:coercible} that this is
possible since we can |coerce| transitively from |T| to |Int| to |Age|).

Now we can see why the instances that \DerivingVia\ can generate are a strict superset of
those that \GND\ can generate. For instance, our earlier
\GND\ example

< newtype Age = MkAge Int deriving Enum

could equivalently have been written using \DerivingVia\ like so:

< newtype Age = MkAge Int deriving Enum via Int

\subsection{Type variable scoping}\label{sec:typevariablescoping}

In the remainder of this section, we present an overview of how type
variables are bound in \DerivingVia\ clauses, and over what types they scope.
\DerivingVia\ introduces a new place where types can go, and more importantly,
it introduces a new place where type variables can be \emph{quantified}, so
it takes some amount of care to devise a consistent treatment for it.

\subsubsection{Binding sites}

Consider the following example:
%if style /= newcode
%format Bar = "\cl{Bar}"
%format Baz = "\cl{Baz}"
%format MkFooDots = DOTS
%else
%format Foo = Foo3
%endif

> data Foo a = MkFooDots
>   deriving (Baz a b c) via (Bar a b)

%if style == newcode

> data Bar a b
> class Baz a b c d

%endif
Where is each type variable quantified?

\begin{itemize}
 \item |a| is bound by |Foo| itself in the declaration |data Foo a|.
       Such a variable scopes over both the derived class,
       |Baz a b c|, as well as the |via| type, |Bar a b|.
 \item |b| is bound by the |via| type, |Bar a b|. Note that |b| is
       bound here but |a| is not, as it was bound earlier by the |data|
       declaration. |b| scopes over the derived class type, |Baz a b c|,
       as well.
 \item |c| is bound by the derived class, |Baz a b c|, as it was not bound
       elsewhere. (|a| and |b| were bound earlier.)
\end{itemize}

In other words, the order of scoping starts at the |data| declaration, then the
|via| type, and then the derived classes associated with that |via| type.

\subsubsection{Establishing order}

This scoping order may seem somewhat surprising, as one might expect the
type variables bound by the derived classes to scope over the |via| type
instead. However, this choice introduces additional complications that are
tricky to resolve. For instance, consider a scenario where one attempts to
derive multiple classes at once with a single |via| type:
%if style /= newcode
%format D = "\ty{D}"
%format C1 = "\cl{C1}"
%format C2 = "\cl{C2}"
%endif

> data D
>   deriving (C1 a, C2 a) via (T a)

%if style == newcode

> class C1 a b
> class C2 a b
> data T a

%endif

Suppose we first quantified the variables in the derived classes and made them
scope over the |via| type. Because each derived class
has its own type variable scope, the |a| in |C1 a| would be bound independently from
the |a| in |C2 a|. In other words, we would have something like this (using a
hypothetical |forall| syntax):

< ??deriving (forall a. C1 a, forall a. C2 a) via (T a)

Now we are faced with a thorny question: which |a| is used in the |via| type,
|T a|? There are multiple choices here, since the |a| variables in
|C1 a| and |C2 a| are distinct! This is an important decision, since the kinds
of |C1| and |C2| might differ, so the choice of |a| could affect whether
|T a| kind-checks or not.

On the other hand, if one binds the |a| in |T a| first and has it scope over
the derived classes, then this becomes a non-issue. We would instead have this:

< ??deriving (C1 a, C2 a) via (forall a. T a)

Now, there is no ambiguity regarding |a|, as both |a| variables in the list of
derived classes were bound in the same place.

It might feel strange visually to see a variable being used
\emph{before} its binding site (assuming one reads code from left to right).
However, this is not unprecedented within Haskell, as this is also legal:
%{
%if style /= newcode
%format f = "\id{f}"
%endif

> f = g + h where g = 1; h = 2

In this example, we have another scenario where things are bound (|g| and |h|)
after their use sites. In this sense, the |via| keyword is continuing a rich
tradition pioneered by |where| clauses.
%}

One alternative idea (which was briefly considered) was to put the |via| type
\emph{before} the derived classes so as to avoid this ``zigzagging'' scoping.
However, this would introduce additional ambiguities. Imagine one were to
take this example:

< ??deriving Z via X Y

And convert it to a form in which the |via| type came first:

< ??deriving via X Y Z

Should this be parsed as |(X Y) Z|, or |X (Y Z)|? It's not clear visually, so
this choice would force programmers to write additional parentheses.

% In the example above, |b| was implicitly quantified, but we could imagine that it
% was explicitly quantified by using |forall| syntax:
% %if style == newcode
% %format Foo = Foo3
% %format MkFoo = MkFoo3
% %format PARENS (x) = ((x))
% %else
% %format PARENS (x) = (x)
% %endif
%
% < data Foo a = MkFoo
% <   deriving PARENS (forall b. Bar a b) via (Baz a b)
%
% This declaration of |Foo| is wholly equivalent to the earlier one, but the use
% of |forall| makes it clear where |b|'s binding site is\alnote{%
% \dots at the price of obfuscating what |b| scopes over \dots}. The possibility for
% explicit quantification of class type variables raises an interesting question:
% how is the following data type treated?
% %if style /= newcode
% %format X = "\ty{X}"
% %format Y = "\cl{Y}"
% %format Z = "\cl{Z}"
% %endif
%
% < data X a = DOTS
% <   deriving (forall a. Y a) via (Z a)
%
% First, recall that the data type variable binders are the outermost ones.
% Moreover, because |Y| explicitly binds its own type variable named |a| within
% the |deriving| clause, the |a| within |Y a| is distinct from the |a| in |X a|.
% And since the binding site for the |a| in |Y a| occurs deeper than the binding
% site for the |a| in |X a|, the |a| in |Z a| refers to the same |a| as in
% |Y a|.
%
% \alnote{What if the via-clause refers to a variable that does not occur in the
% data type or before the via? Can this ever be correct (I think so)? Can we still
% explicitly quantify over it, even if it looks totally silly?}
%
% \subsubsection{Multiple binding sites?}
%
% One slight wrinkle in this story is that |deriving| clauses can specify \emph{multiple}
% classes to derive per data type, e.g.,
%
% < data Bar
% <   deriving (C1 a, C2 a)
%
% How should this behave when combined with |deriving via|? Suppose we augmented the previous
% example with a |via| type, and to make the issue more evident, let's explicitly quantify the
% type variables in the |deriving| clause:
%
% < data Bar
% <   deriving (forall a. C1 a, forall a. C2 a) via (T a)
%
% Where is the |a| in |T a| bound? There are two equally valid options: the |a| from
% |forall a. C1 a|, or the |a| from |forall a. C2 a|. Moreover, we cannot combine the binding
% sites for these |a| variables in general, as it is possible that the |a| in |C1 a| has a
% different kind than the |a| in |C2 a|.
%
% We avoid this thorny issue as follows: whenever we have a |deriving via| clause with
% two or more classes, we desugar it to a series of single-class |deriving via| clauses.
% For instance, we would desugar our earlier example:
%
% < data Bar
% <   deriving (forall a. C1 a, forall a. C2 a) via (T a)
%
% Into this:
%
% < data Bar
% <   deriving (forall a. C1 a) via (T a)
% <   deriving (forall a. C2 a) via (T a)
%
% Now, the quantification has become unambiguous.
%
% A tricky corner case to consider is that |deriving| clauses can also derive \emph{zero}
% classes to derive. Combined with |deriving via|, this can lead to the following example:
%
% < data Bar
% <   deriving () via S
%
% To deal with this, we opt to desugar this declaration to a data type with no |deriving|
% clauses whatsoever:
%
% < data Bar
%
% This is a bit strange, since the |S| type is never actually used post-desugaring, but doing
% so keeps the rules fairly consistent. Some care is needed here, however, because we must
% also reject an example like this:
%
% < data Bar
% <   deriving () via (T a)
%
% Where the |a| in |T a| has no binding site.


% \subsection{|deriving via| is opt-in}
%
% |deriving| can sometimes be slightly ambiguous due to the fact that it can generate completely
% different code for a type class instance depending on the context. For instance,
% consider the following example:
%
% < data T = MkT Int
% <   deriving Ord
%
% In this case, GHC will generate the following instance:
%
% < instance Ord T where
% <   compare (MkT i1) (MkT i2) = compare i1 i2
%
% This is the standard approach for deriving |Ord|. However, if one tweaks the definition of |T|
% slightly:
%
% < newtype T = MkT Int
% <   deriving Ord
%
% Then GHC recognizes the fact that |T| is a newtype and will instead generate code
% using the @GeneralizedNewtypeDeriving@ approach:
%
% < instance Ord T where
% <   compare = coerce (compare :: Int -> Int -> Ordering)
%
% This approach uses an explicit TODO RGS

\section{More use cases}\label{sec:usecases}

We have already seen in Section~\ref{sec:quickcheck} how \DerivingVia\
facilitates greater code reuse in the context of \QuickCheck. This is far from
the only domain where \DerivingVia\ proves to be a natural fit, however. In
fact, there are so many of these domains, there would be enough to fill
pages upon pages!

Unfortunately, we do not have enough space to document all of these use cases,
so in this section, we present a cross-section of scenarios in which
\DerivingVia\ can capture interesting patterns and allow programmers to
abstract over them in a convenient way.

% We demonstrate how to:
%
% \begin{itemize}
% \item Swiftly define instances of classes in a superclass hierarchy
%       (Section~\ref{sec:superclasses}) and avoid orphan instances
%       (Section~\ref{sec:orphaninstances}).
% \item Codify techniques to achieve asymptotic performance improvements
%       in default implementations of class methods
%       (Section~\ref{sec:asymptotic}).
% \item Generalize the concept of default signatures to allow for
%       \textit{multiple} defaults
%       (Section~\ref{sec:defaultsignatures}).
% \item Share code between types that are \textit{isomorphic}, not just
%       representationally equivalent
%       (Section~\ref{sec:isomorphisms}).
% \end{itemize}

\subsection{Asymptotic improvements with ease}\label{sec:asymptotic}
%if style /= newcode
%format Rep = "\ty{Rep}"
%format Type = "\ki{Type}"
%format index = "\id{index}"
%format tabulate = "\id{tabulate}"
%format Representable = "\cl{Representable}"
%endif

A widely used feature of type classes is their ability to give default
implementations for their methods if a programmer leaves them off.
One example of this can be found in the |Applicative| class. The main
workhorse of |Applicative| is the |(<*>)| method, but on occasion,
it is more convenient to use the |(<*)| or |(*>)| methods, which
sequence their actions but discard the result of one of their arguments:
%{
%if style == newcode
%format Applicative = Applicative2
%format pure = pure2
%format <*> = .<*>
%format <* = .<*
%format *> = .*>
%format liftA2 = liftA22

> liftA2 f x y = pure f <*> x <*> y

%endif

> class Functor f => Applicative f where
>   pure  :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b
>
>   (<*) :: f a -> f b -> f a
>   (<*) = liftA2 (\ a _ -> a)
>   (*>) :: f a -> f b -> f b
>   (*>) = liftA2 (\ _ b -> b)

As shown here, |(<*)| and |(*>)| have default implementations in terms of
|liftA2|. This works for any |Applicative|, but is not as efficient as it
could be in some cases. For some instances of |Applicative|, we can
actually implement these methods in \(O(1)\) time instead of using
|liftA2|, which can often run in superlinear time. One such
|Applicative| is the function type |(->)|:

> instance Applicative ((->) r) where
>   pure = const
>   (<*>) f g x = f x (g x)
>   f <*  _ = f
>   _  *> g = g

%}
Note that we had to explicitly define |(<*)| and |(*>)|, as the default
implementations would not have been as efficient. But |(->)| is not the
only type for which this trick works---it also works for any data type
that is isomorphic to |(->) r| (for some |r|).
These ‘function-like’ types are characterized by the |Representable|
type class:

> class Functor f => Representable f where
>   type Rep f
>   index    :: f a -> (Rep f -> a)
>   tabulate :: (Rep f -> a) -> f a

This is a good deal more abstract than |(->) r|, so it can be helpful to
see how |Representable| works for |(->) r| itself:

> instance Representable ((->) r) where
>   type Rep ((->) r) = r
>   index f = f
>   tabulate f = f

With |Representable|, we can codify the |Applicative| shortcut for |(<*)| and
|(*>)| with a suitable newtype:
%if style /= newcode
%format WrapRep = "\ty{WrapRep}"
%format MkWrapRep = "\con{WrapRep}"
%endif

> newtype WrapRep f a = MkWrapRep (f a)
>   deriving (Functor, Representable)
>
> instance Representable f
>   => Applicative (WrapRep f) where
>   pure = tabulate . pure
>   f <*> g = tabulate (index f <*> index g)
>
>   f <* _ = f
>   _ *> g = g

Now, instead of having to manually override |(<*)| and \mbox{|(*>)|} to get the
desired performance, one can accomplish this in a more straightforward fashion
by using \DerivingVia:
%if style /= newcode
%format IntConsumer = "\ty{IntConsumer}"
%format MkIntConsumer = "\con{IntConsumer}"
%endif

> newtype IntConsumer a = MkIntConsumer (Int -> a)
>   deriving (Functor, Representable)
>   deriving Applicative via (WrapRep IntConsumer)

Not only does this save code in the long run, but it also gives a name to
the optimization being used, which allows it to be documented, exported
from a library, and thereby easier to spot ``in the wild'' for other programmers.

% There is a class of functors where the last two definitions always
% hold,
%
%
%
% which this definition of |(<*)|,
% |(*>)| always holds and that is any functor isomorphic to |(Rep f
% ->)|.
%
% For a class of ‘function-like’ functors
% they can be defined as constant functions
%
%
%
% And assuming that our functor |f| is isomorphic to functions |(Rep f
% ->)| (|Representable f|) we can always define |(<*)|, |(*>)| this way.
%
% This definition is actually valid for a subset of functors that is
% isomorphic to functions, called |Representable|
%
% so for each |Representable| we get these constant-time operations
%
% < f <* _ = f
% < _ <* g = g
%
% For representable functors the definitions of |m *> _ = m| and |_ <* m
% = m| are \(O(1)\).\footnote{Edward Kmett:
% \url{https://ghc.haskell.org/trac/ghc/ticket/10892?cversion=0&cnum_hist=4\#comment:4}
% } This codifies knowledge (on a ``library, not lore'' principle) where
% the code can be documented and linked to.

%if style == newcode
\subsection{Deriving with configuration}

This lets us pass static static value to instance deriving.

< data Person = P { name :: String, age :: Int, addr :: Maybe Address }
<   deriving (Show, Read, ToJSON, FromJSON)
<     via (Person `EncodeAs` Config OmitNothing)

Many of these newtypes existed a long time before @-XDerivingVia@ did
but can be used directly with it which is promising.
%endif

%if style == newcode
\subsection{Every Applicative can be reversed}

The Haskell ‘wisdom’ that says every |Applicative| can be reversed can
be codified in the data type |Rev|:\alnote{|Rev| is called |Backwards|
in @transformers@.}

> newtype Rev f a = MkRev (f a) deriving Functor
>
> instance Applicative f => Applicative (Rev f) where
>   pure = MkRev . pure
>
>   MkRev f <*> MkRev x = MkRev (liftA2 (flip ($)) x f)

%endif

%if style == newcode
\subsection{Equivalent Applicative definition}

There is an equivalent, more symmetric definition of |Applicative|
arising from category theory (characterizing Applicative as a strong
lax monoidal functor)~\cite{computations-as-monoids} that can be more
convenient to define and work with
~\cite{applicative-programming-with-effects}
~\cite{constructing-applicative-functors}
%if style /= newcode
%format Monoidal = "\cl{Monoidal}"
%format unit = "\id{unit}"
%format mult = "\id{mult}"
%format WrapMonoidal = "\ty{WrapMonoidal}"
%format WrapApplicative = "\ty{WrapApplicative}"
%format WM = "\con{WM}"
%format <$ = "\opsym{<\$}"
%endif

> class Functor f => Monoidal f where
>   unit  ::  f ()
>   mult  ::  f a -> f b -> f (a, b)

Allowing us to derive |Applicative| from a |Monoidal| instance, allow
us to use whatever formulation we prefer

> newtype WrapMonoidal f a = WM (f a)
>   deriving newtype (Functor, Monoidal)
>
> instance Monoidal f => Applicative (WrapMonoidal f) where
>   pure a    = a <$ unit
>   mf <*> mx = fmap (\(f, x) -> f x) (mf `mult` mx)

We can then define the opposite direction, codifying the equivalence
in these two instances

< instance Monoidal     f => Applicative  (WrapMonoidal     f)
< instance Applicative  f => Monoidal     (WrapApplicative  f)

This becomes more important (and assist us in transitioning) as we
move to a more categorical.\footnote{Such as Kmett's |hask|}

\subsection{Equivalent Monad definition}

\cite{computations-as-monoids}
%if style /= newcode
%format Triple = "\cl{Triple}"
%format eta = "\id{eta}"
%format mu = "\id{mu}"
%format WrapTriple = "\ty{WrapTriple}"
%format WT = "\con{WT}"
%format unWT = "\id{unWT}"
%endif

> class Functor m => Triple m where
>   eta  ::  a -> m a
>   mu   ::  m (m a) -> m a
>
> newtype WrapTriple m a = WT { unWT :: m a } deriving newtype Functor
>
> instance Triple m => Applicative (WrapTriple m) where
>   pure = WT . eta
>
>   WT mf <*> WT mx = WT (mu (fmap (\f -> mu (fmap (eta . f) mx)) mf))
>
> instance Triple m => Monad (WrapTriple m) where
>   WT mx >>= k = WT (mu (fmap (unWT . k) mx))

%endif

%if style == newcode
\subsection{Classes over Defunctionalization Symbols}

\bbnote{TODO}: Using \Package{singletons} library we can create
instances of actual functions of types, not just matchable
constructors

< class Functor f where
<   fmap :: (a -> a') -> (f@@a -> f@@a')
<
< dup     a     = (a, a)
< kleisli f a b = a -> f @@ b
<
< instance Functor id
< instance Functor dup
< instance (Functor f, Functor g) => Functor (f . g)
< instance Functor f => Functor (kleisli f a)

at the cost of inference. But if we are willing to guide the
inference Haskell will synthesize the code for us:

< newtype Apply f a = Apply (f @@ a)
<
< instance Functor f => Prelude.Functor (Apply f) where
<   Prelude.fmap f (Apply fa) = Apply (fmap @f f fa)
<
< newtype DupKleiDup a b = DKD (a -> (b, b), a -> (b, b))
<   deriving Prelude.Functor
<     via (Apply (dup . kleisli dup a))

Refinement Reflection:
Parallel Legacy Languages as Theorem Provers (deriving
%endif

%if style == newcode
\subsection{Traversal order}
\url{Discuss ideas here https://www.reddit.com/r/haskell/comments/6udl0i/representable_functors_parameterised_by/}
%endif

\subsection{Making defaults more flexible}\label{sec:defaultsignatures}
%if style == newcode
%format Rep = "GHC.Rep"
%endif

In the previous section, we saw an example of how relying too much on a type
class's default implementations can backfire. This is an unfortunately
common trend with type classes in general: Many classes try to pick
one-size-fits-all defaults that do not work well in certain scenarios, but
because Haskell allows specifying only one default per method, if the provided
default does not work for a programmer's use case, then she is forced to
implement her own implementations by hand.

In this section, we continue the trend of generalizing defaults by looking
at another language extension that \DerivingVia\ can substitute for:
\emph{default signatures}. Default signatures (a slight generalization of
default implementations) can eliminate large classes of boilerplate,
but they too are limited by the one-default-per-method restriction.
Here, we demonstrate how one can scrap uses of
\DefaultSignatures\ in favor of \DerivingVia\ and show how \DerivingVia\
can overcome the limitations of \DefaultSignatures.

%if style /= newcode
%format Pretty = "\cl{Pretty}"
%format pPrint = "\id{pPrint}"
%format Doc = "\ty{Doc}"
%format GPretty = "\cl{GPretty}"
%format genericPPrint = "\id{genericPPrint}"
%format Rep = "\ty{Rep}"
%format Generic = "\cl{Generic}"
%format Foo1 = "\ty{Foo}"
%format Foo2 = "\ty{Foo}"
%else

> data Doc
>
> stringToDoc :: String -> Doc
> stringToDoc = undefined
>
> class GPretty (f :: k -> *) where
> instance GPretty f => GPretty (M1 t m f)
> instance GPretty U1
> instance GPretty (K1 r a) -- omitting the Pretty constraint normally needed
> instance (GPretty f, GPretty g) => GPretty (f :+: g)
> instance (GPretty f, GPretty g) => GPretty (f :*: g)

> data Foo1 = Foo1
>   deriving (Generic)

> data Foo2 = Foo2
>   deriving (Show, Generic)

%endif
The typical use case for \DefaultSignatures\ is when one has a class method
that has a frequently used default implementation at a constrained type.
For instance, consider a |Pretty| class with a method |pPrint| for
pretty-printing data:

> class Pretty a where
>   pPrint :: a -> Doc

Coming up with |Pretty| instances for the vast majority of data types is repetitive
and tedious, so a common pattern is to abstract away this tedium using
generic programming libraries, such as those found in |GHC.Generics|
~\cite{gdmfh} or \Package{generics-sop}~\cite{true-sums-of-products}. For example,
using |GHC.Generics|, we can define

> genericPPrint ::
>   (Generic a, GPretty (Rep a)) => a -> Doc

%if style == newcode

> genericPPrint = undefined

%endif
The details of how |Generic|, |GPretty|, and |Rep| work are not important to
understanding the example. What is important is to note that we cannot
just add

< ??pPrint = genericPPrint

as a conventional default implementation to the |Pretty| class, because
it does not typecheck due to the extra constraints.

Before the advent of \DefaultSignatures, one had to work around this by
defining |pPrint| to be |genericPPrint| in every |Pretty| instance, as in the
examples below:

> instance Pretty Bool where
>   pPrint = genericPPrint
>
> instance Pretty a => Pretty (Maybe a) where
>   pPrint = genericPPrint

%if style == newcode

> instance (Pretty a, Pretty b) => Pretty (Either a b) where
>   pPrint = genericPPrint

%endif
%
To avoid this repetition, \DefaultSignatures allow one to provide a default
implementation of a class method using \emph{additional} constraints
on the method's type. For example:
%if style == newcode
%format Pretty = Pretty2
%format pPrint = pPrint2
%endif

> class Pretty a where
>   pPrint :: a -> Doc
>   default pPrint ::
>     (Generic a, GPretty (Rep a)) => a -> Doc
>   pPrint = genericPPrint

Now, if any instances of |Pretty| are given without an explicit definition of
|pPrint|, the default implementation is used. For this to typecheck,
the data type |a| used in the instance must satisfy the constraints
|(Generic a, GPretty (Rep a))|. Thus, we can reduce the instances above
to just

> instance Pretty Bool
> instance Pretty a => Pretty (Maybe a)

%if style == newcode

> instance (Pretty a, Pretty b) => Pretty (Either a b)

%endif
%

Although \DefaultSignatures\ remove the need for many occurrences of
boilerplate code, it also imposes a significant limitation: every class
method can have at most one default implementation. As a result,
\DefaultSignatures\ effectively endorse one default implementation as the
canonical one. But in many scenarios, there is far more than just one way to
do something. Our |pPrint| example is no exception. Instead of
|genericPPrint|, one might want to:

\begin{itemize}
 \item Leverage a |Show|-based default implementation instead of a
       |Generic|-based one,
 \item Use a different generic programming library, such as \Package{generics-sop},
       instead of |GHC.Generics|, or
 \item Use a tweaked version of |genericPPrint| that displays extra debugging
       information.
\end{itemize}

All of these are perfectly reasonable choices a programmer might want to make,
but alas, \GHC\ lets type classes bless each method with only one default.

Fortunately, \DerivingVia\ provides a convenient way of encoding default
implementations with the ability to toggle between different choices:
newtypes! For instance, we can codify two different approaches to
implementing |pPrint| as follows:
%if style /= newcode
%format GenericPPrint = "\ty{GenericPPrint}"
%format MkGenericPPrint = "\con{GenericPPrint}"
%format ShowPPrint = "\ty{ShowPPrint}"
%format MkShowPPrint = "\con{ShowPPrint}"
%format stringToDoc = "\id{stringToDoc}"
%endif

> newtype GenericPPrint a = MkGenericPPrint a
>
> instance (Generic a, GPretty (Rep a))
>     => Pretty (GenericPPrint a) where
>   pPrint (MkGenericPPrint x) = genericPPrint x
>
> newtype ShowPPrint a = MkShowPPrint a
>
> instance Show a => Pretty (ShowPPrint a) where
>   pPrint (MkShowPPrint x) = stringToDoc (show x)

With these newtypes in hand, choosing between them is as simple as changing
a single type:
%if style /= newcode
%format DataType1 = "\ty{DataType1}"
%format DataType2 = "\ty{DataType2}"
%else

> data DataType1 = MkDataType1
>   deriving (Generic)

%endif
\begin{joincode}%

> ??deriving Pretty via (GenericPPrint  DataType1)

%if style == newcode

> data DataType2 = MkDataType2
>   deriving (Show)

%endif

> ??deriving Pretty via (ShowPPrint     DataType2)

\end{joincode}
We have seen how \DerivingVia\ makes it quite simple to give names to
particular defaults, and how toggling between defaults is a matter of
choosing a name. In light of this, we believe that many current uses of
\DefaultSignatures\ ought to be removed entirely and replaced with the
\DerivingVia-based idiom presented in this section. This avoids the need
to bless one particular default and forces programmers to consider which
default is best suited to their use case, instead of blindly trusting the
type class's blessed default to always do the right thing.

An additional advantage is that it allows decoupling the definition of
such defaults from the site of the class definition. Hence, if a package
author is hesitant to add a default because that might incur an
unwanted additional dependency, nothing is lost, and the default can
simply be added in a separate package.

\subsection{Deriving via isomorphisms}\label{sec:isomorphisms}
%if style /= newcode
%format Track = "\ty{Track}"
%format MkTrack = "\con{Track}"
%format SameRepAs = "\ty{SameRepAs}"
%format MkSameRepAs = "\con{SameRepAs}"
%format Generic = "\cl{Generic}"
%format Rep = "\ty{Rep}"
%format from = "\id{from}"
%format to = "\id{to}"
%format title = "\id{title}"
%format duration = "\id{duration}"
%format Title = "\ty{Title}"
%endif
%if style == newcode

> newtype Title = MkTitle String
>   deriving Arbitrary

%endif

All of the examples presented thus far in the paper rely on deriving
through data types that have the same runtime representation as the original
data type. In the following, however, we point out that---perhaps
surprisingly---we can also derive through data types that are \emph{isomorphic},
not just representationally equal. To accomplish this feat, we rely
on techniques from generic programming.

Let us go back to \QuickCheck\ (as in Section~\ref{sec:quickcheck}) once
more and consider the data type

> data Track = MkTrack Title Duration

for which we would like to define an |Arbitrary| instance. Let us further
assume that we already have |Arbitrary| instances for both |Title| and |Duration|.

The \QuickCheck\ library defines an instance for pairs, so we could generate
values of type |(Title, Duration)|, and in essence, this is exactly what
we want. But unfortunately, the two types are not inter-|Coercible|, even
though they are isomorphic\footnote{Isomorphic in the sense that we can
define a function from |Track| to |(Title, Duration)| and vice versa.
Depending on the class we want to derive, sometimes an even weaker relationship
between the types is sufficient, but we focus on the case of isomorphism
here for reasons of space.}.

However, we can exploit the isomorphism and still get an instance for free,
and the technique we apply is quite widely applicable in similar situations.
As a first step, we declare a newtype to capture that one type is
isomorphic to another:

> newtype SameRepAs a b = MkSameRepAs a

Note that the idea here is that |a| and |b| are isomorphic in some sense,
but only |a| is used as the value of the type. So |SameRepAs a b| is
inter-|Coercible| with |a|.

We choose to witness an isomorphism between the two types via
their generic representations: if two types have
inter-|Coercible| generic representations, we can transform back and forth
using the |from| and |to| methods of the |Generic| class
from |GHC.Generics|~\cite{gdmfh}.
We can use this to define a suitable |Arbitrary| instance for |SameRepAs|:
%if style /= newcode
%format Gen = "\ty{Gen}"
%endif

> instance
>   (  Generic a, Generic b
>   ,  Coercible (Rep a ()) (Rep b ()), Arbitrary b
>   ) => Arbitrary (a `SameRepAs` b) where
>   arbitrary = MkSameRepAs . coerceViaRep <$> arbitrary
>     where
>       coerceViaRep :: b -> a
>       coerceViaRep =
>         to . (coerce :: Rep b () -> Rep a ()) . from

Here, we first use |arbitrary| to give us a generator of type
|Gen b|, then coerce
this via the generic representations into an |arbitrary| value
of type |Gen a|.

Finally, we can use the following |deriving| declarations for |Track|
to obtain the desired |Arbitrary| instance:
%if style == newcode
%format Track = Track2
%format MkTrack = MkTrack2
%format title = title2
%format duration = duration2

> data Track =
>   MkTrack
>     {  title     ::  Title
>     ,  duration  ::  Duration
>     }

%endif


> ??  deriving Generic
> ??  deriving Arbitrary
>       via (Track `SameRepAs` (String, Duration))

With this technique, we can significantly expand the ``equivalence classes''
of data types that can be used when picking suitable types to derive through.

\subsection{Retrofitting superclasses}\label{sec:superclasses}
%if style /= newcode
%format FromMonad = "\ty{FromMonad}"
%format MkFromMonad = "\con{FromMonad}"
%format Stream = "\ty{Stream}"
%format Yield = "\con{Yield}"
%format Done = "\con{Done}"
%endif

On occasion, the need arises to retrofit an existing type class with a
superclass, such as when |Monad| was changed to have |Applicative| as
a superclass (which in turn has |Functor| as a superclass).

One disadvantage of such a change is that if the primary goal is to
define the |Monad| instance for a type, one now has to write two
additional instances, for |Functor| and |Applicative|, even though
these instances are actually determined by the |Monad| instance.

With \DerivingVia, we can capture this fact as a newtype, thereby
making the process of defining such instances much less tedious:

> newtype FromMonad m a = MkFromMonad (m a)
>   deriving Monad
>
> instance Monad m => Functor (FromMonad m) where
>   fmap  =  liftM
>
> instance Monad m => Applicative (FromMonad m) where
>   pure   =  return
>   (<*>)  =  ap

Now, if we have a data type with a |Monad| instance, we can simply derive
the corresponding |Functor| and |Applicative| instances by
referring to |FromMonad|:

> data Stream a b = Done b | Yield a (Stream a b)
>   deriving (Functor, Applicative)
>     via (FromMonad (Stream a))
>
> instance Monad (Stream a) where
>   return = Done
>
>   Yield a k >>= f  =  Yield a (k >>= f)
>   Done b    >>= f  =  f b

% A similar newtype could also be defined to quickly implement the |(<>)| method
% from the |Semigroup| class in terms of an existing |Monoid| instance
% (of which |Semigroup| is a superclass).

% \alnote{Several other mechanisms have been proposed to deal with this situation.
% We should go through them and point out whether they're subsumed by this or not.}

One potentially problematic aspect remains.
Another proposal~\cite{monad-no-return} has been put forth
(but has not been implemented, as of now) to remove the |return| method from
the |Monad| class and make it a synonym for |pure| from |Applicative|.
The argument is that |return| is redundant, given that |pure| does the
same thing with a more general type signature. All other prior discussion about
the proposal aside, it should be noted that removing |return| from the |Monad|
class would prevent |FromMonad| from working, as then |Monad| instances would
not have any way to define |pure|.
~\footnote{A similar, yet somewhat weaker, argument applies to suggested changes
to relax the constraints of |liftM| and |ap| to merely |Applicative| and to
change their definitions to be identical to |fmap| and |(<*>)|, respectively.}

\subsection{Avoiding orphan instances}\label{sec:orphaninstances}

Not only can \DerivingVia\ quickly procure class instances, in some
cases, it can eliminate the need for certain instances altogether.
Haskell programmers often want to avoid \emph{orphan instances}: instances
defined in a separate module from both the type class and data types being
used. Sometimes, however, it is quite tempting to
reach for orphan instances, as in the following example adapted
from a blog post by Gonzalez~\cite{equational-reasoning-at-scale}:
%if style /= newcode
%format Plugin = "\ty{Plugin}"
%format MkPlugin = "\con{Plugin}"
%endif

> newtype Plugin = MkPlugin (IO (String -> IO ()))
>   deriving Semigroup

In order for this derived |Semigroup| instance to typecheck, there must be a
|Semigroup| instance for |IO| available. Suppose for a moment that there was
no such instance for |IO|. How could one work around this issue?

\begin{itemize}
\item One could patch the \Package{base} library to add the instance for |IO|.
      But given \Package{base}'s slow release cycle, it would be a while
      before one could actually use this instance.
\item Write an orphan instance for |IO|. This works, but is
      undesirable, as now anyone who uses |Plugin| must incur a
      possibly unwanted orphan instance.
\end{itemize}

Luckily, \DerivingVia\ presents a more convenient third option: re-use a
|Semigroup| instance from \emph{another} data type. Recall the |App|
data type from Section~\ref{sec:introducingdv} that lets us define a
|Semigroup| instance by lifting through an |Applicative| instance. As luck would
have it, |IO| already has an |Applicative| instance, so we can derive the
desired |Semigroup| instance for |Plugin| like so:
%if style == newcode
%format Plugin = Plugin2
%format MkPlugin = MkPlugin2
%endif

> newtype Plugin = MkPlugin (IO (String -> IO ()))
>  deriving Semigroup
>    via (App IO (String -> App IO ()))

% RGS: I'm leaving this off
% If, like \cite{twist-pointers}
% we wanted to sequential composion for |IO ()| rather than lifted
% behaviour all we need to do is write an adapter type
%
% > newtype Seq f = Seq (f ())
% >
% > instance Applicative f => Monoid (Seq f) where
% >   mempty :: Seq f
% >   mempty = Seq (pure ())
% >
% >   mappend :: Seq f -> Seq f -> Seq f
% >   Seq fs `mappend` Seq gs = Seq (fs *> gs)
%
% and derive via
%
% <     via (IO (String -> Seq IO))
%
% Another example from the same paper can be derived as well:
%
% < data Ptr
% <
% < newtype ParseAction a = PA (Ptr -> IO a)
% <   deriving (Functor, Applicative) via
% <     (Compose ((->) Ptr) IO)
%
Note that we have to use |App| twice in the |via| type, corresponding
to the two occurences of |IO| in the |Plugin| type. This is ok, because
|App IO| has the same representation as |IO|.
As desired, we completely bypass the need for a |Semigroup| instance for |IO|.

% %if style == newcode
%
% > instance Applicative f => Semigroup (Seq f) where
% >   (<>) = mappend
%
% %endif

\section{Related Ideas}\label{sec:related}

We have demonstrated in the previous section that \DerivingVia\ is an
extremely versatile technique, and can be used to tackle a wide variety of
problems. \DerivingVia\ also bears a resemblance to other distinct language
features, such as ML functors and explicit dictionary passing, so in this
section, we present an overview of their similarities and differences.

\subsection{ML functors}

Languages in the ML family, such as Standard ML or OCaml, provide
\emph{functors}, which are a feature of the module system that allows
writing functions from modules of one signature to modules of another
signature. In terms of functionality, functors somewhat closely resemble
\DerivingVia, as functors allow ``lifting'' of code into the module
language much like \DerivingVia\ allows lifting of code into \GHC's
deriving construct.

\subsection{Explicit dictionary passing}

The power and flexibility of \DerivingVia\ is largely due to \GHC's ability
to take a class method of a particular type and massage it into a method
of a different type. This process is almost completely abstracted away from
the user, however. A user only needs to specify the types involved, and \GHC\
will handle the rest behind the scenes.

An alternative approach, which would put more power into the hands of the
programmer, is to permit the ability to explicitly construct and pass the
normally implicit dictionary arguments corresponding to type class instances
~\cite{implicit-params-explicit}. Unlike in \DerivingVia, where going between
class instances is a process that is carefully guided by the compiler,
permitting explicit dictionary arguments would allow users to actually
coerce concrete instance values and pass them around as first-class values.
In this sense, explicit dictionary arguments could be thought of as a further
generalization of the technique that \DerivingVia\ uses.

However, explicit dictionary arguments are a considerable extension of
the language and its type system, and we feel that
to be too large a hammer for the nail we are trying to hit.
\DerivingVia\ works by means of a simple desugaring of code with some
light typechecking on top, which makes it much simpler to describe and
implement. Finally, the problem that explicit dictionaries aims to
solve---resolving ambiguity in implicit arguments---almost never arises
in \DerivingVia, as the programmer must specify all the types involved
in the process.

\section{Current status}\label{sec:status}

We have implemented \DerivingVia\ within \GHC.
Our implementation also interacts well with other \GHC\ features that were
not covered in this paper, such as kind polymorphism ~\cite{haskell-promotion},
\StandaloneDeriving, and type classes with associated type families
~\cite{associated-type-synonyms}.
However, there are still challenges remaining, which we describe
in this section.

\subsection{Quality of error messages}

The nice thing about |deriving| is that when it works, it tends to work
extremely well. When it \emph{doesn't} work, however, it can be challenging
to formulate an error message that adequately explains what went wrong. The
fundamental issue is that error messages resulting from uses of |deriving|
are usually rooted in \emph{generated} code, and pointing to code that the
user did not write in error messages can lead to a confusing
debugging experience.

Fortunately, we have found in our experience that the quality of
\DerivingVia-related error messages is overall on the positive side. \GHC\
has already invested significant effort into making type errors involving
|Coercible| to be easily digestible by programmers, so \DerivingVia\
benefits from this work. For instance, if one inadvertently tries to
derive through a type that is not inter-|Coercible| with the original
data type, such as in the following example:
%if style /= newcode
%format UhOh = "\ty{UhOh}"
%format MkUhOh = "\con{UhOh}"
%endif

< newtype UhOh = MkUhOh Char deriving Ord via Int

Then GHC will tell you exactly that, in plain language:
\begingroup
\invisiblecomments

< -- \textbullet\ Couldn't match representation of type |Char| with that of |Int|
< -- \phantom{\textbullet\ }\quad arising from the coercion of the method |compare|
< -- \phantom{\textbullet\ }\qquad from type `|Int -> Int -> Ordering|'
< -- \phantom{\textbullet\ }\qquad to type `|UhOh -> UhOh -> Ordering|'

\endgroup

That is not to say that every error message is this straightforward. There
is are some scenarios that produce less-than-ideal errors, such as this:

< newtype Foo a = MkFoo (Maybe a) deriving Ord via a

\begingroup
\invisiblecomments

< -- \textbullet\ Occurs check: cannot construct the infinite type: |a ~ Maybe a|
< -- \phantom{\textbullet\ }\quad arising from the coercion of the method `|compare|'
< -- \phantom{\textbullet\ }\qquad from type `|a -> a -> Ordering|'
< -- \phantom{\textbullet\ }\qquad to type `|Foo a -> Foo a -> Ordering|'

\endgroup

The real problem is that |a| and |Maybe a| do not have the same representation
at runtime, but the error does not make this obvious.
% \alnote{This seems similar to the question I brought up during the call
% yesterday, so unless we find a better place, this might be a good point to
% discuss the example of empty type classes and why we don't want to impose
% a specific check for representation-equivalence that is not induced by the
% class methods.}
It is possible that one
could add an \emph{ad hoc} check for this class of programs, but there are
likely many more tricky corner cases lurking around the corner given that
one can put anything after |via|.

We do not propose a solution to this problem here, but instead note that issues
with \DerivingVia\ error quality are ultimately issues with |coerce| error
quality, given that the error messages are a result of |coerce| failing to
typecheck. It is likely that investing more effort into making |coerce|'s
error messages easier to understand would benefit \DerivingVia\ as well.

\subsection{Multi-Parameter Type Classes}

GHC extends Haskell by permitting type classes with more than one parameter.
Multi-parameter type classes are extremely common in modern Haskell, to the
point where we assumed the existence of them in Section \ref{sec:kinds}
without further mention. However, multi-parameter type classes pose an
intriguing design question when combined with \DerivingVia\ and
\StandaloneDeriving, another \GHC feature that allows one to write
|deriving| declarations independently of a data type.

For example, one can write the following instance using
\StandaloneDeriving:

%if style == newcode
%format Triple = Triple_
%format A = A3
%format B = B3
%format C = C3
%format MkA = MkA3
%format MkB = MkB3
%format MkC = MkC3

> instance Triple A B () where
>   triple = undefined

%else
%format A = "\ty{A}"
%format B = "\ty{B}"
%format C = "\ty{C}"
%format MkA = "\con{A}"
%format MkB = "\con{B}"
%format MkC = "\con{C}"
%format Triple = "\cl{Triple}"
%format triple = "\id{triple}"
%endif

> class Triple a b c where
>   triple :: (a, b, c)
> instance Triple () () () where
>   triple = ((), (), ())
>
> newtype A = MkA ()
> newtype B = MkB ()
> newtype C = MkC ()
>
> deriving via () instance Triple A B C

However, the code it generates is somewhat surprising. Instead of reusing
the |Triple () () ()| instance in the derived instance, it will attempt
to reuse an instance for |Triple A B ()|. This is because, by convention,
\StandaloneDeriving\ will only ever coerce through the \emph{last}
argument of a class. That is because the standalone instance above would be
the same as if a user had written:
%if style == newcode
%format C = C4
%format MkC = MkC4
%endif

> newtype C = MkC () deriving (Triple A B) via ()

This consistency is perhaps a bit limiting in this context, where we have
multiple arguments to |C| that one could ``derive through''. But it is not
clear how GHC would figure out which of these arguments to |C|
should be derived through, as there seven different combinations to choose
from! It is possible that another syntax would need to be devised to
allow users to specify which arguments should be coerced to avoid this
ambiguity.

% %if style == newcode
% %format Triple = Triple_
% %format A = A3
% %format B = B3
% %format C = C3
% %else
% %format A = "\ty{A}"
% %format B = "\ty{B}"
% %format C = "\ty{C}"
% %format MkA = "\con{A}"
% %format MkB = "\con{B}"
% %format MkC = "\con{C}"
% %endif
%
% > class Triple a b c where triple :: (a, b, c)
% > instance Triple () () () where triple = ((), (), ())
%
% It is sensible to use this instance to derive new instances for types
% representationally equal to unit. Certainly, it works for the final
% parameter:
%
% > newtype A = MkA ()
% > newtype B = MkB ()
% > newtype C = MkC ()
% >
% > deriving via () instance Triple () () A
% > deriving via () instance Triple () () B
% > deriving via () instance Triple () () C
%
% But can we derive the instance |Triple A B C|? Not readily, the
% instance used is the instance being derived with the |via|-type as the
% last parameter. The following is forced to derive via the instance
% |Triple A B ??|:
%
% < deriving via ?? instance Triple A B C
%
% But we can derive |Triple A B C| via |Triple () () ()| with
% |newtype|ing where a, b, c will be instantiated to units.
%
% > newtype Via3 a b c = Via3 c
% >
% > instance (Triple a b c, Coercible (a, b) (a', b')) => Triple a' b' (Via3 a b c) where
% >   triple :: (a', b', Via3 a b c)
% >   triple = coerce (triple @a @b @c)
% >
% > deriving via (Via3 () () ()) instance Triple A B C
% > deriving via (Via3 () () ()) instance Triple A A A
% > deriving via (Via3 () () ()) instance Triple C B A
%
% This author (Baldur) believes it impossible to derive instances like
% |Sieve Arr Identity| using the |Sieve (->) Identity| dictionary
%
% > class (Profunctor pro, Functor f) => Sieve pro f | pro -> f where
% >   sieve :: pro a b -> (a -> f b)
% >
% > instance Sieve (->) Identity where
% >   sieve :: (a -> b) -> (a -> Identity b)
% >   sieve f a = Identity (f a)
% >
% > newtype Arr a b = Arr (a -> b) deriving newtype Profunctor
%
% @DerivingVia@ requires us to derive it via the |Sieve (->) ???|
%  dictionary but due to the functional dependencies (|pro -> f|) |???|
%  must be fully determined by |(->)|.
%
% The author proposes a more general form as future work
%
% < instance Sieve Arr  Identity
% <      via Sieve (->) Identity
%
% Another use for this is something like
%
% < class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
% <   _Cons :: Prism s t (a,s) (b,t)
% <
% < instance Cons [a] [b] a b
%
% and deriving an instance for |Cons (ZipList a) (ZipList b) a b|.

\section{Conclusions}\label{sec:conclusions}

In this paper, we have introduced the \DerivingVia\ language
extension, explained how it is implemented, and shown a wide
variety of use cases. We believe that \DerivingVia\ has
the potential to dramatically change the way we write instances,
as it encourages giving names to recurring patterns and reusing them
where needed. It is our feeling that most instance
declarations that occur in the wild can actually be derived
by using a pattern that deserves to be known and named,
and that instances defined manually should become an anti-pattern
in all but some rare situations.

\subsection*{Acknowledgements}

We would like to thank Richard Eisenberg for his
feedback on Section~\ref{sec:typevariablescoping}, as well as
the first author's former colleagues
at Standard Chartered Bank for their feedback.

\bibliographystyle{includes/ACM-Reference-Format}

\bibliography{refs}

\end{document}

