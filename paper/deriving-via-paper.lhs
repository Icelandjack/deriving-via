\documentclass[%
  %format=acmsmall,% 1-column format used by PACMPL
  format=sigplan,% 2-column format used by other SIGPLAN conferences
  review=true,% review mode / enables line numbers
  anonymous=false,% enable to remove author names
  timestamp=true,% adds timestamps to the pages
  authordraft=true,% development mode
  ]{acmart}

% disable the watermark
\SetWatermarkText{}%

\usepackage{booktabs}
\usepackage{hyperref}

% comments
\colorlet{bbnote}{blue}
\colorlet{alnote}{orange}
\colorlet{rsnote}{red}
\newcommand\bbnote[1]{\footnote{\color{bbnote}[BB: #1]}}
\newcommand\alnote[1]{\footnote{\color{alnote}[AL: #1]}}
\newcommand\rsnote[1]{\footnote{\color{rsnote}[RS: #1]}}

%include general.fmt

\setcopyright{rightsretained}

% Data to be filled in later:

%\acmDOI{...}
%\acmISBN{...}
%\acmConference[acro]{name}{month/year}{location}
%\acmYear{...}
%\copyrightyear{...}
%\acmPrice{...}

%if style == newcode

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DefaultSignatures #-}
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
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> import Control.Applicative
> import Control.Monad
> import Control.Monad.Identity
> import Control.Monad.ST
> import Data.Coerce
> import Data.Profunctor
> import Data.Proxy
> import GHC.Generics hiding (C, C1)
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
%format overlapping =
%else

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

%format overlapping = " {-# OVERLAPPING #-} "
%endif

\begin{document}

\title{Deriving Via}
\subtitle{or, How to Turn Hand-Written Instances into an Anti-Pattern}
\author{Baldur Blöndal}
\authornote{It would be nice to have a title, something that (like "copy-paste") invokes the sense of boilerplate that can be textually substituted}
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
Introduces a deriving strategy that

Subsumes generalized |newtype| deriving.

We present a new Haskell language extension that miraculously solves
all problems in generic programming that ever existed.

I want to make it very clear that this works perfectly for |data|, not
limited to |newtype|.

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

``These types we write down they're not just names for data
representations in memory, they're tags that queue in mathematical
structures that we exploit.''\footnote{Taken from unknown position:
https://www.youtube.com/watch?v=3U3lV5VPmOU}

\section{Introduction}
%if style /= newcode
%format via = "\keyw{via}"
%format Foo = "\ty{Foo}"
%format MkFoo = "\con{MkFoo}"
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
%endif

In Haskell, type classes capture common interfaces. When we declare a
datatype to be an instance of a type class, we explain how it
implements the interface by providing implementations of all the
methods of the class.

Quite often, however, these implementations are not unrelated but the
application of a common pattern. For example, in the @base@ package,
we can find the following |Monoid| instances:

> instance Monoid a => Monoid2 (IO a) where
>
>   mempty2  = pure mempty
>   mappend2 = liftA2 mappend

> instance Monoid a => Monoid2 (ST s a) where
>
>   mempty2  = pure mempty
>   mappend2 = liftA2 mappend

While the definition as given is specific to |IO a| and |ST s a|, the
principle is not: we can always lift a monoid |a| over a type
constructor |f| as long as |f| is an applicative (or |Biapplicative|)
functor. This is the case for |IO|, but it is also true for all the
other applicative functors out there.  \alnote{There was a reference
to Conor McBride here, mentioning ``routine programming'' and
\cite{applicative-programming-with-effects}. We might want to reinsert
this.}

\subsection{The problem: capturing general instance rules}

It is tempting to avoid this obvious repetition by defining an
instance for all applicatives, in one fell swoop.

> instance (Applicative f, Monoid a) => Monoid2 (f a) where
>
>   mempty2 :: f a
>   mempty2 = pure mempty
>
>   mappend2 :: f a -> f a -> f a
>   mappend2 = liftA2 mappend

Unfortunately, this general instance is undesirable for several
reasons:

First, it overlaps with all other instances that match |Monoid (f
a)|. Instance resolution will match the instance head before even
considering the context, even if |f| is not applicative. Consider

> newtype Endo a = MkEndo (a -> a) -- Data.Monoid

|Endo| is not even a |Functor| yet it admits a perfectly valid monoid
instance that overlaps with the lifted instance above

> instance overlapping Monoid2 (Endo a) where
>   mempty2 = MkEndo id
>   mappend2 (MkEndo f) (MkEndo g) = MkEndo (f . g)

and while we can make GHC accept it nevertheless, the presence of
overlapping instances often leads to undesirable behavior.\alnote{The
original enumeration mentioned another point which I do not understand
right now, so I omitted it for the time being: ``Structure of the |f|
is often considered more significant that that of |x|.''  Much of this
is stolen from Conor:
https://personal.cis.strath.ac.uk/conor.mcbride/so-pigworker.pdf}

Second, even if |f| is an applicative functor the lifted monoid
instance may not be the only one, or the one we want to use.  Most
notably, lists are the \emph{free monoid} (the most ‘fundemental’
monoid), and their monoid instance looks as follows:

> instance Monoid2 [a] where
>   mempty2   =  []
>   mappend2  =  (++)

This instance does not coincide with the instantiation of the rule above
(and in particular, imposes no constraint on |a| to be a monoid). In fact,
lists are an example of applying a different rule for defining monoids
based on an |Alternative| instance for the type constructor:

> instance Alternative f => Monoid3 (f a) where
>   mempty3   =  empty
>   mappend3  =  (<|>)

But clearly, we could not have both general instances in our program at the
same time. The way that Haskell instance search works is to only look at the
instance head when choosing an instance, and then to commit and never backtrack.
So even with overlapping instances enabled, we could not define all the rules
we wanted to in this way.

Currently, the only viable workaround is to define individual
instances for each datatype in spirit of the |Monoid (IO a)| instance
shown in the beginning. But as we shall see in the remainder of this
paper, there are many such rules, and while the approach of defining
individual instances in a uniform format may be somewhat feasible for
classes that consists of just one or two methods, it becomes extremely
tedious for classes with many methods.

For example, there is a way to lift a |Num| instance through any applicative
functor (and similarly, there are ways to lift |Floating| and |Fractional|):
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
>
>   (+), (-), (*) :: f a -> f a -> f a
>   (+) = liftA2 (+)
>   (-) = liftA2 (-)
>   (*) = liftA2 (*)
>
>   negate, abs, signum :: f a -> f a
>   negate = liftA negate
>   abs    = liftA abs
>   signum = liftA signum
>
>   fromInteger :: Integer -> f a
>   fromInteger = pure . fromInteger

%}
Defining such a boilerplate instance manually for a concrete type constructor
is so annoying that Conal Elliott has introduced a preprocessor for this particular
use case several years ago.\footnote{https://hackage.haskell.org/package/applicative-numbers}
\alnote{Should ideally be replaced with a proper citation.}
\alnote{And Conal is by no means alone: see
https://gist.github.com/Icelandjack/e1ddefb0d5a79617a81ee98c49fbbdc4\#a-lot-of-things-we-can-find-with-define
We cannot put a gist dump like this into a paper. We might want to make a selection,
or just describe the situation in words.}

\subsection{Our solution: newtypes and a new form of deriving}
%if style /= newcode
%format App = "\ty{App}"
%format Alt = "\ty{Alt}"
%format MkApp = "\con{App}"
%format MkAlt = "\con{Alt}"
%endif

We solve the above problem of capturing general rules for defining
new instances by using a known mechanism: |newtype|s.

We can turn a problematic generic and overlapping instance into an
entirely unproblematic (but not yet useful) one by defining a |newtype|
and wrapping the instance head in it\alnote{According to Baldur, Conor
calls these ``adaptors''. Perhaps we should consider this terminology too.}:

> newtype App f a = MkApp (f a)
>
> instance (Applicative f, Monoid4 a) => Monoid4 (App f a) where
>
>   mempty4 :: App f a
>   mempty4 = MkApp (pure mempty4)
>
>   mappend4 :: App f a -> App f a -> App f a
>   mappend4 (MkApp f) (MkApp g) = MkApp (liftA2 mappend4 f g)

Since GHC 8.4, we also need a |Semigroup| instance, because it just became
a superclass of |Monoid|\footnote{See Section~\ref{sec:superclasses} for
a more detailed discussion of this aspect.}:

> instance (Applicative f, Semigroup a) => Semigroup (App f a) where
>
>   (<>) :: App f a -> App f a -> App f a
>   MkApp f <> MkApp g = MkApp (liftA2 (<>) f g)

Such instance definitions can be made more concise by employing the
existing language extension @GeneralizedNewtypeDeriving@ which allows
us to make an instance on the underlying type available on the wrapped
type. This is always possible because a |newtype|-wrapped type is
guaranteed to have the same representation as the underlying type
\cite{zero-cost-coercions}\bbnote{|Alt| is found in |Data.Monoid|.}

> newtype Alt f a = MkAlt (f a)
>   deriving (Functor, Applicative, Alternative)
>
> instance Alternative f => Monoid4 (Alt f a) where
>   mempty4   =  empty
>   mappend4  =  (<|>)
>
> instance Alternative f => Semigroup (Alt f a) where
>   (<>) = mappend4

We now introduce a new style of |deriving| that allows us to instruct
the compiler to use such a newtype-derived rule as the basis of a new
instance definition.

For example, using the @StandaloneDeriving@ language extension, the
|Monoid| instances for |IO| and |[]| could be written as follows:

> deriving via (App IO a) instance Monoid4 a => Monoid4 (IO a)
> deriving via (Alt [] a) instance Monoid4 [a]

Here, |via| is a new language construct that explains \emph{how} GHC
should derive the instance, namely be reusing the instance already
available for the given type. It should be easy to see why this works:
due to the use of a |newtype|, |App IO a| has the same internal
representation as |IO a|, and |Alt [] a| has the same representation
as |[a]|, and any instance available on one type can be made to work
on a representationally equal type as well.

\subsection{Structure of the paper}

In the rest of this paper, we will spell out this idea in more detail.

In Section~\ref{sec:examples} we will look at several more useful examples of
instance rules that can be captured and applied using |newtype|s. In
particular, we will see that our new language extension subsumes
@GeneralizedNewtypeDeriving@.
%
In Section~\ref{sec:typechecking}, we explain how the language extension works
from a typechecking perspective and analyze the code that it generates.
%
Section~\ref{sec:advanced} shows some further uses cases that are more advanced and perhaps
somewhat surprising.

We discuss related work in Section~\ref{sec:related} and conclude
in Section~\ref{sec:conclusions}.

The extension is fully implemented in a GHC branch and all the code presented
in this paper compiles, so it will hopefully be available in a near future
release of GHC. \alnote{We should make sure that we don't end up promising
something that isn't true, but I think it's likely we'll have a full implementation
by the time the paper is published, given that we have an almost working one
already.}

\section{Examples}\label{sec:examples}
%if style /= newcode
%format FromMonad = "\ty{FromMonad}"
%format MkFromMonad = "\con{FromMonad}"
%format Stream = "\ty{Stream}"
%format Yield = "\con{Yield}"
%format Done = "\con{Done}"
%endif

\subsection{Defining superclasses}\label{sec:superclasses}

When the ``Applicative Monad Proposal'' was introduced and turned |Monad|
from a plain type class into one that has |Applicative| as a superclass
(which in turn has |Functor| as a superclass), one counter-argument against
the change was that someone who wants to primarily wants to declare a~|Monad|
instance is now required to define two extra instances for~|Functor|
and~|Applicative| -- both of which are usually boilerplate, because they can
be defined from the~|Monad| instance.

We can capture these rules as follows:

> newtype FromMonad m a = MkFromMonad (m a)
>   deriving Monad
>
> instance Monad m => Functor (FromMonad m) where
>   fmap  =  liftM
>
> instance Monad m => Applicative (FromMonad m) where
>   pure   =  return
>   (<*>)  =  ap

The wrapper type |FromMonad| serves the purpose of giving a name
to the patterns. The two instance make it precise what it means
to define the |Functor| and |Applicative| instances in terms of
the monad instance.

If we now have a datatype with a monad instance, we can simply derive
the |Functor| and |Applicative| instances by referring to |FromMonad|:

> data Stream a b = Done b | Yield a (Stream a b)
>   deriving (Functor, Applicative)
>     via (FromMonad (Stream a))
>
> instance Monad (Stream a) where
>
>   return = Done
>
>   Yield a k >>= f  =  Yield a (k >>= f)
>   Done b    >>= f  =  f b

A similar rule could also be added to define the |(<>)| of the |Semigroup|
class in terms of an existing |Monoid| instance.

\alnote{Several other mechanisms have been proposed to deal with this situation.
We should go through them and point out whether they're subsumed by this or not.}

One potentially problematic aspect remains. Another proposal that has been made
but (as of now) not been accepted, namely to remove the |return| method from
the |Monad| class. The argument is that it is redundant given the presence of
|pure| in |Applicative|. All other points that have been raised about this
proposal aside, it should be noted that removing |return| from the |Monad|
class would prevent the above scheme from working. A similar, yet somewhat
weaker, argument applies to suggested changes to relax the constraints of
|liftM| and |ap| to merely |Applicative| and change their definitions to be
identical to |fmap| and |(<*>)|, respectively.

\subsection{QuickCheck}\label{sec:quickcheck}
%if style /= newcode
%format Arbitrary = "\cl{Arbitrary}"
%format arbitrary = "\id{arbitrary}"
%format shrink = "\id{shrink}"
%endif

QuickCheck~\cite{quickcheck} is a well-known Haskell library for randomized
property-based testing.  At the core of QuickCheck's test case generation
functionality is the |Arbitrary| class. Its primary method |arbitrary|
describes how to generate suitable random values of a given size and type. It
also has a method |shrink| that is used to try to shrink failing
counterexamples of test properties.

Many standard Haskell types such as |Int| or lists are already an instance of
|Arbitrary|. This is sometimes very convenient, because many properties
involving these types can be quick-checked without any extra work.

On the other hand, sometimes there are additional constraints imposed on the
actual values of a type that are not sufficiently expressed in their types.
Depending on the context and the situation, we might want to guarantee positive
integers, or non-empty lists, or even sorted lists.

The QuickCheck library provides a mechanism of newtype-based \emph{modifiers}
for this purpose. For example, QuickCheck provides:
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
in an extra constructor, and the type and constructor come from a specific library.
An implementation detail (the choice of testing library) leaks into the data model
of an application. While we might be willing to use domain-specific newtypes for
added type safety, such as |Age| or |Duration|, we might not be happy to add
QuickCheck modifiers everywhere. And what if we wanted more than one modifier?
And what if other libraries export their own set of modifiers as well? We certainly
do not want to change the actual definition of our datatypes (and corresponding
code) whenever we start using a new library.

With |deriving via|, we have the option to reuse the existing infrastructure of
modifiers without paying the price of cluttering up our datatype definitions.
We can choose an actual domain-specific newtype such as

> newtype Duration = MkDuration Int -- in seconds

and now specify exactly how we want to derive |Arbitrary| for this. The simplest
option is to derive via |Int| itself:

>   deriving Arbitrary via Int

This declaration has exactly the same effect as using the
@GeneralizedNewtypeDeriving@ extension to derive the instance: because
|Int| and |Duration| have the same run-time representation, we can reuse
the instance for |Int|, but this allows negative durations.

If we want to restrict ourselves to non-negative durations, we replace this by
%if style == newcode
%format Duration = Duration2
%format MkDuration = MkDuration2

> newtype Duration = MkDuration Int

%endif

>   deriving Arbitrary via (NonNegative Int)

and now we get the |Arbitrary| instance for non-negative integers. Only the
|deriving| clause changes, not the datatype itself. If we later decide we want
only positive integers as durations, we replace |NonNegative| with |Positive|
in the |deriving| clause. Again, the datatype itself is unaffected. In particular,
we do not have to change any constructor names anywhere in our code.

\subsection{Composition}
%if style /= newcode
%format Large = "\ty{Large}"
%format MkLarge = "\con{Large}"
%format getLarge = "\id{getLarge}"
%endif

Multiple modifiers can be combined. For example, there is another modifier
called |Large| that will scale up integral values being produced by a generator
in size. It is defined as

> newtype Large a = MkLarge { getLarge :: a }

%if style == newcode

>   deriving (Eq, Ord, Num) via a

%endif
with an instance

> instance (Integral a, Bounded a) => Arbitrary (Large a)

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

>   deriving Arbitrary via (NonNegative (Large Int))

The types |NonNegative (Large Int)| and |Duration| still share
the same run-time representation (namely that of~|Int|), so
the instance can be reused.

\subsection{Adding new modifiers}

Of course, we can add add our own modifiers if the set of predefined modifiers
is not sufficient. For example, it is difficult to provide a completely generic
|Arbitrary| instance that works for all datatypes, simply because there are too
many assumptions about what makes good test data that need to be taken into account.

But for certain subclasses of datatypes, there are quite reasonable strategies of
coming up with a generic instance. For example, for enumeration type, one reasonable
strategy is to simply desire a uniform distribution of the finite set of values.

QuickCheck even offers such a generator, but it does not expose it as a |newtype|
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
We can do so by using a modifier that has extra arguments, and using
the extra arguments in the associated |Arbitrary| instance.

An extreme case that also makes use from type-level programming features
in GHC is a modifier that allows us to specify a lower and an upper bound
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

> newtype Between (l :: Nat) (u :: Nat) = MkBetween Int
>
> instance (KnownNat l, KnownNat u)
>   => Arbitrary (Between l u) where
>   arbitrary =  MkBetween <$> choose
>                  (  fromIntegral (natVal (MkProxy (TYAPP l)))
>                  ,  fromIntegral (natVal (MkProxy (TYAPP u))))

We can then equip an application-specific type for years with
a generator implementing respecting a plausible range:

> newtype Year = MkYear Int
>   deriving Show
>   deriving Arbitrary via (Between 1900 2100)

%if style == newcode

> deriving instance Show (Between l u)

%endif

\section{Typechecking}\label{sec:typechecking}

Seeing enough examples of |deriving via| can give the impression that it is
a somewhat magical feature. In this section, we aim to explain the magic
underlying |deriving via| by giving a more precise description of:
\begin{itemize}
 \item How |deriving via| clauses are typechecked
 \item What code |deriving via| generates behind the scenes
 \item How to determine the scoping of type variables in |deriving via| clauses
\end{itemize}

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
implementation of these ideas in GHC does so.

\subsection{Well typed uses of |deriving via|}

|deriving via| grants the programmer the ability to put extra types in her programs,
but the flip side to this is that it's possible for her to accidentally put total nonsense
into a |deriving via| clause, such as:

< newtype S = S Char
<   deriving Eq via Maybe

In this section, we will describe a general algorithm for when a |deriving via| clause should
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
%endif

< data D (sub d 1) DOTS (sub d m)
<   deriving (C (sub c 1) DOTS (sub c n)) via (V (sub v 1) DOTS (sub v p))

In order for this declaration to typecheck, we must check the \textit{kinds} of each type.
In particular, the following conditions must hold:

\begin{enumerate}
 \item
   |C (sub c 1) DOTS (sub c n)| must have kind
   |(((sub k 1) -> ... -> (sub k r) -> *) -> Constraint)| for some kinds
   |(sub k 1), DOTS, (sub k r)|.
   This because the instance we must generate:

< instance C (sub c 1) DOTS (sub c n) (D (sub d 1) DOTS (sub d i)) where DOTS

   Requires that we apply |C (sub c 1) DOTS (sub c n)| to another type
   |D (sub d 1) DOTS (sub d i)| (more on what
   |(sub d i)| is in a moment).
   Therefore, it would be nonsense to try to derive an instance of |C (sub c 1) DOTS (sub c n)|
   if it had kind, say, |Constraint|.

 \item
   The kinds of |C (sub c 1) DOTS (sub c n)|,
   |V (sub v 1) DOTS (sub v n)|, and |D (sub d 1) ... (sub d i)| must all unify.
   This check would rule out the earlier example of |deriving Eq via Maybe|, as it does
   not even make sense to talk about
   reusing the |Eq| instance for |Maybe|---which is of kind |(* -> *)|---as |Eq| instances
   only make sense for types of kind |*|.
\end{enumerate}

\subsubsection{Eta-reducing the data type}

Note that the conditions above, |D (sub d 1) DOTS (sub d i)| (for some |i|), instead of
|D (sub d 1) DOTS (sub d m)|. That is because in general, the kind of
|C (sub c 1) DOTS (sub c n)| is allowed to be different from the kind of
|D (sub d 1) DOTS (sub d m)|! For instance, the following example is perfectly legitimate:

< class Functor (f :: * -> *) where ...
<
< data Foo a = MkFoo a a
<   deriving Functor

Despite the fact that |Foo a| has kind |*| and the argument to |Functor|
has kind |(* -> *)|. This is
because the code that actually gets generated has the following shape:

< instance Functor Foo where ...

To put it differently, we have \textit{eta-reduced} away the |a| in |Foo a| before applying
|Functor| to it. The power to eta-reduce variables from the data types is part of what
makes |deriving| clauses so flexible.

To determine how many variables to eta-reduce,
we must examine the kind of
|C (sub c 1) DOTS (sub c n)|, which by constraint (1) is of the form
|(((sub k 1) -> ... -> (sub k r) -> *) -> Constraint)| for some kinds
|(sub k 1), DOTS, (sub k r)|. Then the number of variables to eta-reduce is simply $r$,
so to compute the $i$ in |D (sub d 1) DOTS (sub d i)|, we take $i = m - r$.

This is better explained by example, so consider the following two scenarios,
both of which typecheck:

< newtype A a = A a deriving Eq      via (Identity a)
< newtype B b = B b deriving Functor via Identity

In the first example, we have the class |Eq|, which is of kind |* -> Constraint|. The argument
to |Eq|, which is of kind |*|, does not require that we eta-reduce any variables. As a result,
we check that |A a| is of kind |*|, which is the case.

In the second example, we have the class |Functor|, which is of kind |(* -> *) -> Constraint|.
The argument to |Functor| is of kind |(* -> *)|, which requires that we eta-reduce one variable
from |B b| to obtain |B|. We then check that |B| is kind of |(* -> *)|, which is true.

\subsection{Code generation}

Once the typechecker has ascertained that a |via| type is fully compatibly with the data type
and the class for which an instance is being derived, GHC proceeds with generating the code
for the instance itself. This generated code is then fed \textit{back} into the typechecker,
which acts as a final sanity check that GHC is doing the right thing under the hood.

\subsubsection{@GeneralizedNewtypeDeriving@} \label{sec:gnd}

The process by which |deriving via| generates code is heavily based off of the approach that
the @GeneralizedNewtypeDeriving@ takes, so it is informative to first explain how
@GeneralizedNewtypeDeriving@ works. From there, |deriving via| is a straightforward
generalization---so much so that |deriving via| could be thought of as
"generalized @GeneralizedNewtypeDeriving@".

Our running example in this section will be the newtype |Age|, which is a simple
wrapper around |Int| (which we will call the \textit{representation type}):

%if style /= newcode
%format Age = "\ty{Age}"
%format MkAge = "\con{MkAge}"
%endif

> newtype Age = MkAge Int
>   deriving Enum

A na{\"i}ve way to generate code would be to manually wrap and unwrap the |MkAge| constructor
wherever necessary, such as in the code below:

< instance Enum Age where
<   toEnum i = MkAge (toEnum i)
<   fromEnum (MkAge x) = fromEnum x
<   enumFrom (MkAge x) = map MkAge (enumFrom x)

This works, but is somewhat unsatisfying. After all, a newtype is intended to be a zero-cost
abstraction that acts identically to its representation type at runtime. Accordingly, any
function that mentions a newtype in its type signature should be able to be converted to
a new function with all occurrences of the newtype in the type signature replaced with the
representation type, and moreover, that new function should behave identically to the old one
at runtime.

Unfortunately, the implementation of |enumFrom| may not uphold this guarantee. While wrapping
and unwrapping the |MkAge| constructor is certain to be a no-op, the |map| function is
definitely \textit{not} a no-op, as it must walk the length of a list. But the fact that we
need to call |map| in the first place feels rather silly, as all we are doing is wrapping
a newtype at each element.

Luckily, there is a convenient solution to this problem: the |coerce| function from
\cite{zero-cost-coercions}:
%if style /= newcode
%format Coercible = "\protect\cl{Coercible}"
%endif

< coerce :: Coercible a b => a -> b

Operationally, |coerce| can be thought of as behaving like its wily cousin, |unsafeCoerce|,
which takes a value of one type as casts it to a value at a another type. Unlike |unsafeCoerce|,
which can break programs if used carelessly, |coerce| is completely type-safe due to its
use of the |Coercible| constraint. We will explain |Coercible| in more detail later, but for now,
it suffices to say that a |Coercible a b| constraint witnesses the fact that two types |a|
and |b| have the same representation at runtime, and thus any value of type |a| can be
casted to type |b|.

Armed with |coerce|, we can show what code @GeneralizedNewtypeDeriving@ would actually
generate for the |Enum Age| instance above:

< instance Enum Age where
<   toEnum = coerce (toEnum :: Int -> Int) :: Int -> Age
<   fromEnum = coerce (fromEnum :: Int -> Int) :: Age -> Int
<   enumFrom = coerce (enumFrom :: Int -> [Int]) :: Age -> [Age]

Now we have a strong guarantee that the |Enum| instance for |Age| has exactly the same
runtime characteristics as the instance for |Int|. As an added benefit, the code ends up
being simpler, as every method can be implemented as a straightforward application of
|coerce|. The only interesting part is generating the two type signatures: one for the
representation type, and one for the newtype.

\subsubsection{The |Coercible| constraint} \label{sec:coercible}

A |Coercible| constraint can be thought of as evidence that GHC can use to
cast between two types. |Coercible| is not a type class, so it is impossible to write
a |Coercible| instance by hand. Instead, GHC can generate and solve |Coercible| constraints
automatically as part of its built-in constraint solver, much like it can solve equality
constraints. (Indeed, |Coercible| can be thought of as a broader notion of equality among
types.)

As mentioned in the previous section, a newtype can be safely cast to and from its
representation type, so GHC treats them as inter-|Coercible|. Continuing our earlier example,
this would mean that GHC would be able to conclude that:

< instance Coercible Age Int
< instance Coercible Int Age

But this is not all that |Coercible| is capable of. A key property is that GHC's constraint
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
\textit{between newtypes}. For instance, if we have these two newtypes:

> newtype A a = A [a]
> newtype B = B [Int]

Then GHC is able to conclude that |Coercible (A Int) B| holds, because we have the following
|Coercible| rules:

< instance Coercible (A Int) [Int]
< instance Coercible [Int] B

Therefore, by the transitivity of |Coercible|, we have |Coercible (A Int) B|. |deriving via|
in particular makes heavy use of the transitivity of |Coercible|, as we will
see momentarily.

\subsubsection{From @GeneralizedNewtypeDeriving@ to |deriving via|}

As we saw in section \ref{sec:gnd}, the code which @GeneralizedNewtypeDeriving@ generates
relies on |coerce| to do the heavy lifting. In this section, we will generalize this
technique slightly to give us a way to generate code for |deriving via|.

Recall that the following instance, which is derived through @GeneralizedNewtypeDeriving@:

< newtype Age = MkAge Int
<   deriving Enum

Generates the following code for |enumFrom|:

< instance Enum Age where
<   enumFrom = coerce (enumFrom :: Int -> [Int]) :: Age -> [Age]

Here, there are two crucially important types: the representation type, |Int|, and the
original newtype itself, |Age|. The implementation of |enumFrom| simply sets up an
invocation of |coerce enumFrom|, with explicit type annotations to indicate that we should
reuse the existing |enumFrom| implementation for |Int| and reappropriate it for |Age.|

The only difference in the code that @GeneralizedNewtypeDeriving@ and |deriving via| generate
is that in the former strategy, GHC always picks the representation type for you, but in
|deriving via|, the \textit{user} has the power to choose this type. For example,
if a programmer had written this instead:

< newtype T = T Int
< instance Enum T where DOTS
<
< newtype Age = MkAge Int
<   deriving Enum via T

Then the following code would be generated:

< instance Enum Age where
<   enumFrom = coerce (enumFrom :: T -> [T]) :: Age -> [Age]

This time, GHC |coerce|s from an |enumFrom| implementation for |T| (the |via| type) to
an implementation for |Age|. (Recall from section \ref{sec:coercible} that this is
possible since we can |coerce| transitivity from |T| to |Int| to |Age|).

Now we can see why the instances that |deriving via| can generate are a strict superset of
those that @GeneralizedNewtypeDeriving@ can generate. For instance, our earlier
@GeneralizedNewtypeDeriving@ example:

< newtype Age = MkAge Int
<   deriving Enum

Could equivalently have been written using |deriving via| like so:

< newtype Age = MkAge Int
<   deriving Enum via Int

\subsection{Type variable scoping}

In the remainder of this section, we will present an overview of how type
variables are bound in |deriving via| clauses, and over what types they scope.
|deriving via| introduces a new place where types can go, and more importantly,
it introduces a new place where type variables can be \textit{quantified}, so
it takes some amount of care to devise a consistent treatment for it.

\subsubsection{Binding sites}

Consider the following example:
%if style /= newcode
%format Bar = "\cl{Bar}"
%format Baz = "\cl{Baz}"
%format MkFoo = DOTS
%endif

> data Foo a = MkFoo
>   deriving (Baz a b c) via (Bar a b)

%if style == newcode

> data Bar a b
> class Baz a b c d

%endif
Where is each type variable quantified?

\begin{itemize}
 \item |a| is bound by |Foo| itself in the declaration |data Foo a|.
       These type variable binders scope over both the derived class,
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

> data D
>   deriving (C1 a, C2 a) via (T a)

%if style == newcode

> class C1 a b
> class C2 a b
> data T a

%endif

Suppose we first quantified the variables in the derived classes and
\textit{then} the variables in the |via| type. Because each derived class
has its own type variable scope, the |a| in |C1 a| is bound independently from
the |a| in |C2 a|. In other words, we have something like this (using a
hypothetical |forall| syntax):

< deriving (forall a. C1 a, forall a. C2 a) via (T a)

Now we are faced with a thorny question: which |a| is used in the |via| type,
|T a|? There are multiple choices here, since the |a| variables in
|C1 a| and |C2 a| are distinct! This is an important choice, since the kinds
of |C1| and |C2| might differ, so the choice of |a| could affect whether
|T a| kind-checks or not.

On the other hand, if one binds the |a| in |T a| first, then this becomes a
non-issue. We would instead have this:

< deriving (C1 a, C2 a) via (forall a. T a)

Now, there is no ambiguity regarding |a|, as both |a| variables in the list of
derived classes were bound in the same place.

It might feel strange visually to see a variable being used
\textit{before} of its binding site (assuming one reads code from left to right).
However, this is not unprecedented within Haskell, as this is also legal:

> f :: Int
> f = g + h
>   where
>     g = 1
>     h = 2

In this example, we have another scenario where things are bound (|f| and |g|)
after their use sites. In this sense, the |via| keyword is continuing a rich
tradition pioneered by |where| clauses.

One alternative idea (which was briefly considered) was to put the |via| type
\textit{before} the derived classes so as to avoid this ``zigzagging'' scoping.
However, this would introduce additional ambiguities. Imagine one were to
take this example:

< deriving Z via X Y

And convert it to a form in which the |via| type came first:

< deriving via X Y Z

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
% datatype or before the via? Can this ever be correct (I think so)? Can we still
% explicitly quantify over it, even if it looks totally silly?}
%
% \subsubsection{Multiple binding sites?}
%
% One slight wrinkle in this story is that |deriving| clauses can specify \textit{multiple}
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
% A tricky corner case to consider is that |deriving| clauses can also derive \textit{zero}
% classes to derive. Combined with |deriving via|, this can lead to the following example:
%
% < data Bar
% <   deriving () via S
%
% To deal with this, we opt to desugar this declaration to a datatype with no |deriving|
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

\section{Advanced uses}\label{sec:advanced}

\subsection{Avoiding orphan instances}

Before we had a |Monoid| instance for |IO a| this could not be derived\footnote{http://www.haskellforall.com/2014/07/equational-reasoning-at-scale.html}

< newtype Plugin = Plugin (IO (String -> IO ()))
<   deriving Monoid

\textbf{deriving via} enables us to override and insert arbitrary
instances adding the following line

<     via (App IO (String -> App IO ()))

\bbnote{I used this just now to get a Semigroup instance for Compose f g a.}

If, like \cite{twist-pointers}
we wanted to sequential composion for |IO ()| rather than lifted
behaviour all we need to do is write an adapter type

> newtype Seq f = Seq (f ())
>
> instance Applicative f => Monoid (Seq f) where
>   mempty :: Seq f
>   mempty = Seq (pure ())
>
>   mappend :: Seq f -> Seq f -> Seq f
>   Seq fs `mappend` Seq gs = Seq (fs *> gs)

and derive via

<     via (IO (String -> Seq IO))

Another example from the same paper can be derived as well:

< data Ptr
<
< newtype ParseAction a = PA (Ptr -> IO a)
<   deriving (Functor, Applicative) via
<     (Compose ((->) Ptr) IO)

%if style == newcode

> instance Applicative f => Semigroup (Seq f) where
>   (<>) = mappend

%endif

\subsection{Asymptotic improvement}

The |Applicative| operators |(*>)| and |(<*)| always have a default
definition in terms of |liftA2|

< (<*) = liftA2 (\ a _ -> a)
< (*>) = liftA2 (\ _ b -> b)

It is well known that functions are |Applicative| which unfolds (TODO:
right word?) gives us constant time (TODO: O(1)) definitions that drop
one of their arguments

< instance Applicative (a ->) where
<   pure = const

<   liftA2 q f g a = q (f a) (g a)
<
<   f <* _ = f
<   _ *> g = g

This definition is actually valid for a subset of functors that is
isomorphic to functions, called |Representable|

> class Functor f => Representable f where
>   type Rep f :: Type
>   index    :: f a -> (Rep f -> a)
>   tabulate :: (Rep f -> a) -> f a

so for each |Representable| we get these constant-time operations

< f <* _ = f
< _ <* g = g

For representable functors the definitions of |m *> _ = m| and |_ <* m
= m| are \(O(1)\).\footnote{Edward Kmett:
\url{https://ghc.haskell.org/trac/ghc/ticket/10892?cversion=0&cnum_hist=4\#comment:4}
} This codifies knowledge (on a ``library, not lore'' principle) where
the code can be documented and linked to.

\subsection{Deriving with configuration}

This lets us pass static static value to instance deriving.

< data Person = P { name :: String, age :: Int, addr :: Maybe Address }
<   deriving (Show, Read, ToJSON, FromJSON)
<     via (Person `EncodeAs` Config OmitNothing)

Many of these newtypes existed a long time before @-XDerivingVia@ did
but can be used directly with it which is promising.

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
%format WrapMonoidal = "\ty{WrapMonoidal}"
%format WrapApplicative = "\ty{WrapApplicative}"
%format WM = "\con{WM}"
%format <$ = "\opsym{<\$}"
%endif

> class Functor f => Monoidal f where
>   unit ::  f ()
>   mult ::  f a -> f b -> f (a, b)

Allowing us to derive |Applicative| from a |Monoidal| instance, allow
us to use whatever formulation we prefer

> newtype WrapMonoidal f a = WM (f a)
>   deriving newtype (Functor, Monoidal)
>
> instance Monoidal f => Applicative (WrapMonoidal f) where
>   pure a    = a <$ unit
>   mf <*> mx = fmap (\(f, x) -> f x) (mul mf mx)

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

\subsection{Classes over Defunctionalization Symbols}

\bbnote{TODO}: Using \emph{Singletons} library we can create
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

\subsection{Traversal order}
\url{Discuss ideas here https://www.reddit.com/r/haskell/comments/6udl0i/representable_functors_parameterised_by/}

\subsection{Enhancing @DefaultSignatures@}\label{sec:defaultsignatures}

In section \ref{sec:gnd}, we observed that |deriving via| can fully replace the
@GeneralizedNewtypeDeriving@ extension. In fact, that's not the only language
extension that |deriving via| can be used as a substitute for! There is another
type class-related extension, @DefaultSignatures@, which is frequently used by
GHC programmers to eliminate large classes of boilerplate but it limited by its
expressive power. Here, we demonstrate how one can scrap uses of
@DefaultSignatures@ in favor of |deriving via|, and show how |deriving via|
can overcome the limitations of @DefaultSignatures@.

The typical use case for @DefaultSignatures@ when one has a type class method
that has a frequently used default implementation at a different type.
For instance, consider a |Pretty| class with a method |pPrint| for
pretty-printing data:
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

> class Pretty a where
>   pPrint :: a -> Doc

Coming up with |Pretty| instances for the vast majority of ADTs is repetitive
and tedious, so a common pattern is to abstract away this tedium using
generic programming libraries, such as those found in |GHC.Generics|
~\cite{gdmfh} or @generics-sop@~\cite{true-sums-of-products}. For example,
it is possible using |GHC.Generics| to write:

> genericPPrint :: (Generic a, GPretty (Rep a)) => a -> Doc

%if style == newcode

> genericPPrint = undefined

%endif
The details of how |Generic|, |GPretty|, and |Rep| work are not important to
understanding the example. What is important is to note that a typical
default implementation of |pPrint| in terms of |genericPPrint| is infeasible:

< class Pretty a where
<   pPrint :: a -> Doc
<   pPrint = genericPPrint

The code above will not typecheck, as |genericPPrint| requires extra
constraints |(Generic a, GPretty (Rep a))| that |pPrint| does not provide.
Before the advent of @DefaultSignatures@, one had to work around this by
defining |pPrint| to be |genericPPrint| in every |Pretty| instance, as in the
examples below:

> instance Pretty Bool where
>   pPrint = genericPPrint
>
> instance Pretty a => Pretty (Maybe a) where
>   pPrint = genericPPrint
>
> instance (Pretty a, Pretty b) => Pretty (Either a b) where
>   pPrint = genericPPrint

To avoid this repetition, @DefaultSignatures@ allows one to provide a default
implementation of a type class method using \emph{different} constraints
than the method itself has. For instance:
%if style == newcode
%format Pretty = Pretty2
%format pPrint = pPrint2
%endif

> class Pretty a where
>   pPrint :: a -> Doc
>   default pPrint :: (Generic a, GPretty (Rep a)) => a -> Doc
>   pPrint = genericPPrint

Then, if any instances of |Pretty| are given without an explicit definition of
|pPrint|, the |default| implementation is used. In order for this to typecheck,
the data type |a| used in the instance must satisfy the constraints
|(Generic a, GPretty (Rep a))|. This allows us to reduce the three instances
above to just:

> instance Pretty Bool
> instance Pretty a => Pretty (Maybe a)
> instance (Pretty a, Pretty b) => Pretty (Either a b)

Although @DefaultSignatures@ removes the need for many occurrences of
boilerplate code, it also imposes a significant limitation: every type class
method can only have at most one default implementation. As a result,
@DefaultSignatures@ effectively endorses one default implementation as the
canonical one. But in many scenarios, there is far more than just one way to
do something. Our |pPrint| example is no exception. Instead of
|genericPPrint|, one might one to:

\begin{itemize}
 \item Leverage a |Show|-based default implementation instead of a
       |Generic|-based one
 \item Swap out |genericPPrint| with a version that uses @generics-sop@ instead
       of |GHC.Generics|
 \item Use a tweaked version of |genericPPrint| which displays extra debugging
       information
\end{itemize}

All of these are perfectly reasonable choices a programmer might want to make,
but alas, @DefaultSignatures@ will only accept a single implementation as the
One True Default.

Fortunately, |deriving via| provides a convenient way of encoding default
implementations with the ability to toggle between different choices:
|newtype|s! For instance, we can codify two different approaches to
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
> instance (Generic a, GPretty (Rep a)) => Pretty (GenericPPrint a) where
>   pPrint (MkGenericPPrint x) = genericPPrint x
>
> newtype ShowPPrint a = MkShowPPrint a
>
> instance Show a => Pretty (ShowPPrint a) where
>   pPrint (MkShowPPrint x) = stringToDoc (show x)

With these |newtype|s in hand, picking between them is as simple as changing
a single type:

< deriving Pretty via (GenericPPrint DataType1)
< deriving Pretty via (ShowPPrint    DataType2)

\section{Related Ideas}\label{sec:related}

TODO: Something about ML functors?

\subsection{Explicit dictionary passing}

The power and flexibility of |deriving via| is largely due to GHC's ability
to take a class method of a particular type and massage it into a method
of a different type. This process is almost completely abstracted away from
the user, however. A user only needs to specify the types involved, and GHC
will handle the rest behind the scenes.

An alternative approach, which would put more power into the hands of the
programmer, is to permit the ability to explicitly construct and pass the
normally implicit dictionary arguments corresponding to type class instances
~\cite{implicit-params-explicit}. Unlike in |deriving via|, where going between
class instances is a process that is carefully guided by the compiler,
permitting explicit dictionary arguments would allow users to actually
@coerce@ concrete instance values and pass them around as first-class objects.
In this sense, explicit dictionary arguments could be thought of as a further
generalization of the technique that |deriving via| uses.

However, explicit dictionary arguments do come with some costs. They
require significantly enhancing Haskell's type system to support, and
they break principle typing. Moreover, we feel as if explicit
dictionary passing to too large a hammer for the nail we are trying to hit.
|deriving via| works by means of a simple desugaring of code with some
light typechecking on top, which makes it much simpler to describe and
implement. Finally, the problem which explicit dictionaries aims to
solve---resolving ambiguity in implicit arguments---almost never arises
in |deriving via|, as the programmer must specify all the types involved
in the process.

\section{Limitations and Future Work}\label{sec:conclusions}

We have implemented |deriving via| within the GHC.
Our implementation also interacts well with other GHC features that were
not covered in this paper, such as kind polymorphism ~\cite{haskell-promotion},
@StandaloneDeriving@ \rsnote{Is this true? Double-check.},
and type classes with associated type families ~\cite{associated-type-synonyms}.
However, there are still challenges remaining, which we will describe
in this section.

\subsection{Quality of error messages}

The nice thing about |deriving| is that when it works, it tends to work
extremely well. When it \emph{doesn't} work, however, it can be challenging
to formulate an error message that adequately explains what went wrong. The
fundamental issue is that error messages resulting from uses of |deriving|
are usually rooted in \emph{generated} code, and pointing to code that the
user didn't write in error messages can lead to a confusing debugging
experience.

|deriving via| is certainly no exception to this trend. In fact, the problem
of creating lucid error messages is arguably \emph{worse} in the context of
|deriving via|, as we give users the power to derive instances through whatever
type they wish. Unfortunately, this makes it easier to shoot oneself in the
foot, as it is now easier than ever before to feed |deriving| garbage. As one
example, if a user were to accidentally write this code:

< newtype Foo a = MkFoo (Maybe a) deriving Ord via a

Then GHC would throw the following, rather unhelpful error:
\begingroup
\invisiblecomments

< -- \textbullet\ Occurs check: cannot construct the infinite type: |a ~ Maybe a|
< -- \phantom{\textbullet\ }\quad arising from the coercion of the method `|compare|'
< -- \phantom{\textbullet\ }\qquad from type `|a -> a -> Ordering|' to type `|Foo a -> Foo a -> Ordering|'
< -- \textbullet\ When deriving the instance for |(Ord (Foo a))|

\endgroup

The real problem is that |a| and |Maybe a| do not have the same representation
at runtime, but the error does not make this obvious.%
\alnote{This seems similar to the question I brought up during the call
yesterday, so unless we find a better place, this might be a good point to
discuss the example of empty type classes and why we don't want to impose
a specific check for representation-equivalence that is not induced by the
class methods.} It is possible that one
could add an \emph{ad hoc} check for this class of programs, but there are
likely many more tricky corner cases lurking around the corner, given that
one can put anything after |via|.

We do not propose a solution to this problem here, but instead note that issues
with |deriving via| error quality are ultimately issues with |coerce| error
quality, given that the error messages are a result of |coerce| failing to
typecheck. It is likely that investing more effort into making |coerce|'s
error messages easier to understand would benefit |deriving via| as well.

\subsection{Multi-Parameter Type Classes}

GHC extends Haskell by permitting type classes with more than one parameter.
Multi-parameter type classes are extremely common in modern Haskell, to the
point where we assumed the existence of them in Section \ref{sec:kinds}
without further mention. However, multi-parameter type classes pose an
intriguing design question when combined with |deriving via| and
@StandaloneDeriving@, another GHC feature which allows one to write
|deriving| declarations independently of a data type.

For example, one can write the following instance using
@StandaloneDeriving@:

%if style == newcode
%format Triple = Triple_
%format A = A3
%format B = B3
%format C = C3

> instance Triple A B () where
>   triple = undefined

%else
%format A = "\ty{A}"
%format B = "\ty{B}"
%format C = "\ty{C}"
%format MkA = "\con{A}"
%format MkB = "\con{B}"
%format MkC = "\con{C}"
%endif

> class Triple a b c where
>   triple :: (a, b, c)
>
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
@StandaloneDeriving@ will only ever coerce through the \textit{last}
argument of a class. That is because the standalone instance above would be
the same as if a user had written:
%if style == newcode
%format C = C4
%format MkC = MkC4
%endif

> newtype C = MkC ()
>   deriving (Triple A B) via ()

This consistency is perhaps a bit limiting in this context, where we have
multiple arguments to |C| that one could ``derive through''. But it is not
immediately clear how GHC would figure out which of these arguments to |C|
should be derived through, as there seven different combinations it could
choose! It is possible that another syntax would need to be devised to
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

\bibliographystyle{includes/ACM-Reference-Format}

\bibliography{refs}

\end{document}

