\documentclass[%
  format=acmsmall,% 1-column format used by PACMPL
  %format=sigplan,% 2-column format used by other SIGPLAN conferences
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

> {-# LANGUAGE DerivingStrategies #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE StandaloneDeriving #-}
>
> import Control.Applicative
> import Control.Monad
> import Data.Coerce

%endif

%if style /= newcode
%format Monoid2 = Monoid
%format mempty2 = mempty
%format mappend2 = mappend
%format Monoid3 = Monoid
%format mempty3 = mempty
%format mappend3 = mappend
%format overlapping =
%else

> class Monoid2 m where
>   mempty2 :: m
>   mappend2 :: m -> m -> m

> class Monoid3 m where
>   mempty3 :: m
>   mappend3 :: m -> m -> m

%format overlapping = " {-# OVERLAPPING #-} "
%endif

\begin{document}

\title{Deriving-via}
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
We present a new Haskell language extension that miraculously solves
all problems in generic programming that ever existed.
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
%format MkWrap1 = "\con{Wrap}"
%format App = "\ty{App}"
%format Alt = "\ty{Alt}"
%format MkApp = "\con{App}"
%format MkAlt = "\con{Alt}"
%format Endo = "\ty{Endo}"
%format MkEndo = "\con{Endo}"
%format coerce = "\id{coerce}"
%endif

In Haskell, type classes capture common interfaces. When we declare a datatype
to be an instance of a type class, we explain how it implements the interface
by providing implementations of all the methods of the class.

Quite often, however, these implementations are not completely ad-hoc, but are
in fact determined by the application of a general rule. For example, in the
@base@ package, we can find the following instance for the |Monoid| class:

> instance Monoid a => Monoid2 (IO a) where
>
>   mempty2 :: IO a
>   mempty2 = pure mempty
>
>   mappend2 :: IO a -> IO a -> IO a
>   mappend2 = liftA2 mappend

While the definition as given is specific to |IO|, the principle is not:
we can always lift a monoid |a| over a type constructor |f| as long as
|f| is an applicative functor. This is the case for |IO|, but it is also
true for all the other applicative functors out there.
\alnote{There was a reference to Conor McBride here, mentioning ``routine
programming'' and \url{http://strictlypositive.org/Idiom.pdf}. We might want
to reinsert this.}

\subsection{The problem: capturing general instance rules}

In fact, we might be tempted to define

> instance (Applicative f, Monoid a) => Monoid2 (f a) where
>
>   mempty2 :: f a
>   mempty2 = pure mempty
>
>   mappend2 :: f a -> f a -> f a
>   mappend2 = liftA2 mappend

thereby capturing the general principle and no longer being forced to provide
individual instances such as the one for |IO|. Unfortunately, the general
instance is undesirable for several reasons:

First, the instance overlaps with any other |Monoid| instance for an applied
type, even if that type is not an applicative functor. Consider

> newtype Endo a = MkEndo (a -> a)

(as defined in |MODULE Data.Monoid|). While |Endo| is not an applicative functor,
it admits a perfectly valid monoid instance:

> instance overlapping Monoid2 (Endo a) where
>
>   mempty2 = MkEndo id
>   mappend2 (MkEndo f) (MkEndo g) = MkEndo (f . g)

But this instance overlaps with the general instance above, and while we
can make GHC accept it nevertheless, the presence of overlapping instances
often leads to undesirable behavior.

\alnote{The original enumeration mentioned another point which I do
not understand right now, so I omitted it for the time being:
``Structure of the |f| is often considered more significant that that of |x|.''
Much of this is stolen from Conor: https://personal.cis.strath.ac.uk/conor.mcbride/so-pigworker.pdf}

Second, even if |f| is an applicative functor, the resulting monoid instance
may not be the only one that can be defined, or the one we want to use.
Most notably, lists are the \emph{free monoid}, and their monoid instance
looks as follows:

> instance Monoid2 [a] where
>
>   mempty2   =  []
>   mappend2  =  (++)

This instance does not coincide with the instantiation of the rule above
(and in particular, imposes no constraint on |a| to be a monoid). In fact,
lists are an example of applying a different rule for defining monoids
based on an |Alternative| instance for the type constructor:

> instance Alternative f => Monoid3 (f a) where
>
>   mempty3   =  empty
>   mappend3  =  (<|>)

But clearly, we could not have both general instances in our program at the
same time. The way that Haskell instance search works is to only look at the
instance head when choosing an instance, and then to commit and never backtrack.
So even with overlapping instances enabled, we could not define all the rules
we wanted to in this way.

Currently, the only viable workaround is to define individual instances for
each datatype in spirit of the |Monoid (IO a)| instance shown in the beginning.
But as we
shall see in the remainder of this paper, there are many such rules, and while
the approach of defining individual instances in a uniform format may be
somewhat feasible for classes that comprise just one or two methods, it becomes
extremely tedious for classes with many methods.

For example, there is a way to lift a |Num| instance through any applicative
functor (and similarly, there are ways to lift |Floating| and |Fractional|):

> instance (Applicative f, Num a) => Num (f a) where
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
%format FromApplicative = "\ty{FromApplicative}"
%format FromAlternative = "\ty{FromAlternative}"
%format MkFromApplicative = "\con{FromApplicative}"
%format MkFromAlternative = "\con{FromAlternative}"
%endif

We solve the above problem of capturing general rules for defining
new instances by using a known mechanism: |newtype|s.

We can turn a problematic generic and overlapping instance into an
entirely unproblematic (but not yet useful) one by defining a |newtype|
and wrapping the instance head in it\alnote{According to Baldur, Conor
calls these ``adaptors''. Perhaps we should consider this terminology too.}:

> newtype FromApplicative f a = MkFromApplicative (f a)
>
> instance (Applicative f, Monoid a) => Monoid (FromApplicative f a) where
>
>   mempty :: FromApplicative f a
>   mempty = MkFromApplicative (pure mempty)
>
>   mappend :: FromApplicative f a -> FromApplicative f a -> FromApplicative f a
>   mappend (MkFromApplicative f) (MkFromApplicative g) =
>     MkFromApplicative (liftA2 mappend f g)

Since GHC 8.4, we also need a |Semigroup| instance, because it just became
a superclass of |Monoid|\footnote{See Section~\ref{sec:superclasses} for
a more detailed discussion of this aspect.}:

> instance (Applicative f, Semigroup a) => Semigroup (FromApplicative f a) where
>
>   (<>) :: FromApplicative f a -> FromApplicative f a -> FromApplicative f a
>   MkFromApplicative f <> MkFromApplicative g =
>     MkFromApplicative (liftA2 (<>) f g)

Such instance definitions can be made more concise by employing the
existing language extension @GeneralizedNewtypeDeriving@ which allows
us to make an instance on the underlying type available on the wrapped
type. This is always possible because a |newtype|-wrapped type is
guaranteed to have the same representation as the underlying type
\cite{zero-cost-coercions}\bbnote{|FromAlternative| is
found in @base@ under the name |Data.Monoid.Alt|.}
\alnote{Ok, I had not known this. Perhaps I should rename these
again, then.}

> newtype FromAlternative f a = MkFromAlternative (f a)
>   deriving (Functor, Applicative, Alternative)
>
> instance Alternative f => Monoid (FromAlternative f a) where
>   mempty   =  empty
>   mappend  =  (<|>)
>
> instance Alternative f => Semigroup (FromAlternative f a) where
>   (<>) = mappend

We now introduce a new style of |deriving| that allows us to instruct
the compiler to use such a newtype-derived rule as the basis of a new
instance definition.

For example, using the @StandaloneDeriving@ language extension, the
|Monoid| instances for |IO| and |[]| could be written as follows
\alnote{Both these declarations currenly fail}:

< deriving via (FromApplicative IO a) instance Monoid3 a => Monoid3 (IO a)
< deriving via (FromAlternative [] a) instance Monoid3 [a]

Here, |via| is a new language construct that explains \emph{how} GHC should
derive the instance, namely be reusing the instance already available for the
given type. It should be easy to see why this works: due to the
use of a |newtype|, |FromApplicative IO a| has the same internal representation
as |IO a|, and |FromAlternative [] a| has the same representation as |[a]|, and
any instance available on one type can be made to work on a representationally
equal type as well.

\subsection{Structure of the paper}

In the rest of this paper, we will spell out this idea in more detail.

In Section~\ref{sec:examples} we will look at several more useful examples of
instance rules that can be captured and applied using |newtype|s. In
particular, we will see that our new language extension subsumes
@GeneralizedNewtypeDeriving@.
%
In Section~\ref{sec:formalism}, we explain how the language extension works
formally and how it fits into the existing framework of @DerivingStrategies@.
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
the change was that someone who wants to primarily wants to declare a
|Monad| instance is now required two extra instances for |Functor| and
|Applicative| -- both of which are usually boilerplate, because they can
be defined from the |Monad| instance.

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

If we now have a datatype with a monad instance, we can simply derive
the |Functor| and |Applicative| instances:

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

\subsection{Subsuming @GeneralizedNewtypeDeriving@}\label{sec:gnd}

\rsnote{The prose in this section assumes that we have introduced the ideas and
underlying concepts behind @GeneralizedNewtypeDeriving@ beforehand. When doing
another pass over this section in the future, we should verify that that is
indeed the case.}

An interesting property of |deriving via| is that it completely subsumes the
capabilities of the @GeneralizedNewtypeDeriving@ extension. Recall that
@GeneralizedNewtypeDeriving@ is used to derive an instance for a |newtype| by
reusing the instance of its underlying representation type. For instance:
%if style /= newcode
%format Age = "\ty{Age}"
%format MkAge = "\con{MkAge}"
%endif

> newtype Age = MkAge Int
>   deriving Num

This code would generate the instance:
%if style == newcode
%format Age = Age2
%format MkAge = MkAge2

> newtype Age = MkAge Int

%endif

> instance Num Age where
>   negate  =  coerce (negate  ::  Int -> Int)
>   abs     =  coerce (abs     ::  Int -> Int)
>   -- etc.

%if style == newcode

>   (+) = undefined
>   (*) = undefined
>   signum = undefined
>   fromInteger = undefined

%endif

\rsnote{Should we introduce |coerce| here?}

That is, one can implement an |Num| instance for |Age| by reusing the |Num|
instance for |Int|. But observe that this is simply a special case of
|deriving via|! If we use the |newtype|'s representation type as the |via|
type, then we can just as well derive the instance above like so:
%if style == newcode
%format Age = Age3
%format MkAge = MkAge3
%endif

> newtype Age = MkAge Int
>  deriving Num via Int

This would generate the exact same code as if we were using
@GeneralizedNewtypeDeriving@. To put it more succintly, |deriving via| is
generalized @GeneralizedNewtypeDeriving@.

\section{Formalism}\label{sec:formalism}

Seeing enough examples of |deriving via| can give the impression that it is
a somewhat magical extension. In this section, we aim to explain the "magic"
underlying |deriving via| by giving a precise, algorithmic description of

\begin{itemize}
 \item How |deriving via| clauses are typechecked
 \item What code |deriving via| generates behind the scenes
\end{itemize}

Throughout this section, we will refer to two groups of examples. One example,
shown below, is intended to be as general as possible:
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

< data D (sub x 1) DOTS (sub x d) = DOTS
<   deriving (C (sub y 1) DOTS (sub y (c - 1)))
<            via (V (sub z 1) DOTS (sub z v))
<
< class C (sub y 1) DOTS (sub y (c-1)) (sub y c) where
<   type T1 (sub t1 1) DOTS (sub y c) DOTS (sub t1 m)
<   DOTS
<   type Ts (sub ts 1) DOTS (sub y c) DOTS (sub ts n)
<
<   m1 :: (sub mty1 1) DOTS (sub y c) DOTS (sub mty1 o)
<   DOTS
<   mf :: (sub mtyf 1) DOTS (sub y c) DOTS (sub mtyf p)

In other words, |D| is a data type with |d| type parameters, |C| is a type
class with |c| type parameters, and |V| is some type with |v| type parameters.
Moreover, |C| has |s| associated type families and |f| class methods, each
with different kinds and types, but all of which mentioning the last type
parameter of |C|, |sub y c|.

Because it is sometimes difficult to communicate ideas by showing them in
their full generality with |D|, |C|, and |V|, we will also occasionally make
reference to the following simpler examples:

TODO RGS: Put stuff here

To avoid clutter, we assume that all types have monomorphic kinds. However, it
is easy to incorporate kind polymorphism~\cite{haskell-promotion}, and our
implementation of these ideas in GHC does so.

\subsection{Type variable scoping}

Consider the following example:
%if style /= newcode
%format Bar = "\cl{Bar}"
%format Baz = "\cl{Baz}"
%endif

< data Foo a = DOTS
<   deriving (Bar a b) via (Baz a b c)

Where is each type variable quantified in this example? The answers are:

\begin{itemize}
 \item |a| is bound by |Foo| itself in the declaration |data Foo a|.
       These type variable binders are the outermost ones, and as a result, it
       scopes over both the derived class, |Bar a b|, as well as the |via|
       type, |Baz a b c|.
 \item |b| is bound by the derived class, |Bar a b|. However, |b| is
       \emph{implicitly} quantified, whereas |a| is \emph{explicitly}
       quantified. |b| scopes over the |via| type as well.
 \item |c| is not bound anywhere, and is a free variable.
\end{itemize}

In the example above, |b| was implicitly quantified, but it is in fact
possible to explicitly quantify it using explicit |forall| syntax:

< data Foo a = DOTS
<   deriving (forall b. Bar a) via (Baz a b c)

This declaration of |Foo| is wholly equivalent to the earlier one, but the use
of |forall| makes it clear where |b|'s binding site is. The possibility for
explicit quantification of class type variables raises an interesting question:
how is the following data type treated?
%if style /= newcode
%format X = "\ty{X}"
%format Y = "\cl{Y}"
%format Z = "\cl{Z}"
%endif

< data X a = DOTS
<   deriving (forall a. Y a) via (Z a)

First, recall that the data type variable binders are the outermost ones.
Moreover, because |Y| explicitly binds its own type variable named |a| within
the |deriving| clause, the |a| within |Y a| is distinct from the |a| in |X a|.
And since the binding site for the |a| in |Y a| occurs deeper than the binding
site for the |a| in |X a|, the |a| in |Z a| refers to the same |a| as in
|Y a|.

\section{Advanced uses}\label{sec:advanced}

\subsection{Avoiding orphan instances}

Before we had a |Monoid| instance for |IO a| this could not be derived\footnote{http://www.haskellforall.com/2014/07/equational-reasoning-at-scale.html}

< newtype Plugin = Plugin (IO (String -> IO ()))
<   deriving Monoid

\textbf{deriving via} enables us to override and insert arbitrary
instances adding the following line

<     via (App IO (String -> App IO ()))

\bbnote{I used this just now to get a Semigroup instance for Compose f g a.}

\subsection{Asymptotic improvement}

For representable functors the definitions of |m *> \ _ = m| and |\ _ <* m = m| are \(O(1)\).\footnote{Edward Kmett: \url{https://ghc.haskell.org/trac/ghc/ticket/10892?cversion=0&cnum_hist=4\#comment:4} } This codifies knowledge (on a ``library, not lore'' principle) where the code can be documented and linked to.

\subsection{Deriving with configuration}

This lets us pass static static value to instance deriving.




\subsection{Capturing theorems / knowledge with instances}

Many of these newtypes existed a long time before |-XDerivingVia| did
but can be used directly with it which is promising.

\subsubsection{Every Applicative can be reversed}

Reversed applicative:

> newtype Rev f a = Rev (f a) deriving Functor
>
> instance Applicative f => Applicative (Rev f) where
>   pure = Rev . pure
>
>   Rev f <*> Rev x = Rev (liftA2 (flip ($)) x f)

\alnote{|Rev| is called |Backwards| in @transformers@.}

\subsubsection{Equivalent Applicative definition}

There is an equivalent, more symmetric definition of |Applicative|
arising from category theory (characterizing Applicative as a strong
lax monoidal functor)\footnote{
\url{https://arxiv.org/pdf/1406.4823.pdf} } that can be more
convenient to define and work with\footnote{Functional Pearl:
\url{http://openaccess.city.ac.uk/13222/1/Applicative-final.pdf}
Applicative Programming with
Effects}\footnote{\url{http://openaccess.city.ac.uk/1141/1/Constructors.pdf}
}

> class Functor f => Monoidal f where
>   unit :: f ()
>   (⋆)  :: f a -> f b -> f (a, b)

Allowing us to derive |Applicative| from a |Monoidal| instance, allow
us to use whatever formulation we prefer

> newtype WrapMonoidal f a = WM (f a)
>   deriving newtype (Functor, Monoidal)
>
> instance Monoidal f => Applicative (WrapMonoidal f) where
>   pure a    = a <$ unit
>   mf <*> mx = fmap (\(f, x) -> f x) (mf ⋆ mx)

We can then define the opposite direction, codifying the equivalence
in these two instances

< instance Monoidal    f => Applicative (WrapMonoidal    f)
< instance Applicative f => Monoidal    (WrapApplicative f)

\subsubsection{Equivalent Monad definition}

\footnote{Name taken from \url{http://www.fceia.unr.edu.ar/~mauro/pubs/Notions_of_Computation_as_Monoids.pdf}}

< class Functor m => Triple m where
<   eta :: a -> m a
<   mu  :: m (m a) -> m a
<
< newtype WrapMonadJoin m a = WMJ (m a) deriving newtype Functor
<
< instance MonadJoin m => Applicative (WrapMonadJoin m) where
<   pure = WMJ . eta
<
<   (<*>) = WMJ mx = WMJ (mu (fmap (\f -> mu (fmap (eta . f) mx)) mf))
<
< instance MonadJoin m => Monad (WrapMonadJoin m) where
<   WMJ mx >>= k = WMJ (mu (fmap (\(k -> WMJ m) -> m) mx))

\subsection{Classes over Defunctionalization Symbols}

\bbnote{TODO}: Using \textit{Singletons} library we can create
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

\subsection{Enhancing @DefaultSignatures@}\label{sec:defaultsignatures}

In \ref{sec:gnd}, we observed that |deriving via| can fully replace the
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

< class Pretty a where
<   pPrint :: a -> Doc

Coming up with |Pretty| instances for the vast majority of ADTs is repetitive
and tedious, so a common pattern is to abstract away this tedium using
generic programming libraries, such as those found in |GHC.Generics|
~\cite{gdmfh} or @generics-sop@~\cite{true-sums-of-products}. For example,
it is possible using |GHC.Generics| to write:

< genericPPrint :: (Generic a, GPretty (Rep a)) => a -> Doc

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

< instance Pretty Bool where
<   pPrint = genericPPrint
<
< instance Pretty a => Pretty (Maybe a) where
<   pPrint = genericPPrint
<
< instance (Pretty a, Pretty b) => Pretty (Either a b) where
<   pPrint = genericPPrint

To avoid this repetition, @DefaultSignatures@ allows one to provide a default
implementation of a type class method using \textit{different} constraints
than the method itself has. For instance:

< class Pretty a where
<   pPrint :: a -> Doc
<   default pPrint :: (Generic a, GPretty (Rep a)) => a -> Doc
<   pPrint = genericPPrint

Then, if any instances of |Pretty| are given without an explicit definition of
|pPrint|, the |default| implementation is used. In order for this to typecheck,
the data type |a| used in the instance must satisfy the constraints
|(Generic a, GPretty (Rep a))|. This allows us to reduce the three instances
above to just:

< instance Pretty Bool
< instance Pretty a => Pretty (Maybe a)
< instance (Pretty a, Pretty b) => Pretty (Either a b)

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

< newtype GenericPPrint a = GenericPPrint a
<
< instance (Generic a, GPretty (Rep a)) => Pretty (GenericPPrint a) where
<   pPrint (GenericPPrint x) = genericPPrint x
<
< newtype ShowPPrint a = ShowPPrint a
<
< instance Show a => Pretty (ShowPPrint a) where
<   pPrint (ShowPPrint x) = stringToDoc (show x)

With these |newtype|s in hand, picking between them is as simple as changing
a single type:

< deriving via (GenericPPrint Foo) instance Pretty Foo
< deriving via (ShowPPrint    Foo) instance Pretty Foo

\section{Related Work}\label{sec:related}

\section{Limitations, Conclusions and Future Work}\label{sec:conclusions}

\subsection{Quality of error messages}

The nice thing about |deriving| is that when it works, it tends to work
extremely well. When it \textit{doesn't} work, however, it can be challenging
to formulate an error message that adequately explains what went wrong. The
fundamental issue is that error messages resulting from uses of |deriving|
are usually rooted in \textit{generated} code, and pointing to code that the
user didn't write in error messages can lead to a confusing debugging
experience.

|deriving via| is certainly no exception to this trend. In fact, the problem
of creating lucid error messages is arguably \textit{worse} in the context of
|deriving via|, as we give users the power to derive instances through whatever
type they wish. Unfortunately, this makes it easier to shoot oneself in the
foot, as it is now easier than ever before to feed |deriving| garbage. As one
example, if a user were to accidentally write this code:

< newtype Foo a = MkFoo (Maybe a) deriving Ord via a

Then GHC would throw the following, rather unhelpful error:

TODO RGS: I don't know how to format this
% • Occurs check: cannot construct the infinite type: a ~ Maybe a
%     arising from the coercion of the method ‘compare’
%       from type ‘a -> a -> Ordering’ to type ‘Foo a -> Foo a -> Ordering’
% • When deriving the instance for (Ord (Foo a))

The real problem is that |a| and |Maybe a| do not have the same representation
at runtime, but the error does not make this obvious. It is possible that one
could add an \textit{ad hoc} check for this class of programs, but there are
likely many more tricky corner cases lurking around the corner, given that
one can put anything after |via|.

We do not propose a solution to this problem here, but instead note that issues
with |deriving via| error quality are ultimately issues with |coerce| error
quality, given that the error messages are a result of |coerce| failing to
typecheck. It is likely that investing more effort into making |coerce|'s
error messages easier to understand would benefit |deriving via| as well.

\bibliographystyle{includes/ACM-Reference-Format}

\bibliography{refs}

\end{document}

