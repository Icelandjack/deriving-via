\documentclass[%
  format=acmsmall,% 1-column format used by PACMPL
  %format=sigplan,% 2-column format used by other SIGPLAN conferences
  review=true,% review mode / enables line numbers
  anonymous=false,% enable to remove author names
  timestamp=true,% adds timestamps to the pages
  authordraft=true,% development mode
  ]{acmart}

\usepackage{booktabs}
\usepackage{hyperref}
\usepackage[nomargin,inline,draft]{fixme}
\FXRegisterAuthor{bb}{ebb}{BB}
\FXRegisterAuthor{al}{eal}{AL}
\FXRegisterAuthor{rs}{ers}{RS}
\fxusetheme{colorsig}

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
%format appEndo = "\id{appEndo}"
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
\alwarning{There was a reference to Conor McBride here, mentioning ``routine
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

> newtype Endo a = MkEndo { appEndo :: a -> a }

(as defined in |MODULE Data.Monoid|). While |Endo| is not an applicative functor,
it admits a perfectly valid monoid instance:

> instance overlapping Monoid2 (Endo a) where
>
>   mempty2 = MkEndo id
>   mappend2 (MkEndo f) (MkEndo g) = MkEndo (f . g)

But this instance overlaps with the general instance above, and while we
can make GHC accept it nevertheless, the presence of overlapping instances
often leads to undesirable behavior.

\alwarning{The original enumeration mentioned another point which I do
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
each datatype in spirit of the |Monoid IO| shown in the beginning. But as we
shall see in the remainder of this paper, there are many such rules, and while
the approach of defining individual instances in a uniform format may be
somewhat feasible for classes that comprise just one or two methods, it becomes
extremely tedious for classes with many methods.

For example, there is a way to lift a |Num| instance through any applicative
functor (and similarly, there are ways to lift |Floating| and |Fractional|):

> instance (Applicative f, Num a) => Num (f a) where
>
>   (+) :: f a -> f a -> f a
>   (+) = liftA2 (+)
>
>   (-) :: f a -> f a -> f a
>   (-) = liftA2 (-)
>
>   (*) :: f a -> f a -> f a
>   (*) = liftA2 (*)
>
>   negate :: f a -> f a
>   negate = liftA negate
>
>   abs :: f a -> f a
>   abs = liftA negate
>
>   signum :: f a -> f a
>   signum = liftA negate
>
>   fromInteger :: Integer -> f a
>   fromInteger = pure . fromInteger

Defining such a boilerplate instance manually for a concrete type constructor
is so annoying that Conal Elliott has introduced a preprocessor for this particular
use case several years ago.\footnote{https://hackage.haskell.org/package/applicative-numbers}
\alwarning{Should ideally be replaced with a proper citation.}
\alwarning{And Conal is by no means alone: see
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
and wrapping the instance head in it:

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

> instance (Applicative f, Monoid a) => Semigroup (FromApplicative f a) where
>
>   (<>) :: FromApplicative f a -> FromApplicative f a -> FromApplicative f a
>   (<>) = mappend

Such instance definitions can be made more concise by employing the
existing language extension @GeneralizedNewtypeDeriving@ which allows
us to make an instance on the underlying type available on the wrapped
type. This is always possible because a |newtype|-wrapped type is guaranteed
to have the same representation as the underlying type \alnote{perhaps cite
the roles paper?}:

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
|Monoid| instances for |IO| and |[]| could be written as follows:

< deriving via (FromApplicative IO a) instance Monoid a => Monoid (IO a)
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
release of GHC. \alwarning{We should make sure that we don't end up promising
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

> data Stream a b = Yield a (Stream a b) | Done b
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

\alwarning{Several other mechanisms have been proposed to deal with this situation.
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

\subsection{Subsuming @GeneralizedNewtypeDeriving@}\label{sec:gnd}

\section{Formalism}\label{sec:formalism}

\section{Advanced uses}\label{sec:advanced}

\subsection{Avoiding orphan instances}
Before we had a |Monoid (IO a)| instance, we could not write\footnote{http://www.haskellforall.com/2014/07/equational-reasoning-at-scale.html}

< newtype Plugin = Plugin (IO (String -> IO ()))
<   deriving Monoid

\textbf{deriving via} enables us to override and insert arbitrary
instances adding the following line

<     via (App IO (String -> App IO ()))

\subsection{Asymptotic improvement}

For representable functors the definitions of |m *> \ _ = m| and |\ _ <* m = m| are \(O(1)\).\footnote{Edward Kmett: \url{https://ghc.haskell.org/trac/ghc/ticket/10892?cversion=0&cnum_hist=4\#comment:4} } This codifies knowledge (on a ``library, not lore'' principle) where the code can be documented and linked to.

\subsection{DeriveAnyClass}

\section{Related Work}\label{sec:related}

\section{Limitations, Conclusions and Future Work}\label{sec:conclusions}

\end{document}

