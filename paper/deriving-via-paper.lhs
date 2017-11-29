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

%include general.fmt

\setcopyright{rightsretained}

% Data to be filled in later:

%\acmDOI{...}
%\acmISBN{...}
%\acmConference[acro]{name}{month/year}{location}
%\acmYear{...}
%\copyrightyear{...}
%\acmPrice{...}

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

"These types we write down they're not just names for data
representations in memory, they're tags that queue in mathematical
structures that we exploit."\footnote{Taken from unknown position:
https://www.youtube.com/watch?v=3U3lV5VPmOU}

\section{Introduction}
%if style /= newcode
%format via = "\keyw{via}"
%format Foo = "\ty{Foo}"
%format MkFoo = "\con{Foo}"
%format Flip = "\ty{Flip}"
%endif

It is common folklore that \texttt{Monoid}s can be lifted over
\texttt{Applicative}s,\footnote{https://www.cs.ox.ac.uk/ralf.hinze/publications/CSC.pdf “In principle, every idiom can be made an instance of \textit{Num}.}

> instance (Appliative f, Monoid a) => Monoid (f a) where
>   mempty :: f a
>   mempty = pure mempty
> 
>   mappend :: f a -> f a -> f a
>   mappend = liftA2 mappend

Conor McBride calls this “routine programming” using \texttt{Monoid} and \texttt{Applicative} as building blocks.\footnote{http://strictlypositive.org/Idiom.pdf}

But this instance is undesirable for multiple reasons (TODO: more
reasons, rewrite)

\begin{itemize}  
\item It overlaps with every \texttt{Monoid} instance over an applied type.
\item "Structure of the \texttt{f} is often considered more significant that that of \texttt{x}."\footnote{Much of this is stolen from Conor: https://personal.cis.strath.ac.uk/conor.mcbride/so-pigworker.pdf}
\item It may not be the desired \texttt{Monoid}: Some constructors have an ‘inherent monoidal structure’, most notably the \textit{free monoid} (lists: \texttt{[a]}) where we prioritize the list structure and not that of the elements.
\end{itemize}

There is another possible \texttt{Monoid} instance for any
\texttt{Alternative} which also completely overlap, 

> instance Alternative f => Monoid (f a) where
>   mempty :: f a
>   mempty = empty
> 
>   mappend :: f a -> f a -> f a
>   mappend = (<|>)

Lists are monoidal in this sense

> instance Monoid [a] where
>   mempty :: [a]
>   mempty = empty
> 
>   mappend :: [a] -> [a] -> [a]
>   mappend = (<|>)

These \texttt{Monoid (f a)} instances are clearly
incompatible.\footnote{Mention how instance resolution first searches
based on the instance head.}

So what are our options.

An unfortunate solution is duplicating code by hand or with editor
support, with all the usual caveats of doing so. 

> instance Monoid a => Monoid (IO a) where
>   mempty   = pure   mempty
>   mappend  = liftA2 mappend
> 
> instance (Monoid a, Monoid b) => Monoid (a, b) where
>   mempty  = pure   mempty
>   mappend = liftA2 mappend
> 
> instance Monoid b => Monoid (a -> b) where
>   mempty  = pure   mempty
>   mappend = liftA2 mappend

Doing this by hand quickly becomes unviable as \texttt{Num},
\texttt{Floating} and \texttt{Fractional} (which together amount to
over 50 methods) can be lifted in the exact same way. Conal Elliott
introdues a
preprocessor\footnote{https://hackage.haskell.org/package/applicative-numbers}
to derive these classes by textual substitution, he is by no means
alone.\footnote{Some notes:
https://gist.github.com/Icelandjack/e1ddefb0d5a79617a81ee98c49fbbdc4\#a-lot-of-things-we-can-find-with-define}\footnote{\texttt{instance
Num a => Num (Stream a)} from
https://www.cs.ox.ac.uk/ralf.hinze/publications/CSC.pdf}

Haskellers already have a way of giving a difference instance to the
same representation, by way of
\textbf{newtype}s.\footnote{\texttt{Sum} and \texttt{Product} must be
the best known example of this.} For example \texttt{Wrap1 ((->) a) b}
has the same memory representation as \texttt{a -> b}

> newtype Wrap  a   = Wrap  a
> newtype Wrap1 f a = Wrap1 (f a)

Now we can define the previous \texttt{Monoid} instance over
\texttt{Applicative} and \text{Alternative} without overlap

> newtype App f a = App (f a) deriving newtype (Functor, Applicative)
> newtype Alt f a = Alt (f a) deriving newtype (Functor, Applicative, Alternative)
>
> instance (Applicative f, Monoid a) => Monoid (App f a) where
>   mempty  = pure   mempty
>   mappend = liftA2 mappend
> 
> instance Alterantive f => Monoid (Alt f a) where
>   mempty  = empty
>   mappend = (<|>)

What this extension allows is to derive instances that exist for types
of the same representation, so we can derive (TODO: should be rewritten)

> data []     a = ... deriving Monoid via (Alt []     a)
> data IO     a = ... deriving Monoid via (App IO     a) 
> data (a, )  b = ... deriving Monoid via (App (a, )  b)
> data (a ->) b = ... deriving Monoid via (Alt (a ->) b) 

\section{Examples}

\section{Formalism}

\section{Advanced uses}

\begin{itemize}
  \item \textbf{Avoiding orphan instances} Before we had a \texttt{Monoid (IO a)} instance, we could not write\footnote{http://www.haskellforall.com/2014/07/equational-reasoning-at-scale.html}
> newtype Plugin = Plugin (IO (String -> IO ())) 
>   deriving Monoid
  \textbf{deriving via} enables us to override and insert arbitrary
  instances adding the following line without orphan instances
>     via (App IO (String -> App IO ()))
  I used this just now to get a \texttt{Semigroup} instance for \texttt{Compose f g}.

  \item \textbf{Asymptotic improvement} For representable functors the definitions of \texttt{m *> \_ = m} and \texttt{\_ <* m = m} are \textit{O(1)}.\footnote{Edward Kmett: \url{https://ghc.haskell.org/trac/ghc/ticket/10892?cversion=0&cnum_hist=4\#comment:4} } This codifies knowledge (on a “library, not lore” principle) where the code can be documented and linked to.
  \item \textbf{TOOD}: Using \textit{Singletons} library we can create
  instances of actual functions of types, not just matchable constructors

> class Functor f where
>   fmap :: (a -> a') -> (f@@a -> f@@a')
>
> instance Functor id where
>   fmap :: (a -> a') -> (a -> a')
>   fmap = id
> 
> instance Functor dup where
>   fmap :: (a -> a') -> ((a, a) -> (a', a'))
>   fmap = join (***)
> 
> instance (Functor f, Functor g) => Functor (f . g) where
>   fmap :: (a -> a') -> (f (g a) -> f (g a'))
>   fmap = fmap @f . fmap @g
>
> kleisli f a b = a -> f @@ b
> 
> instance Functor f => Functor (kleisli f a) where
>   fmap :: (b -> b') -> ((a -> f@@b) -> (a -> f@@b'))
>   fmap f kont = fmap @f f . kont

  at the cost of inference. But if we are willing to guide the
  inference Haskell will synthesize the code for us:

> newtype Apply f a = Apply (f @@ a)
> 
> instance Functor f => Prelude.Functor (Apply f) where
>   Prelude.fmap f (Apply fa) = Apply (fmap @f f fa)
> 
> newtype DupKleiDup a b = DKD (a -> (b, b), a -> (b, b))
>   deriving Prelude.Functor
>     via (Apply (dup . kleisli dup a))

\end{itemize}

\subsection{Generalized GeneralizedNewtypeDeriving}

\subsection{DeriveAnyClass}

\section{Limitations, Conclusions and Future Work}

\section{Related Work}

\end{document}

