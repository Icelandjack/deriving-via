\documentclass[%
  format=acmsmall,% 1-column format used by PACMPL
  %format=sigplan,% 2-column format used by other SIGPLAN conferences
  review=true,% review mode / enables line numbers
  anonymous=false,% enable to remove author names
  timestamp=true,% adds timestamps to the pages
  authordraft=true,% development mode
  ]{acmart}

\usepackage{booktabs}

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
  \institution{Standard Chartered Bank}
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

\section{Introduction}
%if style /= newcode
%format via = "\keyw{via}"
%format Foo = "\ty{Foo}"
%format MkFoo = "\con{Foo}"
%format Flip = "\ty{Flip}"
%endif

> newtype Foo a = MkFoo (Either a Int)
>   deriving Functor via (Flip Either Int)

\section{Specification}

\section{Examples}

\section{Conclusions and Future Work}

\end{document}

