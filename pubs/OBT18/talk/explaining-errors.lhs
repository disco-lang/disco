%% -*- mode: LaTeX; compile-command: "lhs2TeX --agda explaining-errors.lhs -o explaining-errors.tex && pdflatex explaining-errors.tex" -*-
\documentclass[xcolor=svgnames,12pt,aspectratio=169]{beamer}

%include agda.fmt

%format neg = "\neg"
%format =>  = "\Rightarrow"

\usepackage[all]{xy}
\usepackage{brent}
\usepackage{xspace}
\usepackage{fancyvrb}
\usepackage{ulem}

\usepackage{ucs}
\usepackage[utf8x]{inputenc}
% \usepackage[backend=pgf,extension=pgf,input,outputdir=diagrams]{diagrams-latex}
\graphicspath{{images/}}

\usepackage{wasysym}  %% for \frownie

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Math typesetting

%% a bit more space for matrices
\setlength{\arraycolsep}{5pt}

\newcommand{\ty}[3]{{#1} \vdash {#2} : {#3}}
\newcommand{\nty}[3]{{#1} \nvdash {#2} : {#3}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\etc}{\textit{etc.}}
\renewcommand{\eg}{\textit{e.g.}\xspace}
\renewcommand{\ie}{\textit{i.e.}\xspace}

\newcommand{\theschool}{Off the Beaten Track}
% \newcommand{\thelocation}{Los Angeles}
\newcommand{\thedate}{13 January 2018}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\setbeamertemplate{items}[circle]

\mode<presentation>
{
  \usetheme{default}                          % use a default (plain) theme

  \setbeamertemplate{navigation symbols}{}    % don't show navigation
                                              % buttons along the
                                              % bottom
  \setbeamerfont{normal text}{family=\sffamily}

  % XX remove this before giving actual talk!
  % \setbeamertemplate{footline}[frame number]
  % {%
  %   \begin{beamercolorbox}{section in head/foot}
  %     \vskip2pt
  %     \hfill \insertframenumber
  %     \vskip2pt
  %   \end{beamercolorbox}
  % }

  \AtBeginSection[]
  {
    \begin{frame}<beamer>
      \frametitle{}

      \begin{center}
%        \includegraphics[width=2in]{\sectionimg}
%        \bigskip

        {\Huge \insertsectionhead}
      \end{center}
    \end{frame}
  }
}

\defbeamertemplate*{title page}{customized}[1][]
{
  \vbox{}
  \vfill
  \begin{centering}
    \begin{beamercolorbox}[sep=8pt,center,#1]{title}
      \usebeamerfont{title}\inserttitle\par%
      \ifx\insertsubtitle\@@empty%
      \else%
        \vskip0.25em%
        {\usebeamerfont{subtitle}\usebeamercolor[fg]{subtitle}\insertsubtitle\par}%
      \fi%
    \end{beamercolorbox}%
    \vskip1em\par
    {\usebeamercolor[fg]{titlegraphic}\inserttitlegraphic\par}
    \vskip1em\par
    \begin{beamercolorbox}[sep=8pt,center,#1]{author}
      \usebeamerfont{author}\insertauthor
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{institute}
      \usebeamerfont{institute}\insertinstitute
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{date}
      \usebeamerfont{date}\insertdate
    \end{beamercolorbox}
  \end{centering}
  \vfill
}

\newenvironment{xframe}[1][]
  {\begin{frame}[fragile,environment=xframe,#1]}
  {\end{frame}}

% uncomment me to get 4 slides per page for printing
% \usepackage{pgfpages}
% \pgfpagesuselayout{4 on 1}[uspaper, border shrink=5mm]

% \setbeameroption{show only notes}

\renewcommand{\emph}{\textbf}

\title{Explaining Type Errors}
\date{\theschool \\ \thedate}
\author{{\usebeamercolor[fg]{title} Brent Yorgey} \and Richard
  Eisenberg \and Harley Eades}
% \titlegraphic{\includegraphics[width=1in]{deriv-tree}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{xframe}{}
   \titlepage
\end{xframe}

% \note{Foo bar}

\begin{xframe}{The Dreaded Type Error Message}
\footnotesize
\begin{Verbatim}
Could not deduce (Num t0)
from the context: (Num (t -> a), Num t, Num a)
  bound by the inferred type for 'it':
             forall a t. (Num (t -> a), Num t, Num a) => a
  at <interactive>:4:1-19
The type variable 't0' is ambiguous
In the ambiguity check for the inferred type for 'it'
To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
When checking the inferred type
  it :: forall a t. (Num (t -> a), Num t, Num a) => a
\end{Verbatim}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics{what-the-function.jpg}
  \end{center}
\end{xframe}

\begin{xframe}{Theses}
  \begin{itemize}
  \item ``Improving'' error messages doesn't fundamentally help.
  \item Interactive \emph{error explanations} instead of static
    \emph{error messages}.
  \item \emph{Error explanation} = \emph{constructive evidence} for
    an error.
  \end{itemize}
\end{xframe}

\section{The Curse of Information}

\begin{xframe}{}
  \begin{center}
\begin{BVerbatim}
(\f -> f 3) (\p -> fst p)
\end{BVerbatim}
\medskip

\begin{overprint}
  \onslide<2>
    \begin{Verbatim}
Type mismatch between expected type (t, b0) and actual type Int




    \end{Verbatim}

  \onslide<3>
\begin{Verbatim}
Type mismatch between expected type (t, b0) and actual type Int
  In the first argument of fst, namely p
  In the expression: fst p
  In the first argument of \ f -> f 3, namely
    (\ p -> fst p)
\end{Verbatim}

\onslide<4>
\begin{Verbatim}
Type mismatch between expected type (t, b0) and actual type Int
  In the first argument of fst, namely p
  In the expression: fst p
  In the first argument of \ f -> f 3, namely
    (\ p -> fst p)
  Inferred types for subterms:
    3             :: Int
    (\f -> f 3)   :: forall a. (Int -> a) -> a
    (\p -> fst p) :: (Int -> a0)
    (\f -> f 3) (\p -> fst p) :: a0
\end{Verbatim}

\onslide<5>
\begin{Verbatim}
Type mismatch between expected type (t, b0) and actual type Int
  In the first argument of fst, namely p
  In the expression: fst p
  In the first argument of \ f -> f 3, namely
    (\ p -> fst p)
  Inferred types for subterms:
    3             :: Int
    (\f -> f 3)   :: forall a. (Int -> a) -> a
    (\p -> fst p) :: (Int -> a0)
    (\f -> f 3) (\p -> fst p) :: a0
  Relevant bindings include:
    fst :: (a,b) -> a
\end{Verbatim}

\onslide<6>
\begin{Verbatim}[fontsize=\footnotesize]
Type mismatch between expected type (t, b0) and actual type Int
  In the first argument of fst, namely p
  In the expression: fst p
  In the first argument of \ f -> f 3, namely
    (\ p -> fst p)
  Inferred types for subterms:
    3             :: Int
    (\f -> f 3)   :: forall a. (Int -> a) -> a
    (\p -> fst p) :: (Int -> a0)
    (\f -> f 3) (\p -> fst p) :: a0
  Relevant bindings include:
    fst :: (a,b) -> a
  Suggested fixes:
    Change p to (p,y)
    Change fst to a function expecting an Int
    Change 3 to (x,y)
\end{Verbatim}

\onslide<7>
\begin{Verbatim}[fontsize=\scriptsize]
Type mismatch between expected type (t, b0) and actual type Int
  In the first argument of fst, namely p
  In the expression: fst p
  In the first argument of \ f -> f 3, namely
    (\ p -> fst p)
  Inferred types for subterms:
    3             :: Int
    (\f -> f 3)   :: forall a. (Int -> a) -> a
    (\p -> fst p) :: (Int -> a0)
    (\f -> f 3) (\p -> fst p) :: a0
  Relevant bindings include:
    fst :: (a,b) -> a
  Suggested fixes:
    Change p to (p,y)
    Change fst to a function expecting an Int
    Change 3 to (x,y)
  Relevant documentation:
    https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-260003.3
    https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-360003.8
    http://dev.stephendiehl.com/fun/006_hindley_milner.html
\end{Verbatim}
\end{overprint}
\end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=3in]{not-helping.jpg}
  \end{center}
\end{xframe}

\begin{xframe}{The Curse of Information}
  \begin{itemize}
  \item<+-> Not enough information $\Rightarrow$ confusing
  \item<+-> Too much information $\Rightarrow$ overwhelming
  \item<+-> No middle ground!
  \end{itemize}
\end{xframe}

\begin{xframe}{}
  \Large
  \begin{center}
    \sout{MESSAGES} \\
    $\Downarrow$ \\
    EXPLANATIONS
  \end{center}
\end{xframe}

\begin{xframe}{}
  \small
\begin{Verbatim}
p is expected to have a pair type but was inferred to have type Int.
  + Why is p expected to have a pair type?
  + Why was p inferred to have type Int?







\end{Verbatim}
\end{xframe}

\begin{xframe}{}
  \small
\begin{Verbatim}
p is expected to have a pair type but was inferred to have type Int.
  + Why is p expected to have a pair type?
  - Why was p inferred to have type Int?
    => p is the parameter of the lambda expression \p -> fst p, which
       must have type (Int -> a0).
    + Why must (\p -> fst p) have type (Int -> a0)?




\end{Verbatim}
\end{xframe}

\begin{xframe}{}
  \small
\begin{Verbatim}
p is expected to have a pair type but was inferred to have type Int.
  + Why is p expected to have a pair type?
  - Why was p inferred to have type Int?
    => p is the parameter of the lambda expression \p -> fst p, which
       must have type (Int -> a0).
    - Why must (\p -> fst p) have type (Int -> a0)?
      => It is an argument to (\f -> f 3), which was inferred to have
         type (forall a. (Int -> a) -> a).
      + Why was (\f -> f 3) inferred to have type
        (forall a. (Int -> a) -> a)?
\end{Verbatim}
\end{xframe}

\begin{xframe}{Related work\dots}
  \begin{itemize}
  \item Plociniczak \& Odersky: Scalad (2012)
  \item Stuckey, Sulzmann \& Wazny: Chameleon (2003)
  \item Simon, Chitil, \& Huch (2000)
  \item Beaven \& Stansifer (1993)
  \end{itemize}

  Not new! But not enough attention\dots

  % Also\dots
  % \begin{itemize}
  % \item Seidel, Jhala \& Weimer: dynamic witnesses for errors
  %   (ICFP 2016). A great idea, but not the focus of this talk.
  % \end{itemize}
\end{xframe}
\section{Explaining errors}

\begin{xframe}{The type of type inference?}

  \begin{overprint}
\onslide<1>
\begin{spec}
infer : Context -> Term -> Maybe Type
\end{spec}

\onslide<2>
\begin{spec}
infer : Context -> Term -> Maybe TypingDerivation
\end{spec}

\onslide<3>
\begin{spec}
infer : Context -> Term -> (Error + TypingDerivation)
\end{spec}

\onslide<4->
\begin{spec}
infer : Context -> Term -> (UntypingDerivation + TypingDerivation)
\end{spec}

\end{overprint}

\begin{overprint}
  \onslide<5>
  See Ulf Norell keynote @@ ICFP 2013: \\
  \url{http://www.cse.chalmers.se/~ulfn/code/icfp2013/ICFP.html}

  \onslide<6->
  \begin{center}
    To generate interactive error explanations, \\
    \emph{focus on designing untyping derivations}.
  \end{center}
\end{overprint}

% \onslide<7>
% \begin{center}
%   (or unparsing derivations, or nontermination derivations, or \dots?)
% \end{center}
\end{xframe}

\begin{xframe}{Example: STLC + \N}
\begin{align*}
  t &::= x \mid n \mid t_1 + t_2 \mid \abs x \tau t \mid \app{t_1}{t_2} \\
  \tau &::= \mathbb{N} \mid \tau_1 \to \tau_2 \\
  \Gamma &::= \varnothing \mid \Gamma,x:\tau
\end{align*}
\end{xframe}

\begin{xframe}{Example: STLC + \N}
\framebox{$\ty \Gamma t {\tau}$}
\begin{mathpar}
  \inferrule{x : \tau \in \Gamma}{\ty \Gamma x \tau} \and
  \inferrule{\ty {\Gamma, x:\tau_1} t {\tau_2}}{\ty \Gamma {\abs x
      {\tau_1} t}{\tau_1 \to \tau_2}} \and
  \inferrule{\ty \Gamma {t_1} {\tau_1 \to \tau_2} \\ \ty \Gamma {t_2} {\tau_1}}
      {\ty \Gamma {\app{t_1}{t_2}} {\tau_2}} \\
  \inferrule{ }{\ty \Gamma n \N} \and
  \inferrule{\ty \Gamma {t_1}{\N} \\ \ty \Gamma {t_2}{\N}}{\ty \Gamma
    {t_1 + t_2}{\N}}
\end{mathpar}
\end{xframe}

\begin{xframe}{Untyping for STLC + $\N$}
  \framebox{$\nty \Gamma t {\tau}$}
  \begin{overprint}
    \onslide<2>
    \begin{mathpar}
      \inferrule*[right=Mismatch\frownie{}]{\ty \Gamma t {\tau_1} \\ \tau_1 \neq \tau_2}{\nty
        \Gamma t {\tau_2}}
    \end{mathpar}

    \onslide<3>
    \begin{mathpar}
      \inferrule{ }{\N \neq (\tau_1 \to \tau_2)} \and
      \inferrule{\tau_1 \neq \tau_2}{(\tau_1 \to \tau_3) \neq (\tau_2
        \to \tau_4)} \and \dots
    \end{mathpar}

    \onslide<4>
    \begin{mathpar}
      \inferrule*[right=PlusL\frownie{}]{\nty \Gamma {t_1} {\N}}{\nty \Gamma {t_1 + t_2}
        {\tau}} \and
      \inferrule*[right=PlusR\frownie{}]{\nty \Gamma {t_2} {\N}}{\nty \Gamma {t_1 + t_2}
        {\tau}} \\
      \inferrule*[right=PlusTy\frownie{}]{\tau \neq \N}{\nty \Gamma {t_1 + t_2} {\tau}}
    \end{mathpar}

    \onslide<5>
    \begin{mathpar}
      \inferrule*[right=AbsTy\frownie{}]
      {\forall \tau_2.\; \tau \neq (\tau_1 \to \tau_2)}
      {\nty \Gamma {\abs x {\tau_1} t} \tau}
      \and

  % -- Otherwise, τ is of the form (τ₁ ⇒ τ₂) but the body t does not
  % -- have type τ₂.  Note this could be either because t is not typable
  % -- at all, or because it has some type other than τ₂.
  % ƛ        : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ₂}
  %            → (τ₁ ∷ Γ) ⊬ t ∶ τ₂
  %            → Γ ⊬ ƛ τ₁ t ∶ (τ₁ ⇒ τ₂)
      \inferrule*[right=AbsBody\frownie{}]
      {\nty{\Gamma, x:\tau_1} {t} {\tau_2}}
      {\nty \Gamma {\abs x {\tau_1} t} {\tau_1 \to \tau_2}}
    \end{mathpar}

    \onslide<6>
    \begin{mathpar}
      % ·-fun    : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ₂}
      % → (∀ {τ₁} → Γ ⊬ t₁ ∶ τ₁ ⇒ τ₂)
      % → Γ ⊬ t₁ · t₂ ∶ τ₂
      \inferrule*[right=LhsTy\frownie{}]
      {\forall \tau_1.\; \nty \Gamma {t_1} {\tau_1 \to \tau_2}}
      {\nty \Gamma {\app {t_1} {t_2}} {\tau_2}}
      \and

      % ·-arg    : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ₁ τ₂}
      % → Γ ⊢ t₁ ∶ τ₁ ⇒ τ₂
      % → Γ ⊬ t₂ ∶ τ₁
      % → Γ ⊬ t₁ · t₂ ∶ τ₂
      \inferrule*[right=RhsTy\frownie{}]
      {\ty \Gamma {t_1} {\tau_1 \to \tau_2} \\
        \nty \Gamma {t_2} {\tau_1}}
      {\nty \Gamma {\app {t_1} {t_2}} {\tau_2}}
    \end{mathpar}
  \end{overprint}
\end{xframe}

\begin{xframe}{Example}
  Does $\abs{f}{\N \to \N}{f+2}$ have type $(\N \to \N) \to \N \to \N$?

  \onslide<2->
  \begin{mathpar}
    \inferrule*[right=AbsBody\frownie{}]
    {
      \inferrule*[right=PlusTy\frownie{}]
      {\inferrule{ }{(\N \to \N) \neq \N}
      }
      {\nty {f : \N \to \N}{f + 2}{\N \to \N}}
    }
    {\nty \varnothing {\abs{f}{\N \to \N}{f + 2}}{(\N \to \N) \to \N
        \to \N}}
  \end{mathpar}

  \onslide<3>
  \begin{Verbatim}
f+2 is expected to have type N->N, but an addition
  must have type N.
  - Why is f+2 expected to have type N->N?
    => f+2 is the body of the lambda expression \f:N->N. f+2,
       which is expected to have type (N->N)->N->N.
  \end{Verbatim}
\end{xframe}

\begin{xframe}{Example, take 2}
  Does $\abs{f}{\N \to \N}{f+2}$ have type $(\N \to \N) \to \N \to \N$?

  \onslide<2->
  \begin{mathpar}
    \inferrule*
    {
      \inferrule*[right=PlusL\frownie{}]
      {
        \inferrule*[right=Mismatch\frownie{}]
        { \inferrule{ }{\ty{f : \N \to \N}{f}{\N \to \N}} \\
          \inferrule{ }{(\N \to \N) \neq \N}
        }
        {\nty {f : \N \to \N}{f}{\N}}
      }
      {\nty {f : \N \to \N}{f+2}{\N \to \N}}
    }
    {\nty \varnothing {\abs{f}{\N \to \N}{f + 2}}{(\N \to \N) \to \N
        \to \N}}
  \end{mathpar}

  \onslide<3>
  \begin{Verbatim}
f is expected to have type N, but has type N->N.
  - Why is f expected to have type N?
    => f is used as an argument to the addition operator.
  - Why does f have type N->N?
    => f is the parameter of the lambda expression \f:N->N. f+2.
  \end{Verbatim}
\end{xframe}

\begin{xframe}{Correctness?}
  Q: How do we know if our definition of untyping is correct?
  \vspace{0.8in}
\end{xframe}

\begin{xframe}{Correctness}
  A: prove a metatheorem!
  \[ \neg \ty \Gamma t \tau \iff \nty \Gamma t \tau \]

  % \onslide<2->
  % ($\Rightarrow$): follows from decidability of type
  % inference/checking. \medskip

  % \onslide<3-> ($\Leftarrow$): $\nty \Gamma t \tau$ adequately encodes
  % evidence that $t$ does not have type $\tau$. \medskip

  \onslide<2> Still a lot of room for variation: round-tripping need
  not be the identity!
\end{xframe}

\section{Challenges}

\begin{xframe}{Structure}
  How well do questions \& explorations really correspond to the
  structure of untyping derivations?
\end{xframe}

\begin{xframe}{Derive untyping derivations?}
  Can we automatically derive untyping rules from typing rules?
  \medskip

  {\scriptsize \dots mumble mumble inversion lemma mumble De Morgan mumble\dots}
\end{xframe}

\begin{xframe}{Unification?}
  \onslide<2->
  \small Does $\app{(\abs{f}{Int \to Int}{\app f
      {(3,4)}})}{(\uabs{x}{x+1})}$ have a type?

  \onslide<3>
  \tiny
  \begin{Verbatim}
Can't unify Int and <Int, Int>
- Checking that Int = <Int, Int>
  because the input types of Int -> Int and <Int, Int> -> u5 must match.
    - Checking that Int -> Int = <Int, Int> -> u5
      because it resulted from applying [u1 ||-> <Int, Int>] to the constraint Int -> Int = u1 -> u5.
        - Inferred that u1 = <Int, Int>
          because <3, 4> is an argument to a function (namely, f), so its type <Int, Int> must be the same as the function's
          input type u1.
        - Checking that Int -> Int = u1 -> u5
          because it resulted from applying [u2 ||-> u5] to the constraint Int -> Int = u1 -> u2.
            - Inferred that u2 = u5
              because the output types of (Int -> Int) -> u2 and (u3 -> Int) -> u5 must match.
                - Inferred that (Int -> Int) -> u2 = (u3 -> Int) -> u5
                  because it resulted from applying [u4 ||-> u3 -> Int] to the constraint (Int -> Int) -> u2 = u4 -> u5.
                    - Inferred that u4 = u3 -> Int
                      because ^x. x + 1 is an argument to a function (namely, ^f : Int -> Int. f <3, 4>), so its type
                      u3 -> Int must be the same as the function's input type u4.
                    - Inferred that (Int -> Int) -> u2 = u4 -> u5
                      because ^f : Int -> Int. f <3, 4> is applied to an argument (namely, ^x. x + 1), so its type
                      ((Int -> Int) -> u2) must be a function type.
            - Checking that Int -> Int = u1 -> u2
              because f is applied to an argument (namely, <3, 4>), so its type (Int -> Int) must be a function type.
  \end{Verbatim}
\end{xframe}

\begin{xframe}{Unification?}
\small Does $\app{(\uabs{p}{\app{fst}{p} + 3})}{((2,5), 6)}$ have a
type?

\onslide<2>
\begin{Verbatim}[fontsize=\TINY]
Can't unify <Int, Int> and Int
- Checking that <Int, Int> = Int
  because it resulted from applying [u2 ||-> <Int, Int>] to the constraint u2 = Int.
    - Inferred that u2 = <Int, Int>
      because the first components of <u2, u3> and <<Int, Int>, Int> must match.
        - Inferred that <u2, u3> = <<Int, Int>, Int>
          because the input types of <u2, u3> -> u2 and <<Int, Int>, Int> -> Int must match.
            - Inferred that <u2, u3> -> u2 = <<Int, Int>, Int> -> Int
              because it resulted from applying [u4 ||-> <<Int, Int>, Int>] to the constraint <u2, u3> -> u2 = u4 -> Int.
                - Inferred that u4 = <<Int, Int>, Int>
                  because it resulted from applying [u1 ||-> <<Int, Int>, Int>] to the constraint u1 = u4.
                    - Inferred that u1 = <<Int, Int>, Int>
                      because the input types of u1 -> Int and <<Int, Int>, Int> -> u7 must match.
                        - Inferred that u1 -> Int = <<Int, Int>, Int> -> u7
                          because it resulted from applying [u6 ||-> <<Int, Int>, Int>] to the constraint u1 -> Int = u6 -> u7.
                            - Inferred that u6 = <<Int, Int>, Int>
                              because <<2, 5>, 6> is an argument to a function (namely, ^p. fst p + 3), so its type <<Int, Int>, Int> must be the same as the function's input type u6.
                            - Inferred that u1 -> Int = u6 -> u7
                              because ^p. fst p + 3 is applied to an argument (namely, <<2, 5>, 6>), so its type (u1 -> Int) must be a function type.
                    - Inferred that u1 = u4
                      because p is an argument to a function (namely, fst), so its type u1 must be the same as the function's input type u4.
                - Inferred that <u2, u3> -> u2 = u4 -> Int
                  because it resulted from applying [u5 ||-> Int] to the constraint <u2, u3> -> u2 = u4 -> u5.
                    - Inferred that u5 = Int
                      because fst p, which was inferred to have type u5, must also have type Int.
                    - Inferred that <u2, u3> -> u2 = u4 -> u5
                      because fst is applied to an argument (namely, p), so its type (<u2, u3> -> u2) must be a function type.
    - Checking that u2 = Int
      because the output types of <u2, u3> -> u2 and <<Int, Int>, Int> -> Int must match.
        - Checking that <u2, u3> -> u2 = <<Int, Int>, Int> -> Int
          because it resulted from applying [u4 ||-> <<Int, Int>, Int>] to the constraint <u2, u3> -> u2 = u4 -> Int.
            - Inferred that u4 = <<Int, Int>, Int>
              because it resulted from applying [u1 ||-> <<Int, Int>, Int>] to the constraint u1 = u4.
                - Inferred that u1 = <<Int, Int>, Int>
                  because the input types of u1 -> Int and <<Int, Int>, Int> -> u7 must match.
                    - Inferred that u1 -> Int = <<Int, Int>, Int> -> u7
                      because it resulted from applying [u6 ||-> <<Int, Int>, Int>] to the constraint u1 -> Int = u6 -> u7.
                        - Inferred that u6 = <<Int, Int>, Int>
                          because <<2, 5>, 6> is an argument to a function (namely, ^p. fst p + 3), so its type <<Int, Int>, Int> must be the same as the function's input type u6.
                        - Inferred that u1 -> Int = u6 -> u7
                          because ^p. fst p + 3 is applied to an argument (namely, <<2, 5>, 6>), so its type (u1 -> Int) must be a function type.
                - Inferred that u1 = u4
                  because p is an argument to a function (namely, fst), so its type u1 must be the same as the function's input type u4.
            - Checking that <u2, u3> -> u2 = u4 -> Int
              because it resulted from applying [u5 ||-> Int] to the constraint <u2, u3> -> u2 = u4 -> u5.
                - Inferred that u5 = Int
                  because fst p, which was inferred to have type u5, must also have type Int.
                - Checking that <u2, u3> -> u2 = u4 -> u5
                  because fst is applied to an argument (namely, p), so its type (<u2, u3> -> u2) must be a function type.
\end{Verbatim}
\end{xframe}

\begin{xframe}{Unification?}
  How to explain unification failures to the user?
  \begin{itemize}
  \item<2-> Implementation matters!
  \item<3-> Union-find might work better than substitutions?
  \item<4> Come up with good untyping derivations and then write an
    algorithm to produce them, rather than the other way around!
  \end{itemize}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    Questions/comments/ideas/discussion?
  \end{center}
\end{xframe}

\end{document}