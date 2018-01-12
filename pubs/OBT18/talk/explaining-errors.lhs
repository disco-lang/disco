%% -*- mode: LaTeX; compile-command: "lhs2TeX --poly explaining-errors.lhs -o explaining-errors.tex && pdflatex explaining-errors.tex" -*-
\documentclass[xcolor=svgnames,12pt,aspectratio=169]{beamer}

%include polycode.fmt

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
\author{\usebeamercolor[fg]{title}{Brent Yorgey} \and Richard
  Eisenberg \and Harley Eades}
% \titlegraphic{\includegraphics[width=1in]{deriv-tree}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{xframe}{}
   \titlepage
\end{xframe}

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
  \end{center}
\end{xframe}

\begin{xframe}{}
%  $\app{(\uabs{f}{\app{f}{3}})}{(\uabs{p}{\app{fst}{p}})}$

  \begin{center}
    \begin{BVerbatim}
(\f -> f 3) (\p -> fst p)
    \end{BVerbatim}

    \begin{Verbatim}
Type mismatch between expected type (t, b0) and actual type Int
    \end{Verbatim}
  \end{center}
\end{xframe}

\begin{xframe}
%  $\app{(\uabs{f}{\app{f}{3}})}{(\uabs{p}{\app{fst}{p}})}$

  \begin{center}
    \begin{BVerbatim}
(\f -> f 3) (\p -> fst p)
    \end{BVerbatim}

\begin{Verbatim}
Type mismatch between expected type (t, b0) and actual type Int
  In the first argument of fst, namely p
  In the expression: fst p
  In the first argument of \ f -> f 3, namely
    (\ p -> fst p)
\end{Verbatim}
  \end{center}
\end{xframe}

\begin{xframe}
  \footnotesize
\begin{Verbatim}
    Type mismatch between expected type (t, b0) and actual type Int
      In the first argument of fst, namely p
      In the expression: fst p
      In the first argument of \ f -> f 3, namely
        (\ p -> fst p)
      Relevant bindings include:
        fst :: (a,b) -> a
      Inferred types for subterms:
        3             :: Int
        (\f -> f 3)   :: forall a. (Int -> a) -> a
        (\p -> fst p) :: (Int -> a0)
        (\f -> f 3) (\p -> fst p) :: a0
      Suggested fixes:
        Change p to (p,y)
        Change fst to a function expecting an Int
        Change 3 to (x,y)
\end{Verbatim}
\end{xframe}

% XXX IF TIME: make one more slide, even more ridiculously small &
% crowded with info

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=3in]{not-helping.jpg}
  \end{center}
\end{xframe}

\begin{xframe}
  \footnotesize
  \begin{Verbatim}
    Type mismatch between expected type (t, b0) and actual type Int
      In the first argument of fst, namely p
      In the expression: fst p
      In the first argument of \ f -> f 3, namely
        (\ p -> fst p)
      Relevant bindings include:
        fst :: (a,b) -> a
      Inferred types for subterms:
        3             :: Int
        (\f -> f 3)   :: forall a. (Int -> a) -> a
        (\p -> fst p) :: (Int -> a0)
        (\f -> f 3) (\p -> fst p) :: a0
      Suggested fixes:
        Change p to (p,y)
        Change fst to a function expecting an Int
        Change 3 to (x,y)
  \end{Verbatim}
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

  Also\dots
  \begin{itemize}
  \item Seidel, Jhala \& Weimer: dynamic witnesses for errors
    (ICFP 2016). A great idea, but not the focus of this talk.
  \end{itemize}
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

\onslide<4>
\begin{spec}
infer : Context -> Term -> (neg TypingDerivation + TypingDerivation)
\end{spec}

\onslide<5->
\begin{spec}
infer : Context -> Term -> (UntypingDerivation + TypingDerivation)
\end{spec}

\end{overprint}

\begin{overprint}
  \onslide<6>
  See Ulf Norell invited talk @@ ICFP 2013: \\
  \url{http://www.cse.chalmers.se/~ulfn/code/icfp2013/ICFP.html}

  \onslide<7->
  \begin{center}
    To generate interactive error explanations, \\
    \emph{focus on designing untyping derivations}.
  \end{center}
\end{overprint}

\onslide<8>
\begin{center}
  (or unparsing derivations, or nontermination derivations, or \dots?)
\end{center}
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
    \onslide<1>
    \begin{mathpar}
      \inferrule*[right=Mismatch\frownie{}]{\ty \Gamma t {\tau_1} \\ \tau_1 \neq \tau_2}{\nty
        \Gamma t {\tau_2}}
    \end{mathpar}

    \onslide<2>
    \begin{mathpar}
      \inferrule{ }{\N \neq (\tau_1 \to \tau_2)} \and
      \inferrule{\tau_1 \neq \tau_2}{(\tau_1 \to \tau_3) \neq (\tau_2
        \to \tau_4)} \and \dots
    \end{mathpar}

    \onslide<3>
    \begin{mathpar}
      \inferrule*[right=PlusL\frownie{}]{\nty \Gamma {t_1} {\N}}{\nty \Gamma {t_1 + t_2}
        {\tau}} \and
      \inferrule*[right=PlusR\frownie{}]{\nty \Gamma {t_2} {\N}}{\nty \Gamma {t_1 + t_2}
        {\tau}} \and
      \inferrule*[right=PlusTy\frownie{}]{\tau \neq \N}{\nty \Gamma {t_1 + t_2} {\tau}}
    \end{mathpar}

    \onslide<4>
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

    \onslide<5>
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

%% XXX IF TIME: can we reveal the derivation one step at a time?
\begin{xframe}{Example}
  \begin{mathpar}
    \inferrule*[right=AbsBody\frownie{}]
    {
      \inferrule*[right=PlusTy\frownie{}]
      {\N \to \N \neq \N
      }
      {\nty {f : \N \to \N}{f + 2}{\N \to \N}}
    }
    {\nty \varnothing {\abs{f}{\N \to \N}{f + 2}}{(\N \to \N) \to \N
        \to \N}}
  \end{mathpar}
\end{xframe}

\begin{xframe}{Correctness?}
  \[ \neg \ty \Gamma t \tau \iff \nty \Gamma t \tau \] \medskip

  \onslide<2->
  ($\Rightarrow$): follows from decidability of type
  inference/checking. \medskip

  \onslide<3->
  ($\Leftarrow$): more interesting; $\nty \Gamma t \tau$ adequately
  encodes evidence that $t$ does not have type $\tau$. \medskip

  \onslide<4>
  Still a lot of room for variations: round-tripping need not
  be the identity!
\end{xframe}

\begin{xframe}{Exploring explanations}
  \begin{itemize}
  \item<+-> Untyping derivation = typing derivation with one negated
    subterm?
  \item<+-> Untyping derivation = typing derivation with many negated
    subterms?
  \item<+-> Untyping derivation = typing derivation with negated subterms
    pushed as far to the leaves as possible?
  \item<+-> \dots pulled as far toward the root as possible?
  \item<+-> Treat typing rules as a big sum-of-products, negate, and
    apply De Morgan laws?
  \end{itemize}
\end{xframe}

% \begin{xframe}{Unification?}
  
% \end{xframe}

\end{document}