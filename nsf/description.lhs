% -*- mode: LaTeX -*-

% \begin{quotation}
% What is the intellectual merit of the proposed activity?

% How important is the proposed activity to advancing knowledge and understanding within its own field or across different fields? How well qualified is the proposer (individual or team) to conduct the project? (If appropriate, the reviewer will comment on the quality of prior work.) To what extent does the proposed activity suggest and explore creative and original concepts? How well conceived and organized is the proposed activity? Is there sufficient access to resources?

% What are the broader impacts of the proposed activity?

% How well does the activity advance discovery and understanding while promoting teaching, training, and learning? How well does the proposed activity broaden the participation of underrepresented groups (e.g., gender, ethnicity, disability, geographic, etc.)? To what extent will it enhance the infrastructure for research and education, such as facilities, instrumentation, networks, and partnerships? Will the results be disseminated broadly to enhance scientific and technological understanding? What may be the benefits of the proposed activity to society?
% \end{quotation}

% Another thing to think about when writing a project proposal is the
% answer to the question ``How do we know if the project has
% succeeded?''  NSF reviewers like to see some form of evaluation of the
% idea that we are exploring (it is science after all), so including a
% section about evaluation is important.
% }

\newenvironment{discomsg}{\begin{quote}\sffamily}{\end{quote}}
\newcommand{\discoq}{\item[$\blacktriangleright$]}
\newcommand{\discoqa}{\item[$\blacktriangledown$]}

\newcommand{\thelang}{\textsc{Disco}\xspace}

\section{Introduction}

Many computer science curricula at the university level require some
sort of \emph{discrete mathematics} course.  Such a course typically
covers mathematical concepts that are often useful in a computer
science context, such as induction and recursion, graphs, modular
arithmetic, logic, and set theory.  In addition, it often serves as an
introduction to writing formal proofs. See \cite{ACM:2013} for the
Association for Computing Machinery's computer science curriculum
guidelines.

\emph{Functional programming} is a style of programming, embodied in
languages such as Haskell, OCaml, Scala, F\#, and Racket (citations;
should we add more?), which emphasizes functions as first-class
objects, compositionality, and XXX.  It lends itself particularly well
to XXX.  XXX promotes high-level thinking, gives a concise and
powerful vocabulary to talk about many other CS topics.  XXX
e.g. parallelism.  XXX also increasingly used in industry.  For these
reasons, it is becoming critical to expose undergraduate students to
functional programming early, but many curricula struggle to make
space for it.  XXX cite ACM guidelines; cite LACS exemplary curriculum
(see Van Drunen).

One creative idea is to combine functional programming and discrete
mathematics into a single course.  This is not a new idea XXX cite examples.
The benefits of such an approach are numerous:
\begin{itemize}
\item It allows functional programming to be introduced at an early
  point in undergraduates' careers, since discrete mathematics is
  typically taken in the first or second year.  This allows ideas from
  functional programming to inform students' thinking about the rest
  of the curriculum.  By contrast, when it is left until later in the
  course of study, it is in danger of being seen as little more than a
  curiosity.
\item The two subjects are highly synergistic. XXX explain.  Cite Van
  Drunen.  Don't go into too much detail since this is not our main
  argument.
\item In a traditional discrete mathematics course with both math and
  computer science majors, math majors can have a ``home turf
  advantage'' since the course deals with topics that are already
  familiar to them (such as writing proofs), whereas computer science
  majors may struggle to connect the course content to computer
  science skills and concepts they already know.  Including functional
  programming levels the playing field, giving both groups of students
  a way to connect the course content to their previous experience.
\item It is just plain fun: using programming enables interactive
  exploration of the mathematics XXX.
\end{itemize}

However, despite its benefits, this model is not widespread in
practice.  This may be due partly to lack of awareness, but there are
also some real roadblocks to adoption that make it impractical or
impossible for many departments.

\begin{itemize}
\item Existing functional languages which are popular for
  teaching---such as Haskell, Racket, OCaml, or SML---are
  general-purpose languages which were not designed specifically with
  teaching in mind.  The majority of their features are not needed in
  the setting of discrete mathematics, and teachers must waste a lot
  of time and energy explaining incidental detail or trying to hide it
  from students.
\item With the notable exception of Racket, tooling for existing
  functional languages is designed for professional programmers, not
  for students.  The systems can be difficult to set up, generate
  confusing error messages, and are generally designed to facilitate
  production of code rather than interactive exploration and learning.
\item As with any subject, teaching a functional language requires
  some expertise, or at least familiarity, on the part of the
  instructor. XXX General-purpose functional languages exacerbate this
  problem, since a high level of expertise is required to be able to
  usefully teach them and help students navigate through the relevant
  features.  This is especially a problem at schools where the
  discrete mathematics course is taught by mathematics faculty rather
  than computer science faculty.
\item There is often an impedance mismatch between standard
  mathematics notation and the notation used by existing functional
  programming languages.  For example, XXX.  XXX confusing for
  students.
\end{itemize}

\section{Proposed work}
\label{sec:proposed-work}

To address these issues, we propose to develop a new functional
programming language, \thelang, explicitly designed to be used in a
discrete mathematics course, along with a suitable curriculum to go
along with it.  Our primary goals in developing \thelang will be XXX.

\subsection{Language features}

Features of \thelang will include:
\begin{itemize}
\item XXX bare-bones: not designed to be ``real-world'' or general
  purpose.  No distracting details to hide.  OK since students will
  not be expected to use it later.  Easier for instructors to learn
  and use.
\item XXX web-based IDE designed specifically for students. Easy to
  get started with (just visit a web page), interactively explorable
  error messages, built-in visualization, integrated help.
\item XXX syntax will be primarily inspired by common mathematical
  notation, rather than typical functional programming syntax.
\item XXX will have many features explicitly designed for discrete
  mathematics: built-in lists, sets, multisets, ordered tuples,
  functions, relations, and graphs; a standard libraries dealing with
  things like number theory and combinatorics; built-in visualization
  tools.
\item XXX ability to state and explore mathematical conjectures.
\item XXX support for experimenting with computational complexity. XXX reword
\end{itemize}

\subsection{An Extended Example: the Euclidean Algorithm}
\label{sec:examples}

As an extended example, we consider the Euclidean Algorithm, a
standard topic in a discrete mathematics course.  One of the oldest
known algorithms, the Euclidean Algorithm finds the \emph{greatest
  common divisor}, or gcd, of two integers.

The basic version of the algorithm works by repeatedly subtracting the
smaller number from the larger, until one of them reaches zero.  It
might be implemented in \thelang as follows:

\begin{spec}
gcd : Nat * Nat -> Nat
gcd (a, b) =
  { a             if b == 0
  { gcd(b, a)     if a < b
  { gcd(a-b, b)   otherwise.
\end{spec}

The first line, |gcd : Nat * Nat -> Nat|, declares that |gcd| is a
function which takes a pair of natural numbers as input and returns
another natural number as output.  The rest of the lines define the
behavior of |gcd| on the pair of natural numbers |(a,b)| by cases: if
|b| is zero, the output is |a|; if |a| is smaller than |b|, the output
is the result of |gcd(b, a)|; in any other case, the output is the
result of |gcd(a-b, b)|.

The \thelang environment could assist a student in writing and
understanding this |gcd| function in several ways:

\begin{itemize}
\item \textbf{Type checking}.
  \thelang will support students in thinking about \emph{types}, which
  are XXX fundamental in mathematics XXX explain why.

  As an example, suppose that instead of |gcd(b,a)| on the
  second-to-last line, the student writes |gcd(b)|.  This does not
  make sense, and the environment must display some sort of error.
  However, the great difficulty with errors is that although it is
  impossible to know \emph{why} the user made a particular error, the
  reason for the error has a profound impact on what information would
  be helpful in correcting it.  For example, if the student just made
  a silly typo, it should be enough to highlight the mistake.  On the
  other hand, if the error is due to a more fundamental
  misunderstanding, more information should be displayed to help the
  student understand what they have done wrong.  Even in the case of a
  fundamental misunderstanding, it would be overwhelming to display
  \emph{all possible} information about the error, because the student
  is likely to give up in frustration rather than patiently read
  through and assimilate all the information.

  \thelang will try to address these problems with \emph{interactive,
    explorable} errors.  For example, in the case of a student typing
  |gcd(b)| instead of |gcd(b,a)|, \thelang would begin by highlighting
  the |b|.  Clicking or hovering on the |b| would bring up a simple
  error message, along with \emph{questions the student can click to
    explore more information}.  For example, it might display
  something like:
  \begin{discomsg}
    Type error: `\verb|b|' is a natural number, but it is used in a place where there should
    be a pair of natural numbers.
    \begin{enumerate}
    \discoq Why is `\verb|b|' a natural number?
    \discoq Why should there be a pair of natural
      numbers here instead?
    \end{enumerate}
  \end{discomsg}

  Clicking to expand both questions might result in something like
  \begin{discomsg}
    Type error: `\verb|b|' is a natural number, but it is used in a place where there should
    be a pair of natural numbers.
    \begin{itemize}
      \discoqa Why is `\verb|b|' a natural number?
      \begin{discomsg}
        `\verb|b|' is a natural number because it is the second
        parameter of gcd.
        \begin{itemize}
        \discoq Why must the second parameter of gcd be a natural
        number?
        \end{itemize}
      \end{discomsg}

    \discoqa Why should there be a pair of natural numbers here instead?
    \begin{discomsg}
      Because this is an argument to gcd.
      \begin{itemize}
        \discoq Why must the argument to gcd be a pair of natural numbers?
      \end{itemize}
    \end{discomsg}
    \end{itemize}
  \end{discomsg}

  Clicking or hovering on any of these would also highlight the
  corresponding region of the program.

  XXX Cite examples for type error paper from ICFP?
\item \textbf{Step-by-step execution}. Of course a student would be
  able to type in |gcd(15, 42)| and get |3| as the result.  But they
  would also be able to interactively trace the step-by-step
  evaluation, with the ability to dynamically expand the evaluation
  trace to show more detail or collapse it to show less. XXX helps
  develop computational thinking, mental model of computational processes

  For example, beginning with |gcd(15, 42)|, the student might expand
  it five steps, resulting in a trace that looks like \[ |gcd(15,42)|
  \to |gcd(42,15)| \to |gcd(27,15)| \to |gcd(12,15)| \to |gcd(15,12)|
  \to |gcd(3, 12)| \]  Perhaps the student does not understand the
  first step: why does |gcd(42,15)| reduce to |gcd(15,42)|?  Clicking
  on the first arrow might expand that step to show more detail:
  \[ |gcd(42,15)| \to
     \begin{array}{ll}
       |{ 42              if 15 == 0| \\
       |{ gcd(15,42)      if 15 < 42| \\
       |{ gcd(15-42, 42)  otherwise|
     \end{array}
     \to |gcd(15,42)| \]
  Clicking again might show even more detail, for example, that |15 ==
  0| reduces to |false|, and so on.

\item \textbf{Visualization}. XXX
\end{itemize}

It turns out that this first version of |gcd| is not very efficient.
XXX exploring number of reduction steps, arithmetic operations, etc.

A more efficient implementation works by directly finding the
remainder of |a| when divided by |b| (using the |mod| operator)
instead of repeatedly subtracting |b| from |a|.

\begin{spec}
gcd2 : Nat * Nat -> Nat
gcd2 (a, b) =
  { a                 if b == 0
  { gcd2(b, a mod b)  otherwise.
\end{spec}

At this point the student thinks these two implementations of |gcd|
have equivalent behavior, but is not entirely sure.  They can formally
write down their conjecture as follows:
\begin{spec}
Conjecture gcd_same : for all a, b, gcd(a,b) == gcd2(a,b).
\end{spec}
\thelang cannot automatically prove this conjecture, but it can check
that the conjecture is at least plausible: it will check that all the
types match up, and automatically test the conjecture on many randomly
generated example inputs XXX cite QuickCheck.  If all the tests
succeed---while not a proof---it gives the student confidence that
their implementation is correct.  On the other hand, if the system
finds a counterexample, it can help the student more quickly hone in
on the source of the error.  For example, suppose the student
mistakenly wrote |gcd2(a, a mod b)| in the last line of the definition
of |gcd2|.  After stating their conjecture, the system would quickly
come back with an interactive error:
\begin{discomsg}
Counterexample found for conjecture \verb|gcd_same|:
\begin{verbatim}
  a = 0
  b = 1
\end{verbatim}
\verb|gcd(0,1) = 1|, but \verb|gcd2(0,1) = 0|.
\begin{itemize}
\discoq Why does \verb|gcd(0,1) = 1|?
\discoq Why does \verb|gcd2(0,1) = 0|?
\end{itemize}
\end{discomsg}
Expanding would once again allow the student to see detailed
evaluation traces for the counterexamples.

%% XXX finally, ... Bezout's identity.

%% \begin{spec}
%% extgcd : Nat * Nat -> Nat * Nat * Nat
%% extgcd = ...

%% Theorem bezouts_identity : for all a, b,
%%   extgcd(a, b) = (g, x, y) =>
%%   (x a + y b == g) and (gcd(a,b) == g).
%% \end{spec}

\subsection{Reshaping The Discrete Mathematics Curriculum}
\label{subsec:reshaping_the_discrete_mathematics_curriculum}
XXX Explain how this project will change the curriculum of a discrete
mathematics course.  Our example curriculum should go in here.  Then
we will reference this section from the broader-impacts section.
% subsection reshaping_the_discrete_mathematics_curriculum (end)

\section{Evaluation}
\label{sec:assessment}

\section{Communication of Results}
\label{sec:dissemination}

Open-source code and freely available platform.  Blogs.  Workshops
and/or papers at SIGCSE, ICFP, other relevant conferences.

\section{Related work}
\label{sec:related-work}


\section{Team qualifications}
\label{sec:qualifications}


\section{Broader impacts}
\label{sec:impacts}

\input{broader-impacts}
