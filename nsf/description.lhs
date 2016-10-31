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

Many computer science curricula at the university level include
\emph{discrete mathematics} as a core requirement \citep{ACM:2013}.
Often taken in the first or second year, a discrete mathematics course
introduces mathematical structures and techniques of foundational
importance in computer science, such as induction and recursion, set
theory, logic, modular arithmetic, functions, relations, and graphs.
In addition, it often serves as an introduction to writing formal
proofs.  Although there is wide agreement that discrete mathematics is
foundational, students sometimes struggle to see its relevance to
computer science.

\emph{Functional programming} is a style of programming, embodied in
languages such as Haskell, OCaml, Scala, F\#, and Racket, which
emphasizes functions (\emph{i.e.}\ input-output processes) rather than
sequences of instructions. It enables working at high levels of
abstraction as well as rapid prototyping and refactoring, and provides
a concise and powerful vocabulary to talk about many other topics in
computer science. It is seeing increasing adoption in industry, with
applications as diverse as high-frequency trading algorithms,
interactive web applications, and Facebook's anti-spam system.  For
these reasons, it is becoming critical to expose undergraduate
students to functional programming early, but many computer science
programs struggle to make space for it.  The Association for Computing
Machinery's 2013 curricular guidelines \citep{ACM:2013} do not include
functional programming as a core topic.

One creative idea is to combine functional programming and discrete
mathematics into a single course.  This is not a new idea
\citep{Wainwright:1992, Henderson:2002, Scharff:2002, Doets:2004,
  ODonnell:2006, VanDrunen:2011, Xing:2008}, and even shows up
in the 2007 model curriculum of the Liberal Arts Computer Science
Consortium \citep{LiberalArtsComputerScienceConsortium:2007}. The
benefits of such an approach are numerous:
\begin{itemize}
\item It allows functional programming to be introduced at an early
  point in undergraduates' careers, since discrete mathematics is
  typically taken in the first or second year.  This allows ideas from
  functional programming to inform students' thinking about the rest
  of the curriculum.  By contrast, when functional programming is left
  until later in the course of study, it is in danger of being seen as
  esoteric or as a mere curiosity.
\item The two subjects complement each other well: discrete math
  topics make good functional programming exercises, and ideas from
  functional programming help illuminate discrete math topics.
\item In a discrete mathematics course with both math and
  computer science majors, math majors can have a ``home turf
  advantage'' since the course deals with topics that may be already
  familiar to them (such as writing proofs), whereas computer science
  majors may struggle to connect the course content to computer
  science skills and concepts they already know.  Including functional
  programming levels the playing field, giving both groups of students
  a way to connect the course content to their previous experience.
  Computer science majors will be more comfortable learning math
  concepts that they can play with computationally; math majors can
  leverage their math experience to learn a bit about programming.
\item It is just plain fun: using programming enables interactive
  exploration of mathematics concepts, which leads to higher
  engagement and increased retention.
\end{itemize}

However, despite its benefits, this model is not widespread in
practice.  This may be due partly to lack of awareness, but there are
also some real roadblocks to adoption that make it impractical or
impossible for many departments.

\begin{itemize}
\item Existing functional languages---such as Haskell, Racket, OCaml,
  or SML---are general-purpose languages which (with the notable
  exception of Racket) were not designed specifically with teaching in
  mind.  The majority of their features are not needed in the setting
  of discrete mathematics, and teachers must waste a lot of time and
  energy explaining incidental detail or trying to hide it from
  students.
\item With the notable exception of Racket, tooling for existing
  functional languages is designed for professional programmers, not
  for students.  The systems can be difficult to set up, generate
  confusing error messages, and are generally designed to facilitate
  efficient production of code rather than interactive exploration and
  learning.
\item As with any subject, effective teaching of a functional language
  requires expertise in the language and its use, or at least thorough
  familiarity, on the part of the instructor. General-purpose
  functional languages are large, complex systems, requiring deep
  study and years of experience to master.  Even if only a small part
  of the language is presented to students, a high level of expertise
  is still required to be able to select and present a relevant subset
  of the language and to help students navigate around the features
  they do not need.  For many instructors, spending years learning a
  general-purpose functional language just to teach discrete
  mathematics is a non-starter.  This is especially a problem at
  schools where the discrete mathematics course is taught by
  mathematics faculty rather than computer science faculty.
\item There is often an impedance mismatch between standard
  mathematics notation and the notation used by existing functional
  programming languages.  As one simple example, in mathematics one
  can write $2x$ to denote multiplication of $x$ by $2$; but many
  programming languages require writing a multiplication operator, for
  example, \texttt{2*x}.  Any one such impedance mismatch is small, but taken
  as a whole they can be a real impediment to students as they move
  back and forth between the worlds of abstract mathematics and
  concrete computer programs.
\end{itemize}

\section{Proposed work}
\label{sec:proposed-work}

To address these issues, we propose to develop a new functional
programming language and web-based environment, \thelang, explicitly
designed to be used in a discrete mathematics course, along with a
suitable curriculum to accompany it.

\subsection{Design goals and features}

Design goals and features of \thelang include:
\begin{itemize}
\item \textbf{Simplicity}: \thelang will not be intended or designed
  to be ``real-world'' or general purpose.  Included features will be
  carefully selected for their relevance, so there will be no
  distracting details to hide from students, and the language will be
  easy for instructors to learn and use.
\item \textbf{Web-based}: \thelang will feature a web-based
  development environment designed specifically for students. A
  web-based environment makes it very easy to get started---one only
  has to visit a web page rather than download and install software.
  A web-based environment will also make it easy to support other
  features such as interactively explorable error messages, built-in
  visualizations, and integrated help.
\item \textbf{Math-inspired}: The syntax of \thelang will be primarily
  inspired by common mathematical notation, rather than typical
  functional programming syntax.  For example, one will be able to
  write \texttt{2x} instead of \texttt{2*x}.  See the next section for
  more examples.
\item \textbf{Discrete-specific}: \thelang will have many features
  designed explicitly for discrete mathematics.  For example, it will
  have built-in list, set, multiset, ordered tuple, function,
  relation, and graph types; standard libraries dealing with things
  like number theory and combinatorics; and built-in visualization
  tools.
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
understanding this |gcd| function in several ways.

\paragraph{Type checking}  \thelang will support students in
  thinking about \emph{types}, which are a way of determining which
  sorts of values can be combined by which operations (for example,
  one can perform addition on two numbers, but one cannot add a number
  and a graph).  Types are fundamental in mathematics, but most
  mathematicians think about them only in an implicit, intuitive way.
  Fronting types as part of the \thelang language will give students
  concrete encouragement and practice in thinking about types
  explicitly.

  As an example, suppose that instead of |gcd(b,a)| on the
  second-to-last line, the student writes |gcd(b)|.  This does not
  make sense, since |gcd| expects two arguments, and the environment
  should display some sort of error.  However, the great difficulty
  with errors is that although it is impossible to know \emph{why} the
  user made a particular error, the reason for the error has a
  profound impact on what information would be helpful in correcting
  it.  For example, if the student just made a silly typo, it should
  be enough to highlight the mistake.  On the other hand, if the error
  is due to a more fundamental misunderstanding, more information
  should be displayed to help the student understand what they have
  done wrong.  Even in the case of a fundamental misunderstanding, it
  would be overwhelming to display \emph{all possible} information
  about the error, because the student is likely to give up in
  frustration rather than patiently read through and assimilate all
  the information.

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
        `\verb|b|' is a natural number because it is defined as the
        second parameter of gcd.
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

  % XXX Cite examples for type error paper from ICFP?
\paragraph{Step-by-step execution} Of course a student would be
  able to type in |gcd(15, 42)| and get |3| as the result.  But they
  would also be able to interactively trace the step-by-step
  evaluation, with the ability to dynamically expand the evaluation
  trace to show more detail or collapse it to show less. This will
  help students develop computational thinking skills, allowing them
  to develop, through examples, an accurate mental model of the
  computation process.

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
     \to |gcd(15,42)|
   \]
   Clicking again might show even more detail, for example, that
   %
   |15 == 0| reduces to |false|, and so on.

\paragraph{Visualization} XXX

\paragraph{Testing conjectures}  Now, it turns out that this first
  version of |gcd| is not very efficient.
% XXX exploring number of reduction steps, arithmetic
% operations, etc.
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
generated example inputs \citep{QuickCheck:2000}.  If all the tests
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

\section{Evaluation}
\label{sec:assessment}

\input{evaluation}

\section{Communication of Results}
\label{sec:dissemination}

\input{comm-results}

\section{Related work}
\label{sec:related-work}

\input{related-work}

\section{Team qualifications}
\label{sec:qualifications}

\input{team-qual}

\section{Broader impacts}
\label{sec:impacts}

\input{broader-impacts}
