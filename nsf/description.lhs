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

\newcommand{\thelang}{\textsc{NameOfLanguage}\xspace}

\subsection{XXX}

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
\item As with any subject, teaching a functional language requires
  some expertise, or at least familiarity, on the part of the
  instructor. XXX made worse by general-purpose languages which
  require a high level of expertise to be able to usefully teach them
  and help students navigate through the relevant features.  This is
  especially a problem at schools where the discrete mathematics
  course is taught by mathematics faculty rather than computer science
  faculty.
\item There is often an impedance mismatch between standard
  mathematics notation and the notation used by existing functional
  programming languages.
\end{itemize}

\subsection{Proposed work}
\label{sec:proposed-work}

To address these issues, we propose to develop a new functional
programming language, \thelang, explicitly designed to be used in a
discrete mathematics course, along with a suitable curriculum to go
along with it.  Our primary goals in developing \thelang will be XXX.

Features of \thelang will include:
\begin{itemize}
\item XXX bare-bones: not designed to be ``real-world'' or general
  purpose.  No distracting details to hide.  OK since students will
  not be expected to use it later.
\item XXX syntax will be primarily inspired by common mathematical
  notation, rather than typical functional programming syntax.
\item XXX will have many features explicitly designed for discrete
  mathematics: built-in lists, sets, multisets, ordered tuples, and
  graphs; a standard library dealing with things like primality and
  combinatorics; built-in visualization tools; and a focus on good
  error messages.
\end{itemize}

\subsection{Examples}
\label{sec:examples}

XXX some examples of code we can/want to/will be able to write.

\subsection{Reshaping The Discrete Mathematics Curriculum}
\label{subsec:reshaping_the_discrete_mathematics_curriculum}
XXX Explain how this project will change the curriculum of a discrete
mathematics course.  Our example curriculum should go in here.  Then
we will reference this section from the broader-impacts section.
% subsection reshaping_the_discrete_mathematics_curriculum (end)

\section{Related work}
\label{sec:related-work}


\section{Team qualifications}
\label{sec:qualifications}


\section{Broader impacts}
\label{sec:impacts}

\input{broader-impacts}
