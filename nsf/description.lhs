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

\section{Related work}
\label{sec:related-work}


\section{Team qualifications}
\label{sec:qualifications}


\section{Broader impacts}
\label{sec:impacts}

XXX expand these

\begin{itemize}
\item undergraduate research---language development
\item collaboration between our universities
\item use as an example in PL classes
\item curriculum will make it possible for others to adopt this model
  as well
\item encourage playing with math and engagement with CS.
\end{itemize}

\subsubsection{Broader Impacts at Williams College}
\label{subsec:broader_impacts_at_williams_college}
XXX
% subsubsection broader_impacts_at_williams_college (end)

\subsection{Broader impacts at Augusta University}
\label{subsec:broader_impacts_at_augusta_university}

\subsubsection{Teaching and Mentoring}
\label{subsec:teaching_and_mentoring}

\textbf{Teaching.}  The Discrete Mathematics course at Augusta
University is a computer science course that is taught by a computer
science faculty member.  Furthermore, the students are predominately
computer science majors in their sophomore year.  This implies that
most of the students already have some basic programming experience,
but they often have only a basic understanding of mathematics.  The
language being developed by this project will be used to add a
programming component to this course.  This will both contribute to
their understanding of the mathematics, but also link the theory to
applications in computer science more easily.  Students will then come
away from the course with a better understanding of discrete
mathematics and how it is applied to solve problems in computer
science.

One of the primary aims of this project is to keep the design of the
language as simple as possible, and so the design and implementation
will be used in the curriculum of the junior/senior Programming
Language Concepts course at Augusta University.  Students will get a
chance to see what the complete design of a non-trivial programming
language looks like, but they will also get a chance to extend it with
new features as course projects.

\ \\
\noindent
\textbf{Undergraduate Research.}  The typical Augusta University
student is a U.S. native non-traditional student -- 30\% of
undergraduate students enrolled at Augusta University are
non-traditional students (25 years of age or older) -- military
veterans -- 9\% of undergraduate students at Augusta University report
that they are either military veterans, active military, or are
members of a military family -- and people of minority groups -- 32\%
of undergraduate students at Augusta University are people of minority
groups -- by providing them with rich research
experiences. Undergraduate research opportunities at Augusta
University have been sparse, and so this proposal will contribute to
creating more research possibilities for these types of students.

%% 61\% of all students at AU are female
%% 63\% of undergraduate students at AU are female.

Increasing the number of females entering computer science and
mathematics is an important endeavor that this project has the
potential to contribute to.  In fact, 61\% of all students attending
Augusta University are female, and 63\% of undergraduate students at
Augusta University are female.  This project as a breadth unlike most
consisting of both a core computer science research aspect and a
teaching research aspect.  This combination provides a number of
opportunities that could be used to recruit and engage female computer
science and mathematics students.

Undergraduate students working on a research project like the one
being proposed here should get a chance to do more than the expected
tasks, for example, just writing programs.  We want to include the
students in all aspects of the project including the design and
theoretical aspects.  This will give them a chance to really
experience research.  Furthermore, many of the students that will work
on this project have never collaborated across universities, hence,
including students in project meetings and allowing the students at
Augusta University and Williams College to contribute to the same code
base will give the students at both universities an authentic research
experience; see
Section~\ref{subsec:broader_impacts_of_cross_university_collaboration}. This
project has the potential convince the types of students listed above
to pursue a career in research and go to graduate school when they
would otherwise never think that such a career path was possible for
them.
% subsection broader_impacts_at_augusta_university (end)

\subsection{Broader Impacts of Cross University Collaboration}
\label{subsec:broader_impacts_of_cross_university_collaboration}
Talk about how our students will be impacted by working with other
students across universities.  How will this be setup to encourage
each group to work together?
% subsection broader_impacts_of_cross_university_collaboration (end)
