\documentclass[11pt]{article}

\usepackage[override]{cmtt}

%\usepackage{palatino}
\usepackage{times}

\usepackage{color}
\usepackage{graphicx}
\usepackage{nsffullpage}
\usepackage{latexsym,amssymb}
\usepackage{comment}
\usepackage{url}
\usepackage{xspace}
\usepackage{supertabular}
\usepackage{prettyref}

%% prettyref stuff -------------------------------------------

\newrefformat{fig}{Figure~\ref{#1}}
\newrefformat{sec}{section~\ref{#1}}
\newrefformat{eq}{equation~\eqref{#1}}
\newrefformat{prob}{Problem~\ref{#1}}
\newrefformat{tab}{Table~\ref{#1}}
\newrefformat{thm}{Theorem~\ref{#1}}
\newrefformat{lem}{Lemma~\ref{#1}}
\newrefformat{prop}{Proposition~\ref{#1}}
\newrefformat{defn}{Definition~\ref{#1}}
\newrefformat{cor}{Corollary~\ref{#1}}
\newcommand{\pref}[1]{\prettyref{#1}}

% \Pref is just like \pref but it uppercases the first letter; for use
% at the beginning of a sentence.
\newcommand{\Pref}[1]{%
  \expandafter\ifx\csname r@@#1\endcsname\relax {\scriptsize[ref]}
    \else
    \edef\reftext{\prettyref{#1}}\expandafter\MakeUppercase\reftext
    \fi
}


%\usepackage{proof}
%\setlength{\inferLineSkip}{4pt}

\newcommand{\smallheader}[1]{\smallskip \noindent \textbf{#1}\ \  }

\newif\ifcomments\commentsfalse
\ifcomments
\newcommand{\ednote}[2]{{\bf \color{#1}{[#2]}}\message{ednote!}}
\else
\newcommand{\ednote}[2]{}
\fi

\definecolor{gray}{rgb}{0.96,0.97,0.98}

\newcommand{\bay}[1]{\ednote{blue}{#1 --BAY}}
\newcommand{\hde}[1]{\ednote{green}{#1 --HDE}}
\newcommand{\todo}[1]{\ednote{red}{TODO: #1}}

\newcommand{\term}[1]{\emph{#1}}
\newcommand{\eg}{\emph{e.g.}}
\newcommand{\ie}{\emph{i.e.}}

\newcommand{\etal}{\emph{et al.}}

%\newif\iftitle\titlefalse
%\newif\ifbib\bibfalse

\newtheorem{objective}{Research Objective}

\newenvironment{note}{\begin{trivlist}\message{note!}\item\it}{\end{trivlist}}

%include polycode.fmt

\begin{document}

%\iftitle
 \setcounter{page}{1}
  \begin{center}
    \vspace*{2in}
    {\LARGE TITLE GOES HERE}\\
    \vspace{0.5in} \LARGE \bf
    XXX NAMES \\[1.8in]
    XXX DATE
  \end{center}
  \newpage
%\fi

\thispagestyle{empty}
\include{summary}
\clearpage\setcounter{page}{1}

%include description.lhs

\clearpage\setcounter{page}{1}
\clearpage\setcounter{page}{1}

\bibliographystyle{alpha}  %% plenty of space
\bibliography{references}

\clearpage\setcounter{page}{1}

\newpage
\include{hde-bio}
\include{bay-bio}

\include{data-management}

\end{document}
