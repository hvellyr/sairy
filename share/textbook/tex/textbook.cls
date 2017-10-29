\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{textbook}
              [2015/11/18 v1.0 EyeStep textbook document class]
%
\usepackage[T1]{fontenc}
\usepackage{palatino}
\usepackage[varqu]{inconsolata}
\usepackage{graphicx}
\usepackage{xcolor}
\usepackage{tbmulticol}
%
\widowpenalty=10000
\clubpenalty=5000
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\sloppy
\raggedbottom
\setlength\parindent{0pt}
%\@twosidetrue
\@twosidefalse
\@mparswitchtrue
\@twocolumnfalse
%
\ExecuteOptions{}
\ProcessOptions
%
% Don't mark overfull boxes
\setlength\overfullrule{0pt}
%
% Default font sizes
\renewcommand\normalsize{%
   \@setfontsize\normalsize{10.5}{12.2}%
   \abovedisplayskip 10.5\p@ \@plus3\p@ \@minus6\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 5.5\p@ \@plus3.5\p@ \@minus3\p@
   \belowdisplayskip \abovedisplayskip}
\normalsize
%
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\ttsl}{\normalfont\ttfamily\slshape}{\mathtt\sl}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareOldFontCommand{\sfb}{\normalfont\sffamily\bfseries}{\@nomath\sfb}
%
%\setlength\lineskip{1\p@}
%\setlength\normallineskip{1\p@}
\renewcommand*\baselinestretch{}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\frenchspacing
%
\def\makeoddfootline{\hfil}
\def\makeevenfootline{\hfil}
\def\makeoddheadline{\hfil}
\def\makeevenheadline{\hfil}
\gdef\makeheadline#1#2#3{%
\def\makeevenheadline{#1\hss{}#2\hss{}#3}%
\def\makeoddheadline{#1\hss{}#2\hss{}#3}}
\newcommand\makefootline[3]{%
\def\makeevenfootline{#1\hss{}#2\hss{}#3}%
\def\makeoddfootline{#1\hss{}#2\hss{}#3}}
%
\def\ps@headings{%
\def\@evenhead{\vbox{\hsize=\textwidth
\hbox to \hsize{{\makeevenheadline}}}}%
\def\@oddhead{\vbox{\hsize=\textwidth
\hbox to \hsize{{\makeoddheadline}}}}%
\def\@evenfoot{\vbox{\hsize=\textwidth%
\hbox to \hsize{{\makeevenfootline}}}}%
\def\@oddfoot{\vbox{\hsize=\textwidth%
\hbox to \hsize{{\makeoddfootline}}}}}%
\pagestyle{headings}
\pagenumbering{arabic}
%
% paragraph settings, body
\newcommand{\para}[2]{{\leavevmode#1\indent{}#2\par}}%
% font size, lineskip
\newcommand{\fns}[2]{\fontsize{#1}{#2}\selectfont}
% first-line-start-indent, extend, start-indent, end-indent
\newcommand{\ps}[4]{%
  \setlength{\parindent}{#1}%
  \addtolength{\leftskip}{#3}%
  \addtolength{\rightskip}{#4}%
  \setlength\parfillskip{#2}%
  }
% vertical space
\newcommand{\vsc}[1]{\addvspace{#1}}
\newcommand{\vs}[1]{\vspace*{#1}}
\newcommand{\ldqo}{``}
\newcommand{\rdqo}{''}
\newcommand{\lsqo}{`}
\newcommand{\rsqo}{'}
\newcommand{\hellip}{\hbox{\kern.5pt.\kern1pt.\kern1pt.\kern.5pt}}
\newcommand{\tbless}{\char"3C}
\newcommand{\tbgreater}{\char"3E}
\newcommand{\thsp}{\kern.1em}
%
%
\newcommand{\vln}[2]{{\leavevmode\tt\insp{#1}#2\par}}
\newcommand{\insp}[1]{%
\setbox0=\hbox{\ }\hbox to \wd0{\hss}
\dimen255=\wd0\multiply\dimen255 by #1\hbox to \dimen255{\hss}}
\newcommand{\ws}{{\setbox0=\hbox{\ }\hbox to \wd0{\hss}}}
\def\void{}
%
\def\pageno{\thepage}
%
\newcommand{\lf}[5]{\setbox1=\hbox{#5}\ifdim\wd1>#1\box1\else\hbox to #1{#3#5#4}\fi}%
%
\newcommand{\quadleft}{\addtolength{\rightskip}{0pt plus 9999fil}}
\newcommand{\quadright}{\addtolength{\leftskip}{0pt plus 9999fil}}
\newcommand{\quadcenter}{%
\addtolength{\rightskip}{0pt plus 9999fil}%
\addtolength{\leftskip}{0pt plus 9999fil}}
%
% footnotes
\def\footnoterule{\kern-3\p@\leavevmode\rule{2in}{0.4pt}\vspace{2.6pt}}
\long\def\@makefntext#1{%
\begingroup
\hsize=\textwidth
\fnlayout
#1\par\endgroup}
%
\newcommand{\h}[1]{\leavevmode\raise.7ex\hbox{{#1}}}%
\renewcommand{\t}[1]{\leavevmode\lower.25ex\hbox{{#1}}}%
%%
\newenvironment{theindex}
               {\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \columnseprule \z@
                \columnsep 35\p@
                \let\item\@idxitem}
               {}
\newcommand\@idxitem{\par\hangindent 40\p@}
\newcommand\subitem{\@idxitem \hspace*{20\p@}}
\newcommand\subsubitem{\@idxitem \hspace*{30\p@}}
\newcommand\indexspace{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}
%%
\newcommand\changelayout{
  \leavevmode\relax%
  \setlength{\@colht}{\textheight}%
  \setlength{\@colroom}{\textheight}%
  \setlength{\vsize}{\textheight}
  \setlength{\columnwidth}{\textwidth}\addtolength{\columnwidth}{-\leftskip}%
  \setlength{\hsize}{\columnwidth}%
  \setlength{\linewidth}{\hsize}%
  \setlength{\leftmargin}{\leftskip}%
}%
%
\endinput
%% End of file `textbook.cls'.
