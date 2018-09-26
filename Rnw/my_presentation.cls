\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{my_presentation}[02/07/2015 My custom presentation]

\LoadClass[8pt, xcolor=svgnames]{beamer}

\usetheme{Boadilla}
\usecolortheme[named=Sienna]{structure}
\usepackage{graphicx}
% \usepackage[T1]{fontenc}
% \usepackage{upquote}
\usepackage{underscore}
\usepackage{anyfontsize}
\usepackage[final]{animate}
%\usepackage[colorlinks=true,urlcolor=blue,citecolor=blue,linkcolor=blue]{hyperref}
\usepackage{breqn}
\usepackage{xcolor}
\usepackage{booktabs}
\usepackage{verbatim}
\usepackage{tikz}
\RequirePackage{tikz}
\usetikzlibrary{fadings,shadows,shapes,arrows,positioning,decorations.pathreplacing}

\definecolor{links}{HTML}{2A1B81}
\definecolor{orange}{HTML}{FFFFED}
\hypersetup{colorlinks,linkcolor=links,urlcolor=links}
\usepackage{pgfpages}

\usepackage{enumitem} % bullet lists
\setenumerate[1]{%
  label=\insertenumlabel.}
\usepackage{tabto} % Like tab

\setbeamersize{text margin left=1.5cm,text margin right=1.5cm}

\setlength{\parskip}{0pt}
\setlength{\parsep}{0pt}
\setlength{\headsep}{0pt}
\setlength{\topskip}{0pt}
\setlength{\topsep}{0pt}
\setlength{\partopsep}{0pt}

\beamertemplatenavigationsymbolsempty

\newcommand*{\yellowemph}[1]{%
\tikz[baseline]\node[rectangle, fill=yellow, rounded corners, inner sep=1mm, anchor=base]{#1};%
}

\makeatletter
\tikzset{%
  remember picture with id/.style={%
    remember picture,
    overlay,
    save picture id=#1,
  },
  save picture id/.code={%
    \edef\pgf@temp{#1}%
    \immediate\write\pgfutil@auxout{%
      \noexpand\savepointas{\pgf@temp}{\pgfpictureid}}%
  }
}

\def\savepointas#1#2{%
  \expandafter\gdef\csname save@pt@#1\endcsname{#2}%
}

\tikzdeclarecoordinatesystem{pic}{%
  \@ifundefined{save@pt@#1}{%
    \pgfpointorigin
  }{%
  \pgfsys@getposition{\csname save@pt@#1\endcsname}\save@orig@pic%
  \pgfsys@getposition{\pgfpictureid}\save@this@pic%
  \pgf@process{\pgfpointorigin\save@this@pic}%
  \pgf@xa=\pgf@x
  \pgf@ya=\pgf@y
  \pgf@process{\pgfpointorigin\save@orig@pic}%
  \advance\pgf@x by -\pgf@xa
  \advance\pgf@y by -\pgf@ya
  }%
}

\newcounter{highlight}
\newcommand{\hlstart}{\tikz[remember picture with id=hlstart\the\value{highlight},baseline=-0.7ex];\hl@start}
\newcommand{\hlend}{\tikz[remember picture with id=hlend\the\value{highlight},baseline=-0.7ex];\hl@end\stepcounter{highlight}}


\newcommand{\hl@start}[1][]{%
  \hl@draw{highlighter}{#1}{\the\value{highlight}}}

\newcommand{\hl@end}{}

\def\hl@sets{%
  \edef\hl@sx{\the\pgf@x}%
  \edef\hl@sy{\the\pgf@y}%
}
\def\hl@sete{%
  \edef\hl@ex{\the\pgf@x}%
  \edef\hl@ey{\the\pgf@y}%
}

\@ifclassloaded{beamer}{

\def\page@node{
  \path (current page.south east)
      ++(-\beamer@rightmargin,\footheight)
  node[
    minimum width=\textwidth,
    minimum height=\textheight,
    anchor=south east
  ] (page) {};
}

}{

  \def\page@node{
    \path (current page.north west)
    ++(\hoffset + 1in + \oddsidemargin + \leftskip,\voffset + 1in + \topmargin + \headheight + \headsep)
    node[
      minimum width=\textwidth - \leftskip - \rightskip,
      minimum height=\textheight,
      anchor=north west
    ] (page) {};
  }

}

\newcommand{\hl@draw}[3]{%
  \begin{tikzpicture}[remember picture,overlay]%
  \page@node
  \tikzset{#2,highlight=#1,every path/.append style={highlight=#1}}%
  \pgfmathsetlengthmacro{\hl@width}{\the\pgflinewidth - 1pt}%
  \coordinate (hlstart) at (pic cs:hlstart#3);
  \coordinate (hlend) at (pic cs:hlend#3);
  \tikz@scan@one@point\hl@sets(pic cs:hlstart#3)
  \tikz@scan@one@point\hl@sete(pic cs:hlend#3)
  \ifdim\hl@sy=\hl@ey\relax
  \draw (hlstart) -- (hlend);
  \else
  \draw (hlstart) -- (hlstart -| page.east);
  \pgfmathsetlengthmacro{\hl@sy}{\hl@sy -\hl@width}%
  \pgfmathsetlengthmacro{\hl@ey}{\hl@ey +\hl@width}%
  \loop\ifdim\hl@sy>\hl@ey\relax
  \draw (0,\hl@sy -| page.west) -- (0,\hl@sy -| page.east);
  \pgfmathsetlengthmacro{\hl@sy}{\hl@sy -\hl@width}%
  \repeat
  \draw (hlend -| page.west) -- (hlend);
  \fi
  \end{tikzpicture}%
}

\tikzset{%
  highlight/.default=highlighter,
  highlight/.style={
    color=\pgfkeysvalueof{/tikz/#1 colour},
    line width=\pgfkeysvalueof{/tikz/#1 width},
    line cap=\pgfkeysvalueof{/tikz/#1 cap},
    opacity=\pgfkeysvalueof{/tikz/#1 opacity},
  },
  highlighter colour/.initial=yellow,
  highlighter width/.initial=10pt,% <-- Tweak (was 12pt)
  highlighter cap/.initial=round,
  highlighter opacity/.initial=.5,
  fader colour/.initial=gray,
  fader width/.initial=12pt,
  fader cap/.initial=butt,
  fader opacity/.initial=.5,
}

\@ifclassloaded{beamer}{
\setbeamercolor{highlighted text}{bg=yellow}

\newcommand<>{\highlight}[2][]{%
  \only#3{\hlstart[#1]}#2\only#3{\hlend}}
}{
\newcommand{\highlight}[2][]{%
\hlstart[#1]#2\hlend}
}



\tikzstyle{block} = [rectangle, draw, text width=7em, text centered, rounded corners, minimum height=3em, minimum width=7em, top color = white, bottom color=brown!30,  drop shadow]
 
\newcommand{\ShowSexpr}[1]{\texttt{{\char`\\}Sexpr\{#1\}}}

\usefonttheme{structurebold}
    \setbeamerfont{title}{size=\Huge, series=\bfseries,parent=structure}
    \setbeamerfont{subtitle}{size=\scriptsize,series=\bfseries,parent=structure}
    \setbeamerfont{author}{size=\large, series=\bfseries, parent=structure}
    \setbeamerfont{institute}{size=\scriptsize,series=\bfseries,parent=structure}
    \setbeamerfont{date}{size=\scriptsize,series=\bfseries,parent=structure}
    
    
    
    
 
 
 
 
 % \usepackage{minted}   % code prettyfier
\usepackage{xsavebox} % file-size-efficient saveboxes
\usepackage{animate}  % for animated scrolling
%\usepackage{MnSymbol} % \triangle, \triangledown for scroll buttons
%\usepackage{media9}   % buttons
\usepackage{listings}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% \smoothScroll[<animate opts>]    % autoplay,loop,... (see: texdoc animate)
%              {<xsavebox id>}
%              {<viewport height>}
%              {<steps>}           % scrolling granularity
%              {<steps per sec>}   % while playing; >25 doesn't make sense
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand\smoothScroll[5][]{%
  \savebox\myMeasBox{\xusebox{#2}}%
  \edef\mywd{\the\wd\myMeasBox}%
  \edef\myht{\the\ht\myMeasBox}%
  \edef\mytht{\the\dimexpr\ht\myMeasBox+\dp\myMeasBox\relax}%
  \edef\portht{\the\dimexpr#3\relax}%
  \begin{animateinline}[#1,label={#2},width=\mywd,height=\portht]{#5}%
    \multiframe{#4}{%
      dRaiseLen=\the\dimexpr-\myht+\portht\relax+\the\dimexpr(\mytht-\portht)/%
                \numexpr#4-1\relax\relax
    }{%
      \begin{minipage}[b][\portht][b]{\mywd}%
        \raisebox{\dRaiseLen}[0pt][0pt]{\xusebox{#2}}%
      \end{minipage}%
    }%
  \end{animateinline}%
}
\newsavebox\myMeasBox % for measuring purposes







\makeatletter
\def\parsecomma#1,#2\endparsecomma{\def\page@x{#1}\def\page@y{#2}}
\tikzdeclarecoordinatesystem{page}{
    \parsecomma#1\endparsecomma
    \pgfpointanchor{current page}{north east}
    % Save the upper right corner
    \pgf@xc=\pgf@x%
    \pgf@yc=\pgf@y%
    % save the lower left corner
    \pgfpointanchor{current page}{south west}
    \pgf@xb=\pgf@x%
    \pgf@yb=\pgf@y%
    % Transform to the correct placement
    \pgfmathparse{(\pgf@xc-\pgf@xb)/2.*\page@x+(\pgf@xc+\pgf@xb)/2.}
    \expandafter\pgf@x\expandafter=\pgfmathresult pt
    \pgfmathparse{(\pgf@yc-\pgf@yb)/2.*\page@y+(\pgf@yc+\pgf@yb)/2.}
    \expandafter\pgf@y\expandafter=\pgfmathresult pt
}
\makeatother

    
    
%\usepackage{lipsum}
%\usepackage{animate}

%\makeatletter
%\newcommand\smoothscroll[5][]{%
  % [#1] autoplay
  % #2 <boxnum>
  % #3 <viewport height>
  % #4 <steps>
  % #5 <steps per sec>
%  \edef\mywd{\the\wd#2}%
%  \edef\myht{\the\ht#2}%
%  \edef\mytht{\the\dimexpr\ht#2+\dp#2\relax}%
%  \@anim@reset%
%  \@anim@xform{}{}{#2}% distill box into re-usable PDF XObject
%  \edef\myxform{\@anim@lastxform}%
%  \edef\portht{\the\dimexpr#3\relax}%
%  \begin{animateinline}[#1,width=\mywd,height=\portht,loop]{#5}
%    \multiframe{#4}{dRaiseLen=-\myht+\dimexpr(\mytht+\portht)/#4\relax}{%
%      \begin{minipage}[b][#3][b]{\mywd}%
%        \raisebox{\dRaiseLen}[0pt][0pt]{\@anim@refxform\myxform}%
%      \end{minipage}%
%    }%
%  \end{animateinline}%
%}
%\makeatother


