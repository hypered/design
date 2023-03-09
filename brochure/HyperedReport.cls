%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hypered Report
% LaTeX Class
% Version 1.0 (December 14, 2022)
%
% This template was created by:
% Vel (enquiries@latextypesetting.com)
% LaTeXTypesetting.com
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------------------------
%	CLASS CONFIGURATION
%----------------------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{HyperedReport}[2022/12/14 Hypered Report Class v1.0]

\usepackage{etoolbox} % Required for conditional logic and easily changing commands

\newtoggle{largemargins} % Create toggle for a class option
\settoggle{largemargins}{false} % Default value for the class option
\DeclareOption{largemargins}{\settoggle{largemargins}{true}} % Set the class option toggle if the class option was used in the template

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}} % Pass through any extra options specified to the base class
\ProcessOptions\relax % Process class options

\LoadClass{article} % Load the base class

%----------------------------------------------------------------------------------------
%	REQUIRED PACKAGES AND MISC CONFIGURATIONS
%----------------------------------------------------------------------------------------

\usepackage{graphicx} % Required for including images
\graphicspath{{images/}{./}} % Specifies where to look for included images (trailing slash required)

\usepackage{fontsize} % Allows changing the default font size to an arbitrary value
\changefontsize[15pt]{10pt} % Change the default font size and baselineskip

\setlength{\parindent}{0pt} % Paragraph indentation
\setlength{\parskip}{10pt} % Vertical space between paragraphs

% Set the paragraph skip inside minipages (by default it's set to 0pt)
\newcommand{\@minipagerestore}{\raggedright\setlength{\parskip}{10pt}}

\usepackage{calc} % Allows arithmetic in LaTeX commands

\raggedright % Left align all text (suppress justification)

\usepackage{float} % Allows more precisely positioning floats e.g. \begin{figure}[H]

\usepackage{multicol} % Required for outputting content in multiple columns on demand

%----------------------------------------------------------------------------------------
%	MARGINS
%----------------------------------------------------------------------------------------

\usepackage[
	a4paper, % Paper size
	\iftoggle{largemargins}{top=2.963cm,}{top=2.116cm,} % Top margin
	\iftoggle{largemargins}{bottom=2.963cm,}{bottom=2.116cm,} % Bottom margin
	\iftoggle{largemargins}{left=1.694cm,}{left=0.847cm,} % Left margin
	\iftoggle{largemargins}{right=7.835cm,}{right=7.552cm,} % Right margin
	\iftoggle{largemargins}{marginparwidth=5.294cm,}{marginparwidth=5.859cm,} % Width of the margin content
	marginparsep=0.847cm, % Padding between the main text and margin
	footskip=1.27cm, % Space from the bottom margin to the baseline of the footer
	headsep=0.8466cm, % Space from the top margin to the baseline of the header
	headheight=0.75cm, % Height of the header
	columnsep=0.8466cm, % Amount of space between text columns (in twocolumn mode)
	%showframe % Uncomment to show frames around the margins for debugging purposes
]{geometry}

\newcommand{\leftmarginwidth}{\Gm@lmargin} % Store the left margin width for use elsewhere
\newcommand{\rightmarginwidth}{\Gm@rmargin} % Store the right margin width for use elsewhere
\newcommand{\topmarginwidth}{\Gm@tmargin} % Store the top margin width for use elsewhere
\newcommand{\bottommarginwidth}{\Gm@bmargin} % Store the bottom margin width for use elsewhere

\newenvironment{fullwidth}{ % Environment to output content into the margin within one page
	\begin{adjustwidth}{}{\dimexpr-\marginparwidth-\marginparsep\relax} % Pull into the right margin by the margin width and margin skip width
}{
	\end{adjustwidth}
}

\newenvironment{twothirdswidth}{ % Environment to output content at two-thirds of the whole page width
	\begin{adjustwidth}{}{\dimexpr-0.5\marginparwidth-0.5\marginparsep\relax} % Pull into the right margin by half the margin width and half the margin skip width
}{
	\end{adjustwidth}
}

\usepackage[oneside]{sidenotes} % Required for outputting content to the margin

% Hang sidenote numbers
\makeatletter
\RenewDocumentCommand\sidenotetext{o o +m}{%
	\IfNoValueOrEmptyTF{#1}{%
		\@sidenotes@placemarginal{#2}{%
		\makebox[0pt][r]{% <--THIS WAS ADDED
			\fontsize{8pt}{12pt}\selectfont\textsuperscript{\thesidenote}{}~%
		}% <--THIS WAS ADDED
	\fontsize{8pt}{12pt}\selectfont #3}%
	\refstepcounter{sidenote}%
	}{%
		\@sidenotes@placemarginal{#2}{%
			\makebox[0pt][r]{% <--THIS WAS ADDED
			\fontsize{8pt}{12pt}\selectfont\textsuperscript{#1}~%
		}% <--THIS WAS ADDED
		\fontsize{8pt}{12pt}\selectfont #3}%
	}%
}
\makeatother

\usepackage{changepage} % Required for temporarily indenting text blocks

%----------------------------------------------------------------------------------------
%	FONTS
%----------------------------------------------------------------------------------------

\usepackage[utf8]{inputenc} % Required for inputting international characters
\usepackage[T1]{fontenc} % Output font encoding for international characters

\usepackage[sfdefault, medium]{plex-sans} % Use IBM Plex Sans as the sans font and make it the default
\usepackage[medium]{plex-serif} % Use IBM Plex Serif as the serif font
\usepackage[medium]{plex-mono} % Use IBM Plex Mono as the mono font

\usepackage{anyfontsize} % Allows font sizes at arbitrary values

%----------------------------------------------------------------------------------------
%	HEADERS AND FOOTERS
%----------------------------------------------------------------------------------------

\usepackage{fancyhdr} % Required for customizing headers and footers
\pagestyle{fancy} % Enable custom headers and footers

\renewcommand{\headrulewidth}{0pt} % Top horizontal rule thickness

\fancyhf{} % Clear default headers/footers

\fancyhead[L]{{\ttfamily\fontsize{8pt}{8pt}\selectfont\doctitle}} % Left header

\fancyfoot[R]{{\ttfamily\fontsize{8pt}{8pt}\selectfont\ifnumless{\thepage}{10}{0\thepage}{\thepage}}} % Right footer

\fancyheadoffset[R]{\marginparwidth + \marginparsep} % Offset the header to account for the margin width
\fancyfootoffset[R]{\marginparwidth + \marginparsep} % Offset the footer to account for the margin width

%----------------------------------------------------------------------------------------
%	SECTIONS
%----------------------------------------------------------------------------------------

\usepackage{titlesec} % Required for modifying sections

\titleformat
	{\section} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\fontsize{16pt}{22.4pt}\selectfont\ttfamily\bfseries} % Text formatting of the whole section, i.e. label and title
	{\thesection.\hspace{3pt}} % Section label (e.g. number) and its formatting
	{5pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\section}{0pt}{\baselineskip}{\baselineskip} % Spacing around section titles, the order is: left, before and after

%------------------------------------------------

\titleformat
	{\subsection} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\large\ttfamily\bfseries} % Text formatting of the whole section, i.e. label and title
	{\thesubsection} % Section label (e.g. number) and its formatting
	{5pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\subsection}{0pt}{\baselineskip}{4pt} % Spacing around section titles, the order is: left, before and after

%------------------------------------------------

\titleformat
	{\subsubsection} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\bfseries} % Text formatting of the whole section, i.e. label and title
	{\thesubsubsection} % Section label (e.g. number) and its formatting
	{5pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\subsubsection}{0pt}{\baselineskip}{4pt} % Spacing around section titles, the order is: left, before and after

%----------------------------------------------------------------------------------------
%	TABLE OF CONTENTS
%----------------------------------------------------------------------------------------

\usepackage{titletoc} % Required for manipulating the table of contents

\setcounter{tocdepth}{2} % Show entries in the table of contents down to subsections

\renewcommand{\contentsname}{Index} % Rename the table of contents chapter heading

\titlecontents{section} % Section type being modified
	[0pt] % Left indentation
	{\hypersetup{hidelinks}\vspace{-\baselineskip+4pt}\rule{\linewidth}{1pt}\\[4pt]\bfseries} % Before code
	{\thecontentslabel.~} % Formatting of numbered sections of this type
	{} % Formatting of numberless sections of this type
	{\normalfont\titlerule*[3pt]{.}\contentspage} % Formatting of the filler to the right of the heading and the page number
	[] % After code

%------------------------------------------------

\titlecontents{subsection} % Section type being modified
	[\iftoggle{largemargins}{5.75cm}{6.5cm}] % Left indentation
	{\vspace{-3pt}\hypersetup{hidelinks}} % Before code
	{\thecontentslabel~} % Formatting of numbered sections of this type
	{} % Formatting of numberless sections of this type
	{\titlerule*[3pt]{.}\contentspage} % Formatting of the filler to the right of the heading and the page number
	[] % After code

%----------------------------------------------------------------------------------------
%	COLORS
%----------------------------------------------------------------------------------------

\usepackage[usenames, svgnames]{xcolor} % Required for defining and using custom colors

\definecolor{linkblue}{HTML}{1F51AF}

\usepackage{pagecolor} % Required for setting background colors to pages

%----------------------------------------------------------------------------------------
%	TABLES
%----------------------------------------------------------------------------------------

\usepackage{booktabs} % Required for better horizontal rules in tables

\usepackage{array} % Required for manipulating table columns

\renewcommand{\arraystretch}{1.75} % Increase the height of table rows

\newcolumntype{R}[1]{>{\raggedleft\arraybackslash}p{#1}} % Define a new right-aligned paragraph column type
\newcolumntype{L}[1]{>{\raggedright\arraybackslash}p{#1}} % Define a new left-aligned (no justification) paragraph column type
\newcolumntype{C}[1]{>{\centering\arraybackslash}p{#1}} % Define a new centered paragraph column type

\newcommand{\thicktablerule}{\specialrule{2.5pt}{0pt}{0pt}} % Table rule with no space above or below

%----------------------------------------------------------------------------------------
%	BIBLIOGRAPHY
%----------------------------------------------------------------------------------------

\usepackage[
	backend=biber, % Use the biber backend for compiling the bibliography
	citestyle=authoryear, % In-text citation style
	bibstyle=apa, % Bibliography style
	sorting=none, % Order references in the order in which they are used in the document
]{biblatex}

\newcommand{\sidecite}[2][0pt]{\sidenote[][#1]{\textcite{#2}}} % Parenthetical citations in the margin
\newcommand{\fullsidecite}[2][0pt]{\sidenote[][#1]{\fullcite{#2}}} % Full citations in the margin

%----------------------------------------------------------------------------------------
%	LISTS
%----------------------------------------------------------------------------------------

\usepackage{enumitem} % Required for list customization

\setlist{itemsep=0pt, topsep=0pt, leftmargin=14pt} % Customize spacing around and inside lists

%----------------------------------------------------------------------------------------
%	CAPTIONS
%----------------------------------------------------------------------------------------

\usepackage{caption} % Required for customizing captions

\captionsetup{justification=justified, singlelinecheck=false} % Left-align captions
\captionsetup{font={footnotesize, bf}} % Small font size

% The sidenotes package defines 4 custom caption styles for wide tables, wide figures, margin tables and margin figures
% To style these the same as in-text tables and figures, set these 4 styles to the same styling values
% Unfortunately, this means any changes to captions need to be made in 5 places
\DeclareCaptionStyle{widetable}{font={footnotesize, bf}, justification=justified, singlelinecheck=false}
\DeclareCaptionStyle{widefigure}{font={footnotesize, bf}, justification=justified, singlelinecheck=false}
\DeclareCaptionStyle{marginfigure}{font={footnotesize, bf}, skip=6pt, justification=justified, singlelinecheck=false}
\DeclareCaptionStyle{margintable}{font={footnotesize, bf}, skip=6pt, justification=justified, singlelinecheck=false}

%----------------------------------------------------------------------------------------
%	LINKS
%----------------------------------------------------------------------------------------

\usepackage{hyperref} % Required for links

\hypersetup{
	colorlinks=true, % Whether to color the text of links
	urlcolor=linkblue, % Color for \url and \href links
	linkcolor=linkblue, % Color for \nameref links
	citecolor=black, % Color of reference citations
	%hidelinks, % Hide the default boxes around links
}

%----------------------------------------------------------------------------------------
%	GLOSSARY
%----------------------------------------------------------------------------------------

\usepackage[nogroupskip]{glossaries} % Required for creating a glossary (MUST BE LOADED AFTER hyperref)

\makeglossaries % Tell LaTeX to generate the glossary file

\setglossarystyle{long} % Set the glossary style
\renewenvironment{theglossary}{ % Modify the style to output two columns at specific widths with bold for the glossary entry name
	\hfuzz=10cm% Ignore overfull \hbox errors, we know we're making the rule wider than \linewidth
	\begin{longtable}[l]{@{} >{\bfseries\arraybackslash}L{0.325\linewidth} L{0.65\linewidth} @{}}
}{
	\end{longtable}
}
 
%----------------------------------------------------------------------------------------
%	TITLE PAGE
%----------------------------------------------------------------------------------------

\renewcommand{\titlepage}[3]{
	\expandafter\ifstrequal\expandafter{#1}{black}{\newpagecolor{black}\color{white}}{} % Black page background color if black option passed in the first parameter
	
	\thispagestyle{empty} % Suppress headers and footers on this page
	
	\vspace*{-44pt-\headsep} % Pull up to the header
	
	\begin{twothirdswidth}
		{\sloppy\fontsize{40pt}{52pt}\selectfont\bfseries\ttfamily #2\par} % Title
		
		\vspace{0.65cm} % Vertical whitespace
		
		{\fontsize{16pt}{24pt}\selectfont #3\par} % Subtitle
	\end{twothirdswidth}
	
	\vfill % Automatic vertical whitespace
	
	\expandafter\ifstrequal\expandafter{#1}{black}{ % White circles and logo on black background
		\includegraphics[width=0.73\linewidth]{circles-white.pdf} \hfill \includegraphics[width=2cm]{logo-white.pdf} \hspace{-\rightmarginwidth+\leftmarginwidth} % Circles and logo (black)
	}{ % Black circles and logo on white background
		\includegraphics[width=0.73\linewidth]{circles-black.pdf} \hfill \includegraphics[width=2cm]{logo-black.pdf} \hspace{-\rightmarginwidth+\leftmarginwidth} % Circles and logo (white)
	}
	
	\vspace{-\footskip} % Pull the circles to the footer 
	
	\newpage
	
	\expandafter\ifstrequal\expandafter{#1}{black}{\restorepagecolor\color{black}}{} % Restore normal page background color and text color after the black title page
}

%----------------------------------------------------------------------------------------
%	COPYRIGHT PAGE
%----------------------------------------------------------------------------------------

\newcommand{\copyrightpage}[1]{
	\thispagestyle{empty} % Suppress headers and footers on this page

	~\vfill % Automatic vertical whitespace
	
	{\fontsize{8pt}{12pt}\selectfont #1\par} % Copyright text
	
	\vspace{-1.1cm} % Pull the text to the footer 
	
	\newpage
}

%----------------------------------------------------------------------------------------
%	CLOSING PAGE ONE
%----------------------------------------------------------------------------------------

\newcommand{\closingpageone}{
	\thispagestyle{empty} % Suppress headers and footers on this page

	~\vfill % Automatic vertical whitespace
	
	\begin{fullwidth} % Span the full width of the page (i.e. use the right margin)
		\raisebox{6pt}{\fontsize{8pt}{8pt}\selectfont Refli is a Hypered project} \hfill \includegraphics[width=2cm]{logo-black.pdf}\hspace{1cm}\raisebox{-0.45cm}{\includegraphics[width=4.5cm]{logo-hypered.pdf}}
	\end{fullwidth}
	
	\vspace{-\footskip-10pt} % Pull the text to the footer
}

%----------------------------------------------------------------------------------------
%	CLOSING PAGE TWO
%----------------------------------------------------------------------------------------

\newcommand{\closingpagetwo}{
	\thispagestyle{empty} % Suppress headers and footers on this page

	~\vfill % Automatic vertical whitespace
	
	\minipage[t]{0.45\linewidth}
		\vspace{0pt} % Hack required to get correct vertical alignment in minipages
		\includegraphics[width=2cm]{logo-black.pdf}
		
		\vspace{10pt} % Vertical whitespace
		
		{\fontsize{8pt}{8pt}\selectfont Refli is a Hypered project}
	\endminipage
	\hfill
	\minipage[t]{0.5\linewidth}
		\vspace{0pt} % Hack required to get correct vertical alignment in minipages
		\includegraphics[width=4.5cm]{logo-hypered.pdf}
		
		\fontsize{8pt}{12pt}\selectfont
		Hypered SRL\\
		Rue Hugo d'Oignies, 10\\
		5100 Jambes, Belgium
		
		VAT: BE652 849 986\\
		IBAN: BE12 3631 6046 2692\\
		BIC: BBRUBEBB\\
		Email: \href{mailto:hello@hypered.io}{hello@hypered.io}
	\endminipage
	
	\vspace{-\footskip+6pt} % Pull the text to the footer 
}

%----------------------------------------------------------------------------------------
%	CUSTOM COMMANDS
%----------------------------------------------------------------------------------------

\newcommand{\doctitle}[1]{\renewcommand{\doctitle}{#1}}
