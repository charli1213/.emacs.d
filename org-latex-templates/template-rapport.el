; Création d'une classe de fichier setupfile LaTeX à utiliser.
(add-to-list 'org-latex-classes
	     '("org-report" 
	       "
% =================================BASE====================================%
\\documentclass[10pt]{article}
\\usepackage[left=2cm,right=2cm,top=2cm,bottom=2cm]{geometry} % Marges
\\usepackage[T1]{fontenc} % Nécessaire avec FrenchBabel
\\usepackage[utf8]{inputenc} % Important pour symboles Francophones, é,à,etc


% Calligraphie
\\usepackage{lmodern}
\\renewcommand{\\familydefault}{cmr} % La meilleure police (CMU Serif Roman) (Je me suis battu).
%\\usepackage{unicode-math} % À réessayer...
%\\setmathfont{Latin Modern Math} % À réessayer...
\\usepackage{mathrsfs} %Permet la command \\mathscr (Lettres attachées genre) \mathscr(B)


% Bibliographie
\\usepackage[round, sort]{natbib} % Bibliographie
\\bibliographystyle{abbrvnat}


\\usepackage{amsmath, amssymb, amsthm} % Symb. math. (Mathmode+Textmode) + Beaux théorèmes.
\\usepackage{mathtools,cancel,xfrac} % Utilisation de boîtes \\boxed{} + \\cancelto{}{}
\\usepackage{graphicx, wrapfig} % Géstion des figures.
\\usepackage{hyperref} % Permettre l'utilisation d'hyperliens.
\\usepackage{color} % Permettre l'utilisation des couleurs.
\\usepackage{colortbl} % Color tables
\\usepackage[dvipsnames]{xcolor} % Couleurs avancées.
\\usepackage{titling} % Donne accès à \\theauthor, \\thetitle, \\thedate

% Physique
\\usepackage{physics} % Meilleur package pour physicien. 
\\usepackage{pxfonts} % Rajoute PLEIN de symboles mathématiques, dont les intégrales doubles et triples

% Style
\\usepackage{lipsum} % For fun
\\usepackage{tikz} % Realisation de figures TIKZ.
\\usepackage{empheq} % Boite autour de MULTIPLE équations

% Français
\\usepackage[french]{babel} % Environnements en Français.
% ==============================BASE-(END)=================================%



% ================================SETTINGS=================================%
% Pas d'indentation en début de paragraphe :
\\setlength\\parindent{0pt}
\\setlength{\\parskip}{0.15cm}

% Tableaux/tabular
% Espace vertical dans les tabular/tableaux
\\renewcommand{\\arraystretch}{1.2}
% Couleur des tableaux/tabular
\\rowcolors{2}{violet!5}{}

% Couleurs de hyperliens :
\\definecolor{mypink}{RGB}{147, 0, 255}
\\hypersetup{colorlinks, 
             filecolor=mypink,
             urlcolor=mypink, 
             citecolor=mypink, 
             linkcolor=mypink, 
             anchorcolor=mypink}



% Numéros d'équations suivent les sections :
\\numberwithin{equation}{section} 

% Les « captions » sont en italique et largeur limitée
\\usepackage[textfont = it]{caption} 
\\captionsetup[wrapfigure]{margin=0.5cm}

% Retirer l'écriture en gras dans la table des matières
\\usepackage{tocloft}
\\renewcommand{\\cftsecfont}{\\normalfont}
\\renewcommand{\\cftsecpagefont}{\\normalfont}

% Change bullet style
\\usepackage{pifont}
\\usepackage{enumitem}
%\\setlist[itemize,1]{label=\\ding{224}}
\\setlist[itemize,1]{label=\\ding{239}}
\\renewcommand{\\boxtimes}{\\blacksquare}
% ================================SETTINGS=================================%



% ==============================NEWCOMMANDS================================%

% Vecteurs de base :
\\newcommand{\\nvf}{\\vb{\\hat{n}}}
\\newcommand{\\ivf}{\\vb{\\hat{i}}}
\\newcommand{\\jvf}{\\vb{\\hat{j}}}
\\newcommand{\\kvf}{\\vb{\\hat{k}}}
\\newcommand{\\uu}{\\vb{u}}
\\newcommand{\\vv}{\\vb{v}}
\\newcommand{\\ust}{\\vb{u}_{\\ast}}

% Physics empty spaces 
\\newcommand{\\typical}{\\vphantom{A}}
\\newcommand{\\tall}{\\vphantom{A^{x^x}_p}}
\\newcommand{\\grande}{\\vphantom{\\frac{1}{xx}}}
\\newcommand{\\venti}{\\vphantom{\\sum_x^x}}
\\newcommand{\\pt}{\\hspace{1pt}} % One horizontal pt space

% Moyenne numérique entre deux points de grilles. 
\\newcommand{\\xmean}[1]{\\overline{#1}^x}
\\newcommand{\\ymean}[1]{\\overline{#1}^y}
\\newcommand{\\zmean}[1]{\\overline{#1}^z}
\\newcommand{\\xymean}[1]{\\overline{#1}^{xy}}

% Tilde over psi
\\newcommand{\\tpsi}{\\tilde{\\psi}}
\\newcommand{\\tphi}{\\tilde{\\phi}}

% Nota Bene env : (\\ding{89})
\\newcommand{\\nb}{\\raisebox{0.8pt}{\\scriptsize\\textleaf}\\ $\\mathscr{N. B.}$\\hspace{4pt}}
\\newcommand{\\cmark}{\\ding{52}}
\\newcommand{\\xmark}{\\ding{55}}
% ==============================NEWCOMMANDS================================%



% ==============================PAGE-TITRE=================================%
% Titlepage 
\\newcommand{\\mytitlepage}{
\\begin{titlepage}
\\begin{center}
{\\Large Contrat Été 2023 \\par}
\\vspace{2cm}
{\\Large \\MakeUppercase{\\thetitle} \\par}
\\vspace{2cm}
RÉALISÉ DANS LE CADRE\\\\ D'UN PROJET POUR \\par
\\vspace{2cm}
{\\Large ISMER--UQAR \\par}
\\vspace{2cm}
{\\thedate}
\\end{center}
\\vfill
Rédaction \\\\
{\\theauthor}\\\\
\\url{charles-edouard.lizotte@uqar.ca}\\\\
ISMER-UQAR\\\\
Police d'écriture : \\textbf{CMU Serif Roman}
\\end{titlepage}
}
% ==============================PAGE-TITRE=================================%



% =================================ENTÊTE==================================%
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\setlength{\\headheight}{13pt}
\\renewcommand{\\headrulewidth}{0.025pt} % Ligne horizontale en haut

\\fancyhead[R]{\\textit{\\thetitle}}
\\fancyhead[L]{\\ \\thepage}
\\fancyfoot[R]{\\textit{\\theauthor}}
\\fancyfoot[L]{}
\\fancyfoot[C]{} 
% =================================ENTÊTE==================================%

	       [NO-DEFAULT-PACKAGES]
	       [PACKAGES]
	       [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}") ))
