;; Création d'une classe de fichier setupfile LaTeX à utiliser.
(add-to-list 'org-latex-classes
	     '("org-report" 
	       "
% =================================BASE====================================%
\\documentclass[10pt]{article}
\\usepackage[left=2cm,right=2cm,top=2cm,bottom=2cm]{geometry} % Marges
\\usepackage[utf8]{inputenc} % Important pour symboles Francophones, é,à,etc.
\\usepackage[T1]{fontenc} % Nécessaire avec FrenchBabel
%\\usepackage{lmodern}
\\renewcommand{\\familydefault}{cmr} % La meilleure police (CMU Serif Roman) (Je me suis battu).

\\usepackage{natbib} % Bibliographie
\\bibliographystyle{abbrvnat}



\\usepackage{amsmath, amssymb, amsthm} % Symb. math. (Mathmode+Textmode) + Beaux théorèmes.

\\usepackage{mathtools, cancel} % Utilisation de boîtes \\boxed{} + \\cancelto{}{}
\\usepackage{graphicx, wrapfig} % Géstion des figures.
\\usepackage{hyperref} % Permettre l'utilisation d'hyperliens.
\\usepackage{color} % Permettre l'utilisation des couleurs.
\\usepackage[dvipsnames]{xcolor} % Couleurs avancées.
\\usepackage{titling} % Donne accès à \\theauthor, \\thetitle, \\thedate

% >>> Physique >>>
\\usepackage{physics} % Meilleur package pour physicien. 
\\usepackage{pxfonts} % Rajoute PLEIN de symboles mathématiques, dont les intégrales doubles et triples
% <<< Physique <<<

\\usepackage{lipsum} % For fun
\\usepackage{tikz} % Realisation de figures TIKZ.
\\usepackage{empheq} % Boite autour de MULTIPLE équations

\\usepackage[french]{babel} % Environnements en Français.
% ==============================BASE-(END)=================================%



% ================================SETTINGS=================================%
% Pas d'indentation en début de paragraphe :
\\setlength\\parindent{0pt} 

% Couleurs de hyperliens :
\\definecolor{mypink}{RGB}{147, 0, 255}
\\hypersetup{colorlinks, urlcolor=mypink, citecolor=mypink, linkcolor=mypink}

% Numéros d'équations suivent les sections :
\\numberwithin{equation}{section} 

% Les « captions » sont en italique et largeur limitée
\\usepackage[textfont = it]{caption} 
\\captionsetup[wrapfigure]{margin=0.5cm}


% Retirer le l'écriture en gras dans la table des matières
\\usepackage{tocloft}
\\renewcommand{\\cftsecfont}{\\normalfont}
\\renewcommand{\\cftsecpagefont}{\\normalfont}

% Change bullet style
\\usepackage{pifont}
\\usepackage{enumitem}
\\setlist[itemize,1]{label=\\ding{224}}
% ================================SETTINGS=================================%



% ==============================NEWCOMMANDS================================%
% Degrés Celsius :
\\newcommand{\\celsius}{${}^\\circ$ C} % \\degrée Celsius : Pas mal plus simple qu'utilise le package gensymb qui plante avec tout...

% Vecteurs de base :
\\newcommand{\\nvf}{\\hat{\\vb{n}}}
\\newcommand{\\ivf}{\\vb{\\hat{i}}}
\\newcommand{\\jvf}{\\hat{\\vb{j}}}
\\newcommand{\\kvf}{\\hat{\\vb{k}}}

% Boîte vide pour ajuster les underbrace
\\newcommand{\\bigno}{\\vphantom{\\qty(\\frac{d}{q})}}
\\newcommand{\\pt}{\\hspace{1pt}}

% Moyenne numérique entre deux points de grilles. 
\\newcommand{\\xmean}[1]{\\overline{#1}^x}
\\newcommand{\\ymean}[1]{\\overline{#1}^y}



% Tilde over psi
\\newcommand{\\tpsi}{\\tilde{\\psi}}
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
ISMER-UQAR
\\end{titlepage}
}
% ==============================PAGE-TITRE=================================%



% =================================ENTÊTE==================================%
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\setlength{\\headheight}{13pt}
\\renewcommand{\\headrulewidth}{1.3pt} % Ligne horizontale en haut

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
