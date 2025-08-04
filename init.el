;; >>>> -------------------- BASE-EMACS -------------------- >>>>

;; >> Adding custom themes path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; >> Custom file : Endroit où l'on envoie les lignes de code qui décrivent le thème.
(setq custom-file "~/.emacs.d/.custom.el")
(load "~/.emacs.d/.custom.el")

;; >> Line-break ou un retour de chariot qui fitte avec le l'indentation automatique. 
(global-visual-line-mode t)

;; >> Retirer toutes les Toolbar ; 
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; >> Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen 1)

;; >> Verbose dans le *Message*
(setq message-log-max t)

;; >>>>--------------------TOUCHES RAPIDES-------------------->>>>
;; >> Constructions rapides en mode Org.
;; > LaTeX equations
(defun insert-latex-equation ()
  "Insert a LaTeX equation in Org Mode"
  (interactive)
  (insert "\\begin{equation}\n   ")
  (save-excursion
    (insert "\n\\end{equation}")))

;; > LaTeX align
(defun insert-latex-align ()
  "Insert a LaTeX align in Org Mode"
  (interactive)
  (insert "\\begin{align}\n   ")
  (save-excursion
    (insert "\n\\end{align}")))

;; > Bash code
(defun insert-source-bash ()
  "Insert a #+begin_src bash and #+end_src"
  (interactive)
  (insert "#+begin_src bash\n >>> ")
  (save-excursion
    (insert "\n#+end_src")))

;; >> Ajout du mapping des hotkeys.
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e") #'insert-latex-equation)
  (define-key org-mode-map (kbd "C-c a") #'insert-latex-align)
  (define-key org-mode-map (kbd "C-c b") #'insert-source-bash))

;; >>>> -------------------- BULLET LIST (Org-superstar) -------------------- >>>>
;; >> Lancement de Org-Superstar lorsqu'on ouvre un fichier org.
;; https://www.reddit.com/r/emacs/comments/v55nco/starting_orgsuperstarmode_whenever_i_open_an_org/
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; >> Modification des items (bullets)
(with-eval-after-load 'org-superstar 
  (setq org-superstar-item-bullet-alist '((?* . "•")
					  (?+ . "◆")
					  (?- . "◇")))
  (setq org-superstar-headline-bullets-list '(
					      ;; Large
					      "■" "▶" "◉" "▢" "○" "✸" "✜"
					      ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠
					      ;; ♣ ♦ ☢ ❀ ◆ ◖ ▶"■""▢"
					      ;; Small
					      "►" "•"
					      ;;• ★ ▸
					      ))
  ;; >> On cache les *** dans les listes
  (setq org-hide-leading-stars t) ;
  )
		      

;; >>>> ==================== LANCEMENT ORG-MODE ==================== >>>>


;; -*- mode: elisp -*-
;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

;; >> Org-Agenda :
;; On set l'endroit où se trouvent les agenda :
;; (setq org-agenda-files '("~/Desktop/Org-Rapports/"))


;; >>>> ---------------------- ESTHÉTIQUE-ORG ---------------------- >>>>

(defun my-org-startup-hook-functions ()
  ;; >> Indentation activée à l'ouverture d'un fichier org. Ça améliore grandement la lecture du fichier.
  (org-indent-mode t)
  ;; >> Équations en LaTeX seront highligtées.
  (setq org-highlight-latex-and-related '(native))
  ;; >> begin_src conservent l'indentation au lieu de s'en débarrasser à la compilation
  (setq org-src-preserve-indentation t)
  ;; >> largeur de base des images
  (setq org-image-actual-width nil)
  ;; Enable transient mark mode
  (setq transient-mark-mode 1)
  ;; >> retirer les indications ou les "markup symbols" comme /abcd/ et *abcd* dans le fichier org. 
  (setq org-hide-emphasis-markers t)
  ;; >> Rendre les blocs de code invisibles
  (setq org-hide-block-startup t)
  ;; >> Les "tables" sont maintenant écrasées par défault. 
  (setq org-startup-align-all-tables t)
  ;; >> Personnaliser l'apparence des blocs de code
  (require 'color)
  (let ((block-color-test
	 (color-darken-name
	  (color-saturate-name
	   (face-attribute 'default :background) 30) -30)
	 )
	)
    (set-face-attribute 'org-block nil
			:background block-color-test
			:foreground (color-darken-name (face-attribute 'default :foreground) -50)
			)
    (set-face-attribute 'org-block-begin-line nil
			:foreground block-color-test
			:background block-color-test
			)
    (set-face-attribute 'org-block-end-line nil
			:foreground block-color-test
			:background block-color-test
			)
    )
  )

;; >>> Ajout des hooks au lancement d'Org-Mode >>>
(add-hook 'org-mode-hook #'my-org-startup-hook-functions)
;; <<< Fin des hooks <<<


;; >>>> ==================== ORG-PDF-LATEX ==================== >>>>
;; >>>> ---------------------- Citations ---------------------- >>>>
(with-eval-after-load 'org-superstar
  ;; >> Bibliographie 
  ;; Pour utiliser, avoir dernière version de Org. pour regarder
  ;; M-x org-version
  ;; Pour installer : 
  ;; M-x list-package (RET) Trouver org (Ret) installer.
  ;; M-x list-package (RET) Trouver citeproc (Ret) installer.
  (require 'citeproc)    ;;; Problème de versions avec Org-mode?

  ;; >>>> -------------- Exportation Org-LaTeX-PDF -------------- >>>>
  (require 'ox-latex) ;;; Problème de versions avec Org-mode?

  ;; >> Titre et sous-titres séparés :
  (setq org-latex-subtitle-format "\\newcommand{\\thesubtitle}{%s}")
  (setq org-latex-subtitle-separate t)

  ;; >> PDFLaTeX et Latexmk :: 
  ;; Problème de path (Sinon Emacs prend le PATH du exec-path et non du PATH définit
  ;; par le shell. Tous les compilateurs LaTeX ne fonctionne plus sinon.
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))

  ;; >> Publication :: fonction de publication "org-latex-pdf-process"
  ;;  > Latexmk sert à faire plusieurs "loop" pour avoir accès à la table des matières et
  ;;    aux citations. Il roule pdflatex 3 fois, essentiellement.
  ;;  > Citations :: Faut lui dire de prendre Bibtex pour que pdfLaTeX produise un bbl file
  ;;    voir https://tex.stackexchange.com/questions/197707/
  ;;  > On exporte les fichiers de compilations auxilières dans tmp, parce que latexmk ne
  ;;    les supprime pas tous (malheureusement) -- donc pour garder ça «clean». 
  (setq org-latex-pdf-process
	;;	'("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -auxdir=tmp -f %f"))
	'("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -auxdir=aux -f %f"))
  ;; >> Citations :: Gestion des citations : 
  ;;  > On choisit le processeur : Biblatex, Natbib ou Bibex.
  ;;  > On peut connecter des tuples pour forcer un style ("phys" par exemple ici).
  ;;  > "t" active la switch "tout le reste". 
  (setq org-cite-export-processors
	'((latex . (biblatex "authoryear")) ;; fichiers latex
	  (beamer natbib) ;; pour les beamers
	  (t csl))) ;; Pour tout le reste


  ;; >> On dit à notre compilateur que ces fichiers sont des "tmp" pour que latexmk
  ;;    les supprime. 
  (setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xml")))


  ;; >> Publication :: Options de simple/multiples publication/s :
  (setq org-publish-project-alist
	'(
          ("org-notes-latex"
           :base-directory "."
           :base-extension "org"
           :publishing-directory "."
           :recursive nil
           :publishing-function org-latex-publish-to-pdf
           :headline-levels 4
	   :completion-function (lambda (plist)
				  (message "Executing completion function")
				  (shell-command "mv *.pdf Fichiers_pdf/")
				  (shell-command "mv *.tex Fichiers_tex/")
				  (shell-command "rm -rf aux")
				  (message "Compilation et réorganisation accomplies"))	 
	   )))



  ;; >>> -------------------- ORG-LATEX-CLASSES -------------------- >>>
  ;; >> Org-Latex-Classes : On crée la variable org-latex-classes (variable vide)
  (setq org-latex-classes nil)

  ;;  > On lit notre préambule qu'on enregistre dans une variable "contenu-preambule".
  (defvar contenu-preambule nil "Ligne de texte qui stocke le préambule.")
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/org-latex-templates/preambule_docurapport.tex")
    (setq contenu-preambule (buffer-string)))
  (setq contenu-preambule (concat contenu-preambule
				  "
                    	 [NO-DEFAULT-PACKAGES]
                    	 [PACKAGES]
                    	 [EXTRA]")
	)

  ;;  > On crée deux latex classes qu'on ajoute à la liste org-latex-classes :
  (add-to-list 'org-latex-classes
               ( list "org-report"
		 (concat "\\documentclass[10pt]{article}" contenu-preambule)
		 '("\\section{%s}" . "\\section*{%s}")
		 '("\\subsection{%s}" . "\\subsection*{%s}")
		 '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 '("\\paragraph{%s}" . "\\paragraph*{%s}")))

  (add-to-list 'org-latex-classes
               (list "org-document"
		     (concat "\\documentclass[10pt]{report}" contenu-preambule)
		     '("\\chapter{%s}" . "\\chapter*{%s}")
		     '("\\section{%s}" . "\\section*{%s}")
		     '("\\subsection{%s}" . "\\subsection*{%s}")
		     '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		     '("\\paragraph{%s}" . "\\paragraph*{%s}")))

  ;;  > Classe LaTeX par défault. 
  (setq org-latex-default-class "org-report")

)
