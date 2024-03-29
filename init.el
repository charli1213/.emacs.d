;;(setq current-directory default-directory)

;; S'assurer que MELPA fonctionne et que la fonction use-package existe
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-install 'use-package)
;; M-x package-install
;; Doc : https://melpa.org/#/getting-started

;; Ajout du chemin des themes installés : 
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Custom file ( On envoie toute la scrap des themes là ) :
(setq custom-file "~/.emacs.d/.custom.el")
;; On load la scrap (Le theme
(load "~/.emacs.d/.custom.el")

;; Transparence : 
(set-frame-parameter (selected-frame) 'alpha '(95 . 100))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

;; Retirer toutes les Toolbar ; 
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Emacs commence à gauche (width=105 for work computer)
(setq default-frame-alist '((left . 0) (width . 105) (fullscreen . fullheight)))


;; >>> Keybinding :
;; ----------------------------------------
;; >> LaTeX equations
(defun insert-latex-equation ()
  "Insert a LaTeX equation in Org Mode"
  (interactive)
  (insert "\\begin{equation}\n   ")
  (save-excursion
    (insert "\n\\end{equation}")))

;; >> LaTeX align
(defun insert-latex-align ()
  "Insert a LaTeX align in Org Mode"
  (interactive)
  (insert "\\begin{align}\n   ")
  (save-excursion
    (insert "\n\\end{align}")))

;; >> LaTeX Nota Bene
(defun insert-latex-nota-bene ()
  "Insert a Nota Bene in Org Mode"
  (interactive)
  (insert "\\nb\\begin{minipage}[t]{0.9\\linewidth}
\\itshape \n   ")
   (save-excursion
     (insert "\n\\end{minipage}")))

;; >> 
(defun insert-source-bash ()
  "Insert a #+begin_src bash and #+end_src"
  (interactive)
  (insert "#+begin_src bash\n  ")
  (save-excursion
    (insert "\n#+end_src")))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e") #'insert-latex-equation)
  (define-key org-mode-map (kbd "C-c a") #'insert-latex-align)
  (define-key org-mode-map (kbd "C-c n") #'insert-latex-nota-bene)
  (define-key org-mode-map (kbd "C-c b") #'insert-source-bash))


;; >>>> Org-Mode (Début) >>>>
;; ----------------------------------
;; >> Bibliographie 
;; Pour utiliser, avoir dernière version de Org. pour regarder
;; M-x org-version
;; Pour installer : 
;; M-x list-package (RET) Trouver org (Ret) installer.
;; M-x list-package (RET) Trouver citeproc (Ret) installer.
(require 'citeproc)

;; >> Org-Agenda :
;; On set l'endroit où se trouvent les agenda :
(setq org-agenda-files '("~/Desktop/Org-Rapports/"))

;; >> PDFLaTeX : 
;; > Citations :: Faut lui dire de prendre Bibtex pour que pdfLaTeX produise un bbl file
;; https://tex.stackexchange.com/questions/197707/
(setq org-latex-pdf-process
  '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
;; > Path :On rajoute PDFLaTeX au path, car il ne le trouve pas, des fois.
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2023/bin/x86_64-linux/"))

;; >> Org-Latex-Classes : On crée la variable org-latex-classes (variable vide)
(setq org-latex-classes nil)
;; On load les org-latex-classes en fichier-template emacs-lisp (Autant qu'on veut).
(load "~/.emacs.d/org-latex-templates/template-rapport.el")

;; >> Esthetique-Org :
;; > Indentation activée à l'ouverture d'un fichier org. Ça améliore grandement la lecture du fichier.
(setq org-startup-indented t)
;; > Line-break ou un retour de chariot qui fitte avec le l'indentation automatique. 
(global-visual-line-mode t)
;; > Équations en LaTeX seront highligtées.
(setq org-highlight-latex-and-related '(native))
;; > begin_src conservent l'indentation au lieu de s'en débarrasser à la compilation
(setq org-src-preserve-indentation t)
;; > largeur de base des images
(setq org-image-actual-width nil)

;; > Items : On change le type d'items pour les sections en org:
;; (package-install 'org-bullets) si ça marche pas
(use-package org-bullets
  :hook (( org-mode ) . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '(;;; Large
    "❀"
    "◉"
    "☀"
    "○"
    "✸"
    "✜"
    ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
    "►"
    "•"
    ;;• ★ ▸
    )))

;; ----------------------------------
;; <<<< Org-Mode (Fin) <<<<
