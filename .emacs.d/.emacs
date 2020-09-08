;;; Package --- Summary
;;; Commentary:
;;; ____________   __            __
;;; |  _________|  \ \          / /
;;; | |             \ \        / /
;;; | |___           \ \      / /
;;; |  ___|           \ \    / /
;;; | |                \ \  / /
;;; | |                 \ \/ /
;;; |_|                  \__/
;;;
;; Felix Valentini
;; Emcas configuration v.2
;; last update 26.03.2020
;;
;; First set up connection to melpa (35-36) and then install use-package,
;; after that you will be able to load the config-file
;; 
;; add MELPA to repo list
;;; Code:
(setq inhibit-startup-screen t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (irony-eldoc company-mode ac-company afternoon-theme sanityinc-tomorrow-bright pyvenv hy-mode tablist pylint jedi org-download epresent pdfview org-pdfview pdf-tools org-present nix-mode haskell-mode org-bullets emms ggtags auto-complete-c-headers yasnippet-snippets smartparens neotree highlight-numbers ace-window default-text-scale nyan-mode spaceline dracula-theme counsel flycheck yasnippet auto-complete which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
;; start package.el with Emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; package.el initialize
(package-initialize)
;; Disable tool-bar
(tool-bar-mode -1)
;; Key bindings for copy paste
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
;; use spaces instead of tab, to convert tabbed document to spaced one selct the whole doc (C-x h; M-x unatbify)
(setq-default indent-tabs-mode nil)
;; shows bindings options
(use-package which-key
  :ensure t
  :config (which-key-mode))
;; auto completion funcion for some languages
(use-package auto-complete
  :ensure t
  ;; start autocomplete with emacs
  :config
  (require 'auto-complete)) ;;a-c mode 4 org-mode
;; company especially for hy-lang
(use-package company
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.hy\\'" . company)))
;;  el-doc
(use-package irony-eldoc
  :ensure t
  :config
  (require 'irony-eldoc))
(use-package auto-complete-c-headers
  :ensure t)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
;;yasnippet for emacs
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)
;; function which initializes auto-complete-c-headers and gets called for c/c++ hook
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
;; to call this funtion from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
;; ggtags to find out where the dependecies of a function or class are store and to easily open them
;; REQUIRES the package GLOBAL (apt on debian)
(use-package ggtags
  :ensure t
  :config
  (add-hook 'C-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode)
		(ggtags-mode 1)))))
;; debugger
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
;; modes
;; TEMPORARY Python-DJANGO shell (DOLASILLA Project)
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i /home/flex/work/02_dolasilla/dolasilla_seite/manage.py shell -i ipython") ;; sets python3 as default interpreter
;; to load virtualenv's in python-mode
(use-package elpy
  :ensure t
  :init
  (elpy-enable))
;; jedi ac for python
(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))
;; htm-mode
(add-to-list 'auto-mode-alist '("\\.css$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cfm$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(use-package hy-mode
  :ensure t
  :config
  (require 'hy-mode))
;; haskell integration
(use-package haskell-mode
  :ensure t
  :config
  (require 'haskell-mode))
;; nix-mode to write nix-expressions
(use-package nix-mode
  :ensure t)
;; iserch with shown result
(use-package counsel
  :ensure t)
(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)))

;; appearence
;; theme
(use-package afternoon-theme
  :ensure t
  :config
  (require 'afternoon-theme))
;; (use-package dracula-theme
;;   :ensure t
;;   :config
;;   (require 'dracula-theme))
;; Mod line configuration, used package: spaceline
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))
;; nyan-mode for mod line
(use-package nyan-mode
  :ensure t
  :config
  (require 'nyan-mode)
  (nyan-mode t))
;; font scaling
(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))
(define-key ctl-x-map [(control ?0)] 'zoom-in/out)
;; line numeration
(global-linum-mode)
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
;; window management
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))121))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;;functionality
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))
;; shows directories
(use-package neotree
  :ensure t
  :config
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle))
;; emacs multimedia system
(use-package emms
  :ensure t)

;; org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;org-beamer for presentations
;; allow for export=>beamer by placing

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
             '("beamer"
               "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\(not )
      \\usepackage{verbatim}\n
      \\(insert )nstitute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"
               
               ("\\section{%s}" . "\\section*{%s}")
               
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

;; letter class, for formal letters

(add-to-list 'org-export-latex-classes
             
             '("letter"
               "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\(not )
      \\usepackage{color}"
               
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;pdf-tools to open pdfs
(use-package let-alist
  :ensure t) ;; dependency
(use-package tablist
  :ensure t) ;; dependency
(use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (bind-keys :map pdf-view-mode-map
        ("\\" . hydra-pdftools/body)
        ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
        ("g"  . pdf-view-first-page)
        ("G"  . pdf-view-last-page)
        ("l"  . image-forward-hscroll)
        ("h"  . image-backward-hscroll)
        ("j"  . pdf-view-next-page)
        ("k"  . pdf-view-previous-page)
        ("e"  . pdf-view-goto-page)
        ("u"  . pdf-view-revert-buffer)
        ("al" . pdf-annot-list-annotations)
        ("ad" . pdf-annot-delete)
        ("aa" . pdf-annot-attachment-dired)
        ("am" . pdf-annot-add-markup-annotation)
        ("at" . pdf-annot-add-text-annotation)
        ("y"  . pdf-view-kill-ring-save)
        ("i"  . pdf-misc-display-metadata)
        ("s"  . pdf-occur)
        ("b"  . pdf-view-set-slice-from-bounding-box)
        ("r"  . pdf-view-reset-slice))
    )

;; org-epresent(KISS_presentation)
(use-package epresent
  :ensure t)
(use-package org-download
  :ensure t
  :config
  (require 'org-download)
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable))

;;; .emacs ends here