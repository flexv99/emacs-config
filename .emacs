;;; Package --- Summary
;;; Code:
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
;; last update 30.11.2020

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(geiser restclient org emojify json-mode mu4e-maildirs-extension magit projectile mu4e-conversation mu4e-alert smartparens doom-themes wsd-mode org-download epresent latex-math-preview pdf-tools tablist org-bullets nix-mode haskell-mode prettier-js tide rjsx-mode hy-mode company flycheck yasnippet-snippets yasnippet ggtags auto-complete-c-headers auto-complete which-key neotree highlight-numbers ace-window default-text-scale nyan-mode spaceline all-the-icons counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(setq inhibit-startup-screen t)
;; start package.el eith Emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; initialize package.el
(package-initialize)
;; Disable tool-bar
(tool-bar-mode -1)
;; Copy paste setup
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(transient-mark-mode 1) ;; No region when it is not highlighted
;; use spaces instead of tab, to convert tabbed document to spaced one selct the whole doc (C-x h; M-x unatbify)
;; Editor plug ins
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
;; theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;; M-x all-the-icons-fonts to install them!
(use-package all-the-icons
  :ensure t)
;; disable scroll-bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
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

(use-package emojify
  :hook (add-hook 'after-init-hook #'global-emojify-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (require 'smartparens-config))

;; shows directories
(use-package neotree
  :ensure t
  :config
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
(setq-default indent-tabs-mode nil)

;; shows bindings options
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Programming plug-ins
;; git interface
(use-package magit
  :ensure t)
;; project manager
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/work/02_src/")))
;; auto completion funcion for some languages
(use-package auto-complete
  :ensure t
  ;; start autocomplete with emacs
  :config
  (require 'auto-complete)) ;;a-c mode 4 org-mode
;; c-specific auto complete
(use-package auto-complete-c-headers
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

;; syntax checker
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
;; python stuff
;; python auto-complete
;; elpy-dep
(use-package company
  :ensure t
  :config
  (setq initial-major-mode 'company))
;; interpreter switch to default
;; elpy-dep
(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "#536878")
  (set-face-background 'highlight-indentation-current-column-face "#657E91"))
;; elpy-dep
(use-package pyvenv
  :ensure t)
;; elpy-dep
;; synatx templates
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)
(use-package s
  :ensure t)
(use-package elpy
  :ensure t
  :init
  (elpy-enable))
;; lisp flavoured python
(use-package hy-mode
  :ensure t
  :mode "\\.hy\\'")

;; javascript stuff
;; rest api tester
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

(use-package rjsx-mode
  :ensure t
  :config
  (setq js-indent-level 2))

;; disable jshint I prefer eslint
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package tide
  :ensure t
  :after (rjsx-mode flycheck)
  :hook (rjsx-mode . setup-tide-mode))

;; requires typescript
(defun setup-tide-mode()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mod +1))

(use-package prettier-js
  :ensure t
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))

;; json utilities
(use-package json-snatcher
  :ensure t
  :config
  (require 'json-snatcher))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; javascript-autocompletion
;; tipe clone repo into .emacs.d and inside do npm install
(add-to-list 'load-path "~/.emacs.d/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'rjsx-mode-hook (lambda () (tern-mode t)))

;; FUNCTIONAL STUFF
;; haskell integration
(use-package haskell-mode
  :ensure t
  :config
  (require 'haskell-mode))

(use-package nix-mode
  :ensure t)

(use-package geiser
  :ensure t)


;; Text manipulation and utilities
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
        ("r"  . pdf-view-reset-slice)))

;; math-latex
(use-package latex-math-preview
  :ensure t)

;; org-epresent(KISS_presentation)
(use-package epresent
  :ensure t)
(use-package org-download
  :ensure t
  :config
  (require 'org-download)
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package wsd-mode
  :ensure t)

(global-set-key (kbd "C-c") 'comment-region)
(global-set-key (kbd "C-u") 'uncomment-region)

;; (find-file "/home/flex/my-todos/todos.org")

;; MU4E stuff
(load-file "~/.config/mu4e/mu4econf.el")
(add-hook 'mu4e-compose-mode-hook 'turn-on-flyspell)
(add-hook 'mu4e-compose-mode-hook 'turn-on-auto-fill)
(use-package mu4e-conversation
  :ensure t)
(use-package mu4e-maildirs-extension
  :ensure t)

(use-package org-mime
  :ensure t
  :config
  (require 'org-mime)
  (setq org-mime-library 'mml))
;;; .emacs ends here
