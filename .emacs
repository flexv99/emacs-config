;;; Package --- Summary
;;; Code:
;;; Commentary:
;;   __ _                              __ _
;;  / _| | _____  __   ___ ___  _ __  / _(_) __ _
;; | |_| |/ _ \ \/ /  / __/ _ \| '_ \| |_| |/ _` |
;; |  _| |  __/>  <  | (_| (_) | | | |  _| | (_| |
;; |_| |_|\___/_/\_\  \___\___/|_| |_|_| |_|\__, |
;;                                          |___/
;; Felix Valentini
;; Emcas configuration v.2
;; last update 30.11.2020

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(web-mode eglot elgot hindent keycast dockerfile-mode disable-mouse yaml-mode multiple-cursors rainbow-delimiters linum-relative f paredit geiser restclient org emojify json-mode mu4e-maildirs-extension magit projectile mu4e-conversation mu4e-alert smartparens doom-themes wsd-mode org-download epresent latex-math-preview pdf-tools tablist org-bullets nix-mode haskell-mode prettier-js rjsx-mode hy-mode company flycheck yasnippet-snippets yasnippet ggtags auto-complete-c-headers auto-complete which-key neotree highlight-numbers ace-window default-text-scale nyan-mode spaceline all-the-icons counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;; start package.el eith Emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;; add exec-path for external programs
(add-to-list 'exec-path "/home/flex99/.local/bin")
(add-to-list 'exec-path "/home/flex99/.yarn/bin")
(setenv "PATH" (concat (getenv "PATH") ":/home/flex99/.yarn/bin"))
;; initialize package.el
(package-initialize)
(setq inhibit-startup-screen t)
;; Disable tool-bar
(tool-bar-mode -1)
;; enable whitespace-mode
;; (require 'whitespace)
;; (global-whitespace-mode 1)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1) ;; No region when it is not highlighted
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono" ))
;; (set-face-attribute 'default t :font "DejaVu Sans Mono" )
;; use spaces instead of tab, to convert tabbed document to spaced one selct the whole doc (C-x h; M-x unatbify)
;; Editor plug ins
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
  :ensure t
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
(use-package linum-relative
  :ensure t
  :config
  (require 'linum-relative)
  (linum-relative-global-mode t)
  (setq linum-relative-backend 'display-line-numbers-mode))

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
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package emojify
  :ensure t
  :hook (after-init-hook #'global-emojify-mode))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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

;; shows typed binding
(use-package keycast
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
	(add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" mode-line-keycast " "))
  (keycast-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
  (add-hook 'after-init-hook 'global-company-mode)
  (setq initial-major-mode 'company))

(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "#536878")
  (set-face-background 'highlight-indentation-current-column-face "#657E91"))
;; elpy-dep
(use-package pyvenv
  :ensure t)
;; ipython interpreter
(defun ipython ()
    (interactive)
    (term "/usr/bin/ipython"))

;; synatx templates
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))
(setq python-shell-interpreter "/usr/bin/python3")

;; lisp flavoured python
(use-package hy-mode
  :ensure t
  :mode "\\.hy\\'")

;; javascript stuff
;; rest api tester
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

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

;; yaml utilities
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode))

;; Dockerfile mode
(use-package dockerfile-mode
  :ensure t
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; FUNCTIONAL STUFF
;; haskell integration
(use-package hindent
  :ensure t)

;; hs-linter
;; ref: https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hs-lint.el
(add-to-list 'load-path "~/.emacs.d/elpa/hs-lint/")
(load "hs-lint")
(defun my-haskell-hslint-hook ()
    (local-set-key "\C-cl" 'hs-lint))
(add-hook 'haskell-mode-hook 'my-haskell-hslint-hook)

(use-package haskell-mode
  :ensure t
  :config
  (require 'haskell-mode)
  ;; (setq haskell-process-type 'cabal-repl)
  ;; (setq haskell-process-log t)
  (add-hook 'haskell-mode-hook 'haskell-indent-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (custom-set-variables '(haskell-process-type 'stack-ghci)))

;; ghcid
;; file: https://raw.githubusercontent.com/ndmitchell/ghcid/master/plugins/emacs/ghcid.el
(add-to-list 'load-path "~/.emacs.d/elpa/ghcid/")
(load "ghcid")

(use-package geiser
  :ensure t)

;; modern list API fir emacs-lisp
(use-package dash
  :ensure t)

;; string manipulation API for emacs-lisp
(use-package s
  :ensure t)

(use-package f
  :ensure t)

;; lisp utility
(use-package paredit
  :ensure t)

;; Text manipulation and utilities
;; org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode)))

;; lsp support
(use-package eglot
  :ensure t
  :hook web-mode
  :config
  (add-to-list 'eglot-server-programs '(web-mode "typescript-language-server --stdio")))

;;; init.el ends here
