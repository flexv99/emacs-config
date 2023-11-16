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
;; last update 2023-04-25

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type 'auto)
 '(package-selected-packages
   '(markdown-mode lsp-haskell elpy pyvenv highlight-indentation lsp-treemacs lsp-ivy helm-lsp lsp-ui vue-mode lsp-mode web-mode elgot hindent keycast dockerfile-mode disable-mouse yaml-mode multiple-cursors rainbow-delimiters linum-relative f paredit geiser restclient org emojify json-mode mu4e-maildirs-extension magit projectile mu4e-conversation mu4e-alert smartparens doom-themes wsd-mode org-download epresent latex-math-preview pdf-tools tablist org-bullets nix-mode haskell-mode prettier-js rjsx-mode hy-mode company flycheck yasnippet-snippets yasnippet ggtags auto-complete-c-headers auto-complete which-key neotree highlight-numbers ace-window default-text-scale nyan-mode spaceline all-the-icons counsel use-package)))
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
;; initialize package.el
(package-initialize)
(setq inhibit-startup-screen t)
;; Disable tool-bar
(tool-bar-mode -1)
;; enable whitespace-mode
;; (require 'whitespace)
;; (global-whitespace-mode 1)
(transient-mark-mode 1) ;; No region when it is not highlighted
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; save annoying backupfiles outside of the working directories
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
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
       ((t (:inherit ace-jump-face-foreground :height 3.0))))) 121))

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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c f") 'projectile-find-file))

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

(use-package haskell-mode
  :ensure t
  :config
  (require 'haskell-mode)
  (add-hook 'haskell-mode-hook #'flycheck-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-type 'cabal-repl)
  (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
  (add-hook 'haskell-mode-hook 'my-haskell-hslint-hook)
  (define-key haskell-mode-map (kbd "M-[") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "M-]") 'haskell-navigate-imports-return)
  (custom-set-variables '(haskell-process-type 'auto))
  ;; https://github.com/erikbackman/haskell-ts-mode/tree/master
  (add-to-list 'load-path (concat user-emacs-directory "haskell-ts-mode"))
  (require 'haskell-ts-mode))

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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (vue-mode . lsp)
         (python-mode . lsp)
         (js-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-vetur-format-default-formatter-css "none")
  (lsp-vetur-format-default-formatter-html "none")
  (lsp-vetur-format-default-formatter-js "none")
  (lsp-vetur-validation-template nil)
  :commands lsp)

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode . prettier-js-mode)
  :config
  (add-hook 'vue-mode-hook #'lsp)
  (setq prettier-js-args '("--parser vue"))
  ;; https://github.com/8uff3r/vue-ts-mode
  (add-to-list 'load-path (concat user-emacs-directory "vue-ts-mode"))
  (require 'vue-ts-mode))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(setq treesit-language-source-alist
   '((css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (vue "https://github.com/ikatyang/tree-sitter-vue")))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)
   (haskell-mode . haskell-ts-mode)
   (markdown-mode . markdown-ts-mode)))

;;; init.el ends here
