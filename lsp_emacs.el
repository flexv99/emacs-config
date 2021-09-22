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
;; last update 22.09.2020

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(restclient dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company lsp-ui company hover)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;; setting up the PATH in emacs
(setenv "PATH" (concat "/home/flex/sources/flutter/bin" ":"
		       "/home/flex/go/bin" ":"
		       "/usr/local/go/bin" ":"
		       "/home/flex/.nvm/versions/node/v15.5.1/bin" ":"
		       (getenv "PATH")))

(setq epg-gpg-program "gpg2")

;; start package.el with Emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;; add exec-path for external programs
(add-to-list 'exec-path "/home/flex/.local/bin")
;; initialize package.el
(package-initialize)

;; space instead of tab
(setq-default indent-tabs-mode nil)

(setq inhibit-startup-screen t)
;; Disable tool-bar
(tool-bar-mode -1)
;; comment/uncomment bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; Font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono" ))
(set-face-attribute 'default t :font "DejaVu Sans Mono" )

;; Editor plug ins
(use-package disable-mouse
  :ensure t
  :config
  (require 'disable-mouse)
  (global-disable-mouse-mode))

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

(use-package flycheck
  :ensure t
  ;; :init
  ;; (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-flycheck-mode))

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

(use-package rainbow-delimiters
  :ensure t
  :hook (progn-mode))

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

;; git interface
(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)))

;; project manager
(use-package projectile
  :ensure t
  :config
  ;; (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/work/vertical-life/02_src/")))

;; template system
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; completion setup
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode t))

(use-package highlight-indentation
  :ensure t
  :hook
  (python-mode . highlight-indentation-mode)
  (yaml-mode . highlight-indentation-mode)
  :config
  (set-face-background 'highlight-indentation-face "#536878")
  (set-face-background 'highlight-indentation-current-column-face "#657E91"))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ;; flycheck instead
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((python-mode c-mode dart-mode js-mode) . lsp))

(use-package lsp-ui
  :ensure t)

;; json utilities
(use-package json-snatcher
  :ensure t
  :config
  (require 'json-snatcher))

;; rest api tester
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package prettier-js
  :ensure t
  :after (js-mode)
  :hook (js-mode . prettier-js-mode))

;; yaml utilities
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode))

(use-package pyvenv
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(setq package-selected-packages 
  '(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
    ;; Optional packages
    lsp-ui company hover))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(setq lsp-dart-flutter-sdk-dir "/home/flex/sources/flutter")

(add-hook 'dart-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil)

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp)
  :init
  (dap-register-debug-template "Flutter :: Custom debug"
                               (list :flutterPlatform "x86_64"
                                     :program "lib/main_debug.dart"
                                     :args '("--flavor" "customer_a"))))

(use-package hover
  :after dart-mode
  ;; :bind (:map dart-mode-map
  ;;             ("C-M-z" . #'hover-run-or-hot-reload)
  ;;             ("C-M-x" . #'hover-run-or-hot-restart)
  ;;             ("C-M-p" . #'hover-take-screenshot'))
  :init
  (setq hover-flutter-sdk-path "/home/flex/sources/flutter/bin/flutter"
	hover-command-path "/home/flex/go/bin/hover"
	hover-hot-reload-on-save t
        hover-screenshot-path (concat (getenv "HOME") "/Pictures"
				      hover-screenshot-prefix "screen-shot-"
				      hover-observatory-uri "http://127.0.0.1:50300"
				      hover-clear-buffer-on-hot-restart t)))

;; emacs.el ends here
