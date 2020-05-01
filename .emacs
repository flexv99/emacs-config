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
;; add MELPA to repo list
;;; Code:
;; start package.el with Emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; package.el initialize
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (emms yasnippet auto-complete zones which-key use-package try spaceline smartparens python-mode org-pdfview org-gcal org-bullets nyan-mode neotree macrostep latex-extra keyfreq jedi highlight-numbers helm-core haskell-mode ggtags flycheck-irony elpy dracula-theme default-text-scale counsel-spotify counsel-gtags beacon babel auto-yasnippet auto-complete-c-headers auctex-latexmk ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
;; Disable tool-bar
(tool-bar-mode -1)
;; Key bindings for copy paste
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
;; shows bindings options
(use-package which-key
  :ensure t
  :config (which-key-mode))
;; auto completion funktion for some languages
(use-package auto-complete
  :ensure t
  ;; start autocomplete with emacs
  :config
  (require 'auto-complete))
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
;;yasnippet for emacs
(use-package yasnippet
  :ensure t
  :config
  (require 'yasnippet)
  (yas-global-mode 1))
;; function which initializes auto-complete-c-headers and gets called for c/c++ hook
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'ac-head:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/7/include"))
;; to call this funtion from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
;; debugger
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
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
(use-package dracula-theme
  :ensure t
  :config
  (require 'dracula-theme))
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

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package neotree
  :ensure t
  :config
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle))

(use-package emms
  :ensure t
  :config
  (require 'emms-player-mplayer)
  (emms-standard)
  (emms-default-players)
  (define-emms-simple-player mplayer '(file url)
    (regexp-opt '(".ogg" ".mp3" ".wav" ".mpeg" ".flac"))
    "mplayer" "-slave" "-quiet" "-relly-quiet" "-fullscreen"))

;;; .emacs ends here
