;;; package -- Summary:
;;; Commentary:
;; 
;; ____________   __            __
;; |  _________|  \ \          / /
;; | |             \ \        / /
;; | |___           \ \      / /
;; |  ___|           \ \    / /
;; | |                \ \  / /
;; | |                 \ \/ /
;; |_|                  \__/
;;
;; Felix Valentini
;; Emcas configuration
;; last update 05.10.2019
;; 
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (macro-math magit beacon python-mode elpy nyan-mode keyfreq smartparens default-text-scale counsel-spotify auto-yasnippet flycheck-irony neotree spaceline babel netree org-gcal company auto-complete-c-headers auctex-latexmk latex-extra haskell-mode flycheck org-pdfview auto-complete try counsel swiper ace-window dracula-theme org-bullets which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(setq inhibit-startup-massage t)
(tool-bar-mode -1)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-packages)
;;  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;Org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'org-archive)
(setq org-archive-save-context-info nil)
(setq org-archive-location "::* ARCHIVE TASKS")

(defalias 'list-buffers 'ibuffer)

;; theme and look
(add-to-list'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

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
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))121
       ))
(use-package counsel
  :ensure t
  )
(use-package swiper
  :ensure try
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
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    ))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;; Pdf tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  )

(use-package org-pdfview
  :ensure t)

;;coding stuff
;; python IDE
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; python mode
(add-to-list 'load-path "~/.emacs.d/elpa/python-mode-20190912.1653")
(require 'python-mode)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
  )

(use-package auto-yasnippet
  :ensure t)

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;;Key bindings for copy paste
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; Mod line configuration, used package: spaceline
(require 'spaceline-config)
(spaceline-emacs-theme)
;; nyan-mode
(require 'nyan-mode)
(nyan-mode t)

(require 'pdf-tools)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;;auto-complete for c programs(require 'company), flycheck
(require 'company)
(add-hook 'global-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)

(eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;Google calendar
(setq package-check-signature nil)

;;(use-package org-gcal
;;  :ensure t
;;  :config
  (require 'org-gcal)
  (setq org-gcal-client-id "891581394610-trsr6lpo52mu7vjpvvul7n3q10iu8hvt.apps.googleusercontent.com"
	org-gcal-client-secret "E1darhWMBqZRQsc6kLZLebwk"
	org-gcal-file-alist '(("valentinifelix@gmail.com" .  "~/Sync/orgfiles/gcal.org")))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

;; highlighted syntax while exporting code in LaTeX, org
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-minted-options
       '(("frame" "lines")
;;      ("fontsize" "\\scriptsize")
         ("linenos" "")))
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; new line mode at border of the screen visual-line-mode
(setq line-move-visual nil)

;; yasnippet
(use-package yasnippet
      :ensure t
      :init
      (yas-global-mode 1))

;; Spotify on emcas using counsel
(use-package counsel-spotify
  :ensure t
  :config
(require 'counsel-spotify))

(setq counsel-spotify-client-id "5856cf6481b0469aa4150a146e2e10eb")
(setq counsel-spotify-client-secret "977ca32c81ad4b9cb01843adfde930d7")

;; latex
(use-package tex
  :ensure auctex)

  (defun tex-view ()
        (interactive)
        (tex-send-command "evince" (tex-append tex-print-file ".pdf")))

;; babel stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (shell . t)
   (C . t)
   (js . t)
   (ditaa . t)
   (dot . t)
   (org . t)
   (latex . t )
     ))
  
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config))

(show-paren-mode t)

;; font scaling
(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))

(define-key ctl-x-map [(control ?0)] 'zoom-in/out)

;; Keyfreq
(use-package keyfreq
  :ensure t
  :config
  (require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

;; beacon mode
(beacon-mode 1)

;; macro-math key binding to caluculate marked equations
(global-set-key "\C-x=" 'macro-math-eval-region)

;;; .emacs ends here
