;;; init.el --- Emacs initialization.

;; Load config files.
(let ((config-directory (concat user-emacs-directory
                                (convert-standard-filename "config/"))))
  (add-to-list 'load-path config-directory))

;; Disable bell
(setq ring-bell-function 'ignore)

;; Move all temp out of .emacs.d directory.
(let ((backup-directory (concat user-emacs-directory
                                (convert-standard-filename "backups/"))))
  (setq auto-save-file-name-transforms `((".*" ,backup-directory t)))
  (setq backup-directory-alist `((".*" . ,backup-directory)))
  (setq auto-save-list-file-prefix backup-directory))
(setq create-lockfiles nil)

;; Disable home screen
(setq inhibit-startup-screen t)

;; Wrap long lines
(column-number-mode)
(setq fill-column 78)
(defvar visual-wrap-column 78)

;; Configure package manager.
(require 'use-package-config)

;; Set theme
(require 'theme-config)

;; Install basic plugins
(use-package better-defaults)

(use-package recentf
  :custom
  (recentf-auto-cleanup 'never)
  :init
  (recentf-mode 1))

(use-package ido
  :init
  (ido-mode t)
  (ido-everywhere t))

(use-package ido-completing-read+
  :after ido
  :init
  (ido-ubiquitous-mode t))

(use-package undo-fu)

(use-package evil
  :custom
  (evil-undo-system 'undo-fu)
  :init
  (evil-mode t))

(use-package smex
  :bind  (("M-x" . smex)
          ("M-X" . smex-major-mode-commands)
          ("C-c C-c M-x" . execute-extended-command)))

(use-package which-key
  :init
  (which-key-mode))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '("~/Projects/"))
  )
(use-package company
  :init
  (global-company-mode))

(require 'lsp-config)
(require 'git-config)

;; Install common major modes.
(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t))
(use-package csv-mode)
(use-package yaml-mode)
(use-package json-mode)
