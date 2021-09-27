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
  (setq auto-save-list-file-prefix backup-directory)
  )

;; Disable home screen
(setq inhibit-startup-screen t)

;; Wrap long lines
(column-number-mode)
(setq fill-column 78)
(defvar visual-wrap-column 78)

;; Configure package manager.
(require 'use-package-config)

;; Install basic plugins
(use-package better-defaults)

(use-package evil
  :config
  (require 'evil)
  (evil-mode t))

(use-package smex
  :bind  (("M-x" . smex)
          ("M-X" . smex-major-mode-commands)
          ("C-c C-c M-x" . execute-extended-command)))

(use-package which-key)
