;;; init.el --- Emacs initialization.

;; Load config files.
(let ((config-directory (concat user-emacs-directory
                                (convert-standard-filename "config/"))))
  (add-to-list 'load-path config-directory))

;; Configure package manager.
(require 'use-package-config)

;; Install basic plugins
(use-package better-defaults)

(use-package evil
  :config
  (require 'evil)
  (evil-mode t))
