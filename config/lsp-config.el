(require 'use-package-config)
(use-package lsp-mode
  :hook ((python-mode . lsp-deferred)
         (c-mode-common . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom (lsp-keymap-prefix "C-c l"))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :init (lsp-treemacs-sync-mode t)
  :commands lsp-treemacs-errors-list)

;; Extra modes
(use-package cmake-mode)

(use-package lsp-jedi
  :after lsp-mode
  :init
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi))

;; optionally if you want to use debugger
(use-package dap-mode)

(provide 'lsp-config)
