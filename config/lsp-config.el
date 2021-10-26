(require 'use-package-config)
(use-package lsp-mode
  :hook ((python-mode . lsp-deferred)
         (c-mode-common . lsp-deferred)
         (verilog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-vhdl-server 'hdl-checker))

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
  (add-to-list 'lsp-disabled-clients 'pyls))

(use-package glsl-mode
  :after lsp-mode
  :init
  (add-to-list 'lsp-language-id-configuration '(glsl-mode . "glsl"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("glslls" "--verbose" "--stdin"))
                    :activation-fn (lsp-activate-on "glsl")
                    :server-id 'glslls))
  )
(use-package company-glsl
  :after company
  :init
  (add-to-list 'company-backends 'company-glsl))

(use-package dap-mode)

(provide 'lsp-config)
