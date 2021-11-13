(require 'lsp-config)
(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  )
(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
  :hook
  (rjsx-mode . lsp-deferred)
  )
(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  :hook
  (typescript-mode . lsp-deferred)
  )
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
  :hook ((web-mode . company-mode)
         (web-mode . lsp-deferred)
         (web-mode . prettier-js-mode)))
(use-package tide
  :hook ((before-save . tide-format-before-save)
         (web-mode . (lambda() (tide-setup) (eldoc-mode)))
         (typescript-mode . (lambda() (tide-setup) (eldoc-mode)))
         (rjsx-mode . (lambda() (tide-setup) (eldoc-mode)))
  ))
(use-package prettier-js)
(use-package nvm)
(provide 'web-config)
