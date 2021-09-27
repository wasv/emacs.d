(require 'use-package-config)

(use-package magit)
(use-package diff-hl
  :after magit
  :init
  (global-diff-hl-mode)
  :hook
  (('magit-pre-refresh . 'diff-hl-magit-pre-refresh)
   ('magit-post-refresh . 'diff-hl-magit-post-refresh))
  )

(provide 'git-config)
