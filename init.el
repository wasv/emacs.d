;;; init.el --- Initial emacs config.
;;; Commentary:
;;; Code:

;;; Disable menubar in console
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(defun require-package (package)
  "Install given PACKAGE."
  (setq-default highlight-tabs t)
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;; Package List
(mapc 'require-package '(
  org
  evil
  flycheck
  helm
  magit
  auto-complete
  diff-hl

  markdown-mode
  python-mode
  racket-mode

  org-bullets

  evil-org
  evil-leader

  dtrt-indent
  smart-tabs-mode
  csv-mode
  ))

;;; evil
(substitute-key-definition 'kill-buffer 'kill-buffer-and-its-windows global-map)
(require 'evil)
(require 'evil-leader)
(evil-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "ff" 'find-file
  "fr" 'helm-recentf

  "bs" 'switch-to-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bk" 'kill-buffer

  "ww" 'evil-window-next
  "wv" 'split-window-vertically
  "wh" 'split-window-horizontally
  "ws" 'split-window-horizontally
  "wk" 'delete-window
  )

;;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

;;; Autocomplete
(ac-config-default)

;;; diff-hl
(diff-hl-mode 1)

;;; org
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; recentf
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;; Custom Pandoc functions
(defun pandoc-blank-to-blank (from-fmt to-fmt out-name)
  "Convert the current buffer from FROM-FMT to TO-FMT using Pandoc.  Output is saved in a file called OUT-NAME."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (format "pandoc -f %s -t %s -o %s" from-fmt to-fmt out-name)
   ))

(defun pandoc-blank-to-pdf (from-fmt out-name)
  "Convert the current buffer from FROM-FMT to pdf using Pandoc.  Output is saved in a file called OUT-NAME."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (format "pandoc -f %s -o %s" from-fmt out-name)
   ))

(defun pandoc-org-to-pdf ()
  "Convert the current buffer from org to a pdf using Pandoc."
  (interactive)
  (pandoc-blank-to-pdf "org" (concat buffer-file-name ".pdf")))

(defun pandoc-md-to-pdf ()
  "Convert the current buffer from md to a pdf using Pandoc."
  (interactive)
  (pandoc-blank-to-pdf "markdown" (concat buffer-file-name ".pdf")))

(defun pandoc-tex-to-pdf ()
  "Convert the current buffer from tex to a pdf using Pandoc."
  (interactive)
  (pandoc-blank-to-pdf "latex" (concat buffer-file-name ".pdf")))

(defun pandoc-md-to-tex ()
  "Convert the current buffer from md to tex using Pandoc."
  (interactive)
  (pandoc-blank-to-blank "markdown" "latex" (concat buffer-file-name ".tex")))

;;; Hooks
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'text-mode-hook (lambda()
			    (setq-default indent-tabs-mode nil)
			    (setq-default tab-width 4)
			    (setq indent-line-function 'insert-tab)))
(add-hook 'c-mode-common-hook
	  (lambda()
	    (require 'dtrt-indent)
	    (dtrt-indent-mode t)))

;;; Autoconfig Stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Dropbox/Schedule")))
 '(org-agenda-regexp-filter (quote ("~/Dropbox/Schedule/*.org")) t)
 '(org-agenda-start-on-weekday 0)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
