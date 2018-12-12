;;; init.el --- Initial emacs config.
;;; Commentary:
;;; Code:

(set-frame-font "Source Code Pro-10" nil t)
(setq-default major-mode 'text-mode)

;;; Disable bleeping beep.
(setq ring-bell-function 'ignore)

;;; Disable menubar in console
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;;; Wrap long lines
(setq fill-column 78)
(defvar visual-wrap-column 78)

;;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(defun require-package (package)
  "Install given PACKAGE."
  (setq-default highlight-tabs t)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;;; Package List
(mapc 'require-package '(
  org
  evil
  flycheck
  helm

  company
  company-irony

  neotree
  helm-projectile

  rust-mode
  flycheck-rust
  markdown-mode
  matlab-mode
  csv-mode
  yaml-mode

  org-bullets

  evil-org
  evil-leader
  evil-numbers

  dtrt-indent
  smart-tabs-mode
  exec-path-from-shell
  ))

;;; Include custom PATH directories.
(exec-path-from-shell-copy-env "PATH")

;;; evil
(substitute-key-definition 'kill-buffer 'kill-buffer-and-its-windows global-map)
(setq evil-want-C-i-jump nil)
(require 'evil)
(require 'evil-leader)
(require 'evil-numbers)
(evil-mode 1)
(global-evil-leader-mode)
(when evil-want-C-i-jump
  define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "ff" 'find-file
  "fr" 'helm-recentf
  "ft" 'neotree-toggle
  "fp" 'projectile-switch-project

  "bs" 'helm-buffers-list
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bk" 'kill-buffer

  "ni" 'evil-numbers/inc-at-pt
  "nd" 'evil-numbers/dec-at-pt

  "ww" 'evil-window-next
  "wv" 'split-window-vertically
  "wh" 'split-window-horizontally
  "ws" 'split-window-horizontally
  "wk" 'delete-window

  "ot" 'org-timeline
  "oa" 'org-agenda-list

  "cp" 'projectile-command-map
  "cw" 'whitespace-cleanup
  "cf" 'fill-paragraph
  "ci" 'indent-buffer
  )

;;; NeoTree + Projectile
(require 'neotree)
(require 'projectile)

(projectile-mode)
(setq neo-smart-open t)

(setq projectile-project-run-cmd "make run")
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq projectile-enable-caching t)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)))
      (message "Could not find git project root."))))

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)


;;; company
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(defun complete-or-indent ()
  "Complete at point using company or indent."
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

;;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(require 'helm-projectile)
(helm-projectile-on)

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

(defun rubber-tex-to-pdf ()
  "Convert the current buffer from tex to a pdf using rubber."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (format "rubber -d %s" buffer-file-name)
   ))

(defun pdflatex-tex-to-pdf ()
  "Convert the current buffer from tex to pdf using pdflatex."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (format "pdflatex %s && pdflatex %s" buffer-file-name buffer-file-name)
   ))

(defun pandoc-md-to-tex ()
  "Convert the current buffer from md to tex using Pandoc."
  (interactive)
  (pandoc-blank-to-blank "markdown" "latex" (concat buffer-file-name ".tex")))

;;; C-Style
(c-add-style "user"
	     '("gnu"
	       (c-indent-tabs-mode . nil)
	       (c-indent-level . 4)
	       (c-basic-offset . 4)
	       (c-offsets-alist
		(defun-block-intro . 4)
		(substatement-open . 0)
		(statement-block-intro . 4)
		)))

;;; Backup file settings
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backup/" t)))
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7 2))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))


;;; Auto Indent Function
(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

;;; Hooks
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook (lambda()
			    (setq indent-tabs-mode nil
				  tab-width 4
				  tab-always-indent
                                    (default-value 'tab-always-indent))
			    ))
(add-hook 'c-mode-common-hook
	  (lambda()
	    (c-set-style "user")
	    (setq-default
	     tab-width 4
	     indent-tabs-mode nil
	     )
	    ))
(add-hook 'asm-mode-hook (lambda()
                           (setq tab-always-indent
                                 (default-value 'tab-always-indent))
                           (setq tab-width 4)
			   (setq indent-tabs-mode nil)
                           (setq indent-line-function 'insert-tab)
                           ))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;; Autoconfig Stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(inhibit-startup-screen t)
 '(org-agenda-files "~/.emacs.d/agenda-files")
 '(org-agenda-regexp-filter (quote ("*.org")) t)
 '(org-agenda-start-on-weekday 0)
 '(org-todo-keywords (quote ((sequence "TODO" "STRT" "DONE"))))
 '(safe-local-variable-values (quote ((eval flycheckmode 0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
