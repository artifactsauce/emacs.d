;;; package -- init.el

;;; Commentary:

;;; Code:

;; cask
(cond
 ((string-equal system-type "gnu/linux") ; Linux
  (require 'cask "~/.cask/cask.el"))
 ((string-equal system-type "darwin") ; Mac OS X
  (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el"))
 )
(cask-initialize)

;; global setting
(set-language-environment 'Japanese)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq require-final-newline t)
(setq completion-ignore-case t)
(global-auto-revert-mode 1)
(exec-path-from-shell-initialize)
(custom-set-variables
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 )
(setq default-directory "~/")
(electric-pair-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; load for own environment
(if window-system
    (load "~/.emacs.d/env/window.el")
  (load "~/.emacs.d/env/terminal.el")
  :)

(cond
 ((string-match "24.5." emacs-version)
  (load "~/.emacs.d/v24-5.el"))
 ((string-match "24.4." emacs-version)
  (load "~/.emacs.d/v24-4.el"))
 ((string-match "24.3." emacs-version)
  (load "~/.emacs.d/v24-3.el"))
 )

; Clipboard copy and paste
(when window-system
  (global-set-key "\C-w" 'clipboard-kill-region)
  (global-set-key "\M-w" 'clipboard-kill-ring-save)
  (global-set-key "\C-y" 'clipboard-yank)
  )
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

; Font setting
(when window-system
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 120)
  (set-fontset-font
   nil 'japanese-jisx0208
   (font-spec :family "Migu 1M"))
  (setq face-font-rescale-alist
        '((".*Migu_1M.*" . 1.2)))
  (load-theme 'solarized-dark t)
  )

;; magit
(global-set-key (kbd "C-x v s") 'magit-status)
(global-set-key (kbd "C-x v l") 'magit-log)

;; helm
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-mini)

;; auto-complete
(global-auto-complete-mode t)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
(yas-global-mode 1)
(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

;; perl
(add-to-list 'auto-mode-alist '("\.\([pP][Llm]\|al\|t\)\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq tab-width nil)
               (require 'auto-complete)
               (require 'perl-completion)
               (add-to-list 'ac-sources 'ac-source-perl-completion)
               (perl-completion-mode t)
               )))

;; php
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.ctp$" . php-mode))
(add-hook 'php-mode-hook
          '(lambda()
             (setq php-mode-force-pear t)
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (setq c-basic-offset 4)
             (c-set-offset 'case-label' 4)
             (c-set-offset 'arglist-intro' 4)
             (c-set-offset 'arglist-cont-nonempty' 4)
             (c-set-offset 'arglist-close' 0)
             ))

;; ruby
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; python
(autoload 'python-mode "python-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(setq c-basic-offset 2
      js2-basic-offset 2
      indent-tabs-mode nil)

;; coffeescript
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
  '(lambda()
     ("coffee-mode-hook"
      (set (make-local-variable 'tab-width) 2)
      (setq coffee-tab-width 2))))

;; css
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(setq css-indent-offset 4)

;; scss
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(setq sass-indent-offset 4)

;; less
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; markdown
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(provide 'init)
;;; init.el ends here
