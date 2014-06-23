;;; package -- init.el

;;; Commentary:

;;; Code:

;; cask
(require 'cask "~/.emacs.d/.cask/cask.el")
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
(tool-bar-mode 0)
(global-auto-revert-mode 1)
(exec-path-from-shell-initialize)
(custom-set-variables
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 )
(setq default-directory "~/")
(electric-pair-mode 1)

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

;; helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-mini)

;; magit
(require 'magit)

;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

;; flymake
(require 'flymake)
(set-face-background 'flymake-errline "red4")
(set-face-foreground 'flymake-errline "black")
(set-face-background 'flymake-warnline "yellow")
(set-face-foreground 'flymake-warnline "black")

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
(require 'yasnippet)
;; (require 'yasnippet-bundle)
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
(autoload 'cperl-mode "cperl-mode" nil t)
(add-to-list 'auto-mode-alist '("\.\([pP][Llm]\|al\|t\)\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h f") 'cperl-perldoc)))

(add-hook 'cperl-mode-hook
          '(lambda ()
             (cperl-set-style "PerlStyle")
             (custom-set-variables
              '(cperl-close-paren-offset -4)
              '(cperl-continued-statement-offset 4)
              '(cperl-indent-level 4)
              '(cperl-indent-parens-as-block t)
              '(cperl-tab-always-indent t)
              )))

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
(autoload 'php-mode "php-mode" nil t)
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
(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

;; python
(autoload 'python-mode "python-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; javascript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; coffeescript
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
  '(lambda()
     ("coffee-mode-hook"
      (set (make-local-variable 'tab-width) 2)
      (setq coffee-tab-width 2))))

;; css
(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; less
(autoload 'less-css-mode "less-css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; yaml
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; markdown
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(provide 'init)
;;; init.el ends here
