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
