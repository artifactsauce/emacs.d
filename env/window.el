(tool-bar-mode 0)

; Font setting
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Migu 1M"))
(setq face-font-rescale-alist
      '((".*Migu_1M.*" . 1.2)))
(load-theme 'solarized-dark t)
