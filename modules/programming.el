;; Basic programming mode settings
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)
                            (display-fill-column-indicator-mode 1)
                            (display-line-numbers-mode 1)))
