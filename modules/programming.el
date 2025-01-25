;; Basic programming mode settings
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
