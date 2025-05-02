;; Basic programming mode settings
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)
                            (display-fill-column-indicator-mode 1)
                            (display-line-numbers-mode 1)))

;; Edit .env files with shell-script-mode
(add-to-list 'auto-mode-alist '("\\.env\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.env_.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.env-dist\\'" . shell-script-mode))
