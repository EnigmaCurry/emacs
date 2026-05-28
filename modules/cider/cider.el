(use-package cider
  :config
  (tooltip-mode -1)
  (add-hook 'cider-connected-hook
            (lambda ()
              (tooltip-mode -1))))
