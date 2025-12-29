(use-package rg
  :straight t
  :config
  ;(rg-enable-default-bindings)
  ;(rg-enable-menu)
  :general
  ("C-c f" (lambda () (interactive) (rg-menu))))
