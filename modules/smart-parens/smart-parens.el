(use-package smartparens
  :hook (clojure-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  :general
  (:keymaps 'smartparens-mode-map
            "M-l" #'sp-forward-sexp
            "M-h" #'sp-backward-sexp
            "M-k" #'sp-up-sexp
            "M-j" #'sp-down-sexp
            "M-e" #'sp-end-of-sexp
            "M-a" #'sp-beginning-of-sexp))
