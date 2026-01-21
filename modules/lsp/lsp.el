;; LSP mode :: https://emacs-lsp.github.io/lsp-mode/
(use-package
  lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-modeline-diagnostics-scope :workspace)
 ;;; extra verbose logging of lsp json messages:
  ;;(setq lsp-log-io t)
  :hook
  ((web-mode . lsp)
   ;(lsp-mode . lsp-enable-which-key-integration)
   (python-mode . lsp-deferred))
  :commands lsp
  :config)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; moved to treemacs module: (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package flycheck)

;; LSP debuggers
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
