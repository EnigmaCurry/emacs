(use-package svelte-mode
  :straight t
  :mode ("\\.svelte\\'" . svelte-mode)
  :init
  ;; Ensure lsp-mode and prettier-js are installed
  (use-package prettier-js
    :straight t)
  :hook ((svelte-mode . lsp)           ; Start LSP when opening a Svelte file
         (svelte-mode . prettier-js-mode)) ; Enable Prettier formatting
  :config
  ;; Optional additional settings (e.g., enable snippets in LSP)
  (setq lsp-enable-snippet t))
