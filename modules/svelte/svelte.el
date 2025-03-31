(use-package svelte-mode
  :straight t
  :mode ("\\.svelte\\'" . svelte-mode)
  :init
  (use-package lsp-mode
    :straight t)
  (use-package apheleia
    :straight t)
  :hook ((svelte-mode . lsp))  ; Start LSP when opening a Svelte file
  :config
  (setq lsp-enable-snippet t)
  
  ;; Configure Apheleia to format Svelte files using Prettier v3.
  ;; This command passes the current file path and forces the "svelte" parser.
  (setf (alist-get 'svelte-mode apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--parser" "svelte"))
  (setf (alist-get 'svelte-mode apheleia-mode-alist) 'svelte-mode)
  
  (apheleia-global-mode +1))
