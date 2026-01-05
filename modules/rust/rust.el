;; Rust
;; must manually install cargo-watch, wasm-pack, cargo-generate
;(my/cargo-dependency "cargo-watch")
;(my/cargo-dependency "wasm-pack")
;(my/cargo-dependency "cargo-generate")
(use-package
  rustic
  :mode ("\\.rs\\'" . rustic-mode)
  ;; :hook (rustic-mode . yas-minor-mode)
  :init
  (setq rustic-format-on-save t)
  (setq rustic-rustfmt-args "--edition 2024")
  (add-to-list 'exec-path "~/.cargo/bin")
  ;(defalias 'org-babel-execute:rust 'org-babel-execute:rustic)
  ;(describe-function 'org-babel-execute:rust)
  (add-hook
   'rustic-mode-hook
   (lambda ()
     (define-key
      rustic-mode-map
      (kbd "C-c M-.")
      'lsp-rust-analyzer-open-external-docs))))
