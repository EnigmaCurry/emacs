;; Rust
;; must manually install cargo-watch, wasm-pack, wasm-bindgen, cargo-generate
(my/cargo-dependency "cargo-watch")
(my/cargo-dependency "wasm-pack")
(my/cargo-dependency "wasm-bindgen")
(my/cargo-dependency "cargo-generate")
(use-package
  rustic
  :mode ("\\.rs\\'" . rustic-mode)
  ;; :hook (rustic-mode . yas-minor-mode)
  :init
  (setq rustic-format-on-save t)
  (setq rustic-rustfmt-args "--edition 2021")
  (add-to-list 'exec-path "~/.cargo/bin")
  (add-hook
   'rustic-mode-hook
   (lambda ()
     (define-key
      rustic-mode-map
      (kbd "C-c M-.")
      'lsp-rust-analyzer-open-external-docs))))
