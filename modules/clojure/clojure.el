(use-package clojure-mode
  :hook
  ((clojure-mode . subword-mode))
  :init
  (use-package cider)
  (defun my/cljfmt-buffer ()
    (interactive)
    (when buffer-file-name
      (shell-command (format "cljfmt fix %s"
                             (shell-quote-argument buffer-file-name)))
      (revert-buffer :ignore-auto :noconfirm)))
  (use-package parinfer-rust-mode))
