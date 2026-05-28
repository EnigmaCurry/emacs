(use-package clojure-mode
  :hook
  ((clojure-mode . subword-mode)
   (clojure-mode . parinfer-rust-mode)
   (clojure-mode . (lambda ()
                   (electric-pair-local-mode -1))))
  :init
  (use-package cider)
  (defun my/cljfmt-buffer ()
    (interactive)
    (when buffer-file-name
      (shell-command (format "cljfmt fix %s"
                             (shell-quote-argument buffer-file-name)))
      (revert-buffer :ignore-auto :noconfirm)))
  (use-package parinfer-rust-mode))
