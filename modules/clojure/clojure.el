(use-package clojure-mode
  :general
  ("C-c SPC" #'clojure-align)
  ("C-c M-SPC" #'my/cljfmt-file)
  :hook
  (clojure-mode . my/clojure-mode-setup)
  :init
  (use-package cider)
  (use-package parinfer-rust-mode)
  (defun my/clojure-format-file ()
    "Format the current Clojure file with cljfmt."
    (interactive)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (call-process "cljfmt" nil "*cljfmt*" nil "fix" file)
        (revert-buffer :ignore-auto :noconfirm))))
  (defun my/clojure-mode-setup ()
    "My Clojure editing defaults."
    (subword-mode 1)
    (parinfer-rust-mode 1)
    (electric-pair-local-mode -1)
    (add-hook 'after-save-hook #'my/clojure-format-file nil t)))
