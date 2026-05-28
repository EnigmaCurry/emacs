(use-package clojure-mode
  :general
  (:keymaps 'clojure-mode-map
            "C-c SPC" #'clojure-align
            "C-c h" #'my/cider-doc-no-select
            "C-c m" #'my/toggle-clojure-structure-mode)
  :hook
  (clojure-mode . my/clojure-mode-setup)
  :init
  (add-to-list
   'display-buffer-alist
   '("\\*cider-doc\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)
     (window-height . 0.30)))
  (defun my/toggle-clojure-structure-mode ()
    "Toggle between parinfer-rust-mode and lispy-mode in Clojure buffers."
    (interactive)
    (cond
     ;; If Parinfer is active, switch to Lispy.
     ((bound-and-true-p parinfer-rust-mode)
      (parinfer-rust-mode -1)
      (lispy-mode 1)
      (message "Clojure structure mode: lispy"))
     ;; If Lispy is active, switch to Parinfer.
     ((bound-and-true-p lispy-mode)
      (lispy-mode -1)
      (parinfer-rust-mode 1)
      (message "Clojure structure mode: parinfer-rust"))
     ;; If neither is active, default to Parinfer.
     (t
      (parinfer-rust-mode 1)
      (message "Clojure structure mode: parinfer-rust"))))
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
    (add-hook 'after-save-hook #'my/clojure-format-file nil t))
  (defun my/cider-doc-no-select ()
    "Show CIDER docs without leaving the current window selected."
    (interactive)
    (let ((win (selected-window)))
      (call-interactively #'cider-doc)
      (select-window win))))
