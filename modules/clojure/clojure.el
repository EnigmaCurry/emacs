(use-package clojure-mode
  :general
  (:keymaps 'clojure-mode-map
            "C-c SPC" #'clojure-align
            "C-c h" #'cider-doc)
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
  (use-package cider
    :config
    (tooltip-mode -1)
    (add-hook 'cider-connected-hook
              (lambda ()
                (tooltip-mode -1))))
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
