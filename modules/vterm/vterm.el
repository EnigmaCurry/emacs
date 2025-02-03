;; vterm (terminal emulator) ::
;;  https://github.com/akermu/emacs-libvterm
;; Configure BASH to work with vterm:
;;  https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
(use-package
  vterm
  :custom (vterm-always-compile-module t)
  :general ("C-c t" 'my/vterm-toggle)
  :config (define-key vterm-mode-map (kbd "<f5>") nil)
  :hook ((vterm-mode . (lambda () (setq-local show-trailing-whitespace nil))))
  :init
  ;; shell-pop for vterm :: https://github.com/jixiuf/vterm-toggle
  (use-package vterm-toggle)
  (defun my/vterm-toggle (&optional args)
    "Customized vterm-toggle wrapper- this fixes the universal
    argument (C-u) to always create a new terminal"
    (interactive "P")
    (if (not
         (or (derived-mode-p 'vterm-mode)
             (and (vterm-toggle--get-window)
                  vterm-toggle-hide-method)))
        (if (equal current-prefix-arg '(4))
            (vterm-toggle--new args)
          (vterm-toggle args))
      (vterm-toggle args))))
