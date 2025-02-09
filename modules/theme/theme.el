(defgroup my/theme-settings nil
  "My custom theme settings"
  :group 'my/custom-settings)
(defcustom my/theme 'deeper-blue
  "Emacs Theme"
  :type 'symbol
  :group 'my/theme-settings)

(defun my/theme-update (theme-fn)
  "Update the `my/theme` variable with the new theme and call THEME-FN."
  (let ((current-theme (car custom-enabled-themes)))
    (funcall theme-fn)
    (customize-set-variable 'my/theme (car custom-enabled-themes))
    (customize-save-customized)
    (message "Theme changed to: %s" my/theme)))

;; Install themes directly from a git repository:
;;;NOTE: don't use deep-thought-theme it crashes Emacs 29.4!!
;;;Keeping this here as an example for loading a theme from git:
;; (use-package
;;   deep-thought-theme
;;   :straight
;;   (deep-thought-theme :type git :repo "https://github.com/emacsfodder/emacs-deep-thought-theme.git"))
(use-package solaire-mode :init (solaire-global-mode +1))
(use-package theme-looper
  :general
  ("C-<f11>" (lambda () (interactive) (my/theme-update 'theme-looper-enable-previous-theme)))
  ("C-<f12>"  (lambda () (interactive) (my/theme-update 'theme-looper-enable-next-theme))))

(load-theme my/theme t)
