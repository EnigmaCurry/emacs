;; Use Modus themes that are builtin to Emacs:
;; Set variable pitch mode for modus themes:
(setq modus-themes-mixed-fonts t) ; allow variable-pitch-mode for
                                  ; org-mode but not for code blocks
(add-hook 'text-mode-hook #'variable-pitch-mode)
;; Load modus-vivendi which is a builtin dark mode theme:
(load-theme 'modus-vivendi)

;; Set a larger default font size (150 = 15 pt size):
(set-face-attribute 'default nil :height 150)
;; Turn off GUI distractions:
(menu-bar-mode -1) ; Press F10 to bring up the menu if you still need it.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
;; Don't resize the frame when adjusting the font size:
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Debug options:
;;; Start Emacs with the `--debug-init` argument, to debug errors during startup.
;;; Enter debugger on specific logger regex (see *Messages* buffer):
;; (setq debug-on-message "Example log message to trace")
;;; M-x toggle-debug-on-error
(setq debug-on-error t)
