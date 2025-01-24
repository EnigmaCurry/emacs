(load-theme 'modus-vivendi) ;; modus themes are builtin to Emacs.

(set-face-attribute 'default nil :height 150) ;; font size

;; Turn off distractions
(menu-bar-mode -1)  ;; Press F10 to bring up the menu if you still need it
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
