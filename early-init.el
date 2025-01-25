;; Load modus-vivendi, one of the dark-mode themes builtin to Emacs.
(load-theme 'modus-vivendi)
;; Set a larger font than the default:
(set-face-attribute 'default nil :height 150)
;; Turn off GUI distractions:
;; Press F10 to bring up the menu if you still need it.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
;; Don't resize the frame when adjusting the font size:
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
