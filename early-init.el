;; early-init.el is loaded *before* the package system and GUI are initialized:

;; define GUI defaults early:
(load-theme 'deeper-blue t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)
