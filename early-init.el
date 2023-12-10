;; early-init.el is loaded *before* the package system and GUI are initialized:

;; Set a default theme - but this will be overridden in init.el by doom theme:
(load-theme 'modus-vivendi t)

;; Set default font size for my different machines based on hashed hostname:
(pcase
    (substring (base64-encode-string (secure-hash 'sha256 (system-name))) 0 10)
  ("ZWMzMTY4Mm" (set-face-attribute 'default nil :height 113))
  ("YzRlMTRjNT" (set-face-attribute 'default nil :height 181))
  (_ (set-face-attribute 'default nil :height 100)))

;; remove distractions:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)
