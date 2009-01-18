(load-library "mmm-mako")
(setq auto-mode-alist (cons '("\\.mako$" . html-mode) auto-mode-alist))
(mmm-add-mode-ext-class 'html-mode "\\.mako$'" 'mako)
