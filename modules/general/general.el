(use-package
  general
  :init
  ;; Switch between two most recent buffers:
  (fset 'quick-switch-buffer [?\C-x ?b return])
  :config
   ;;; Custom global bindings:
  (general-define-key
   "C-h B"
   'general-describe-keybindings
   "s-b"
   'quick-switch-buffer
   "s-B"
   'buffer-menu-other-window
   "C-x B"
   'buffer-menu-other-window
   "s-o"
   'browse-url
   "C-;"
   'comment-region ; C-u C-; to uncomment
   "s-<down-mouse-1>"
   'mouse-drag-region-rectangle)
  ;;; Put the Emacs default keybindings you want included in general-describe-keybindings here:
  ;;; Its useful to duplicate these simply as a way of documentation:
  (general-define-key
   "M-SPC"
   'cycle-spacing ; If you document it, you will use it.
   "M-h"
   'mark-paragraph ; C-h B is like your personal cheat sheet.
   "C-h b"
   'describe-bindings ;; default binding for documentation purpose
   "C-x 4 c"
   'clone-indirect-buffer-other-window ;; default binding
   )
  ;;; Define bindings for specific builtin (non use-package) modes:
  ;; Emacs Lisp mode bindings:
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   "s-e" 'eval-defun ;eval top-level form
   "M-;" 'paredit-comment-dwim)
  ;; Dired mode bindings:
  (general-define-key
   :keymaps 'dired-mode-map "C-c C-q" 'dired-toggle-read-only))
