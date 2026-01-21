(use-package lsp-treemacs :commands lsp-treemacs-errors-list
  :general
  ( "C-c l"   #'my/treemacs-open-or-focus
    "C-c M-l" #'my/treemacs-close)
  :init    
  (defun my/treemacs-open-or-focus ()
    "Open Treemacs if not visible; otherwise focus the Treemacs window."
    (interactive)
    (require 'treemacs)
    (if-let ((win (treemacs-get-local-window)))
        (select-window win)
      (treemacs))) ;; note: `treemacs` itself is a show/hide toggle :contentReference[oaicite:1]{index=1}
  (defun my/treemacs-close ()
    "Close (hide) the Treemacs window if it's visible."
    (interactive)
    (require 'treemacs)
    (when-let ((win (treemacs-get-local-window)))
      ;; Close the *Treemacs* window, not the current editing window:
      (delete-window win)))
  )
