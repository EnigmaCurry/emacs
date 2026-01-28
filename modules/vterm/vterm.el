;; vterm (terminal emulator) ::
;;  https://github.com/akermu/emacs-libvterm
;; Configure BASH to work with vterm:
;;  https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
(use-package
  vterm
  :custom (vterm-always-compile-module t)
  :general ("C-c t" 'my/vterm-toggle)
           ("C-c M-t" 'my/vterm-buffer-menu)
  :config (define-key vterm-mode-map (kbd "<f5>") nil)
  :hook ((vterm-mode . (lambda () (setq-local show-trailing-whitespace nil))))
  :init
  ;; shell-pop for vterm :: https://github.com/jixiuf/vterm-toggle
  (use-package vterm-toggle)
  (defun my/vterm-toggle (arg)
    "Toggle or create vterm buffers with prefix semantics.

No prefix: behave like `vterm-toggle' (reuse most-recent vterm or create one).

C-u:       always create a NEW vterm buffer (local or remote context
follows the current buffer’s TRAMP-ness).

C-u C-u:   always create a NEW vterm buffer on the LOCAL host,
regardless of the current buffer’s TRAMP context."
    (interactive "P")
    (cond
     ;; --- Case 3: two or more universal prefixes -> force LOCAL new terminal
     ((and arg (>= (prefix-numeric-value arg) 16))
      (let ((default-directory (expand-file-name "~"))
            (name (generate-new-buffer-name vterm-buffer-name)))
        (vterm-toggle--new name)))
     
     ;; --- Case 2: single C-u -> new terminal following current TRAMP context
     ((equal arg '(4))
      (let* ((name (generate-new-buffer-name vterm-buffer-name)))
        (vterm-toggle--new name)))
     
     ;; --- Case 1: no (or other) prefix -> defer to vterm-toggle's default behavior
     (t
      (vterm-toggle arg))))
  (defvar consult--source-vterm
    `(:name "Vterm"
      :narrow ?t
      :category buffer
      :face consult-buffer
      :state ,#'consult--buffer-state
      :items ,(lambda ()
                (mapcar #'buffer-name
                        (seq-filter (lambda (buf)
                                      (eq (buffer-local-value 'major-mode buf) 'vterm-mode))
                                    (buffer-list)))))
    "Vterm buffer source for `consult-buffer'.")
  (defun consult-vterm-buffer ()
    "Switch to a vterm buffer using consult."
    (interactive)
    (consult-buffer '(consult--source-vterm)))
  (defun my/vterm-buffer-menu ()
    "Display a buffer menu with only vterm buffers."
    (interactive)
    (let ((vterm-buffers (seq-filter (lambda (buf)
                                        (eq (buffer-local-value 'major-mode buf) 'vterm-mode))
                                      (buffer-list))))
      (display-buffer (list-buffers-noselect nil vterm-buffers))))
  )
