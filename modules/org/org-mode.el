(use-package org
  :ensure nil
  :hook (org-mode . flyspell-mode)
  :general
  ("s-<up>" 'org-previous-visible-heading)
  ("s-<down>" 'org-next-visible-heading)
  ("C-c o k" 'org-babel-remove-result)
  :config
  (setq org-startup-folded t)
  )

(use-package htmlize
  :ensure t)

(defun my/emacs-org-tangle ()
"Tangle all code blocks in 'emacs.org' and export this document to HTML."
(let* ((org-file (expand-file-name "emacs.org" user-emacs-directory))
       (modules-dir (expand-file-name "modules" user-emacs-directory))
       (export-dir (expand-file-name "export" user-emacs-directory))
       (export-file (expand-file-name "emacs.html" export-dir)))
  (when (file-exists-p org-file)
    (with-current-buffer (find-file-noselect org-file)
      (delete-directory modules-dir t)
      (org-babel-tangle)
      (org-export-to-file 'html export-file)
      ;; No reason to save the buffer again, but maybe in the future,
      ;; we will want to run code blocks automatically and capture output?
                                        ;(save-buffer)
      ))))
;; Tell Emacs to trust this code in all buffer local vars:
(add-to-list 'safe-local-variable-values
             '(eval add-hook 'after-save-hook 'my/emacs-org-tangle nil t))
(add-to-list 'safe-local-variable-values
             '(org-confirm-babel-evaluate))

(defvar my/emacs-org-html-server-host "127.0.0.1") ;; Set to 0.0.0.0 to serve publicly
(defvar my/emacs-org-html-server-port "7776")
(defun my/emacs-org-html-server ()
  "Start a local live-server for org HTML exports. Installs live-server if not found.
   If cargo is not available, it notifies the user."
  (interactive)
  (let ((live-server-path (executable-find "live-server"))
        (cargo-path (executable-find "cargo")))
    (if live-server-path
        (progn
          (message "live-server found at: %s" live-server-path)
          (let ((host "127.0.0.1")  ;; Set to "0.0.0.0" to serve publicly
                (port "7776")
                (html-file "emacs.html")
                (export-dir (expand-file-name "export" user-emacs-directory)))
            (start-process "live-server" "*my/emacs-org-html-server*" "live-server"
                           "-H" host "-p" port "-o" html-file export-dir)
            (message "Started live-server on http://%s:%s" host port)))
      (if cargo-path
          (progn
            (message "live-server not found, installing via cargo...")
            (start-process-shell-command "cargo-install-live-server" "*cargo-install-output*" "cargo install live-server")
            (message "Installation started. Please rerun `M-x my/emacs-org-html-server` once installation completes."))
        (message "Neither live-server nor cargo were found. Please install Rust and Cargo first.")))))

(require 'org-tempo) ; required for Structure Templates
                     ; See https://orgmode.org/manual/Structure-Templates.html

(add-hook 'org-mode-hook (lambda ()
                           (modify-syntax-entry ?/ "$/" org-mode-syntax-table)
                           (modify-syntax-entry ?~ "$~" org-mode-syntax-table)
                           (modify-syntax-entry ?= "$=" org-mode-syntax-table)
                           ))
