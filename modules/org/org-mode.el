(add-hook 'org-mode-hook 'flyspell-mode)

(defun my/emacs-org-tangle ()
"Tangle all code blocks in 'emacs.org' and export this document to HTML."
(use-package htmlize
  :ensure t)
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
(start-process "live-server" "*my/emacs-org-html-server*" "live-server"
	   "-H" my/emacs-org-html-server-host "-p"
	   my/emacs-org-html-server-port "-o" "emacs.html"
	   (expand-file-name "export" user-emacs-directory))

(require 'org-tempo) ; required for Structure Templates
                     ; See https://orgmode.org/manual/Structure-Templates.html

(add-hook 'org-mode-hook (lambda ()
                           (modify-syntax-entry ?/ "$/" org-mode-syntax-table)
                           (modify-syntax-entry ?~ "$~" org-mode-syntax-table)
                           (modify-syntax-entry ?= "$=" org-mode-syntax-table)
                           ))
