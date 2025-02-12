;; hydra (rapid fire mnemonic keybindings) :: https://github.com/abo-abo/hydra
(use-package hydra)
(use-package org
  :after hydra
  :hook ((org-mode . flyspell-mode)
         (org-mode . unpackaged/org-export-html-with-useful-ids-mode))
  :custom
  (org-html-validation-link nil)
  (org-html-use-infojs nil)
  (org-html-postamble 'auto)
  (org-export-with-author t)
  (org-export-with-date t)
  (org-export-with-creator nil)
  (org-export-with-email t)
  (org-export-timestamp-file t)
  (org-export-allow-bind-keywords t)
  (org-directory "~/Org")
  :general
  ("s-<up>" 'org-previous-visible-heading)
  ("s-<down>" 'org-next-visible-heading)
  ("C-c o k" 'org-babel-remove-result)
  :config
  (setq org-startup-folded t)
  (defun my-org-html--translate (original-function keyword info)
    "Custom advice to translate the keyword 'Created' to 'Last Modified'."
    (if (string-equal keyword "Created")
        "Last Modified"
      (funcall original-function keyword info)))
  (advice-add 'org-html--translate :around #'my-org-html--translate)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (scheme . t) (shell . t) (ditaa . t)))
  :init
  ;; Hydra for commonly used org commands:
  (defhydra
    hydra-org
    (global-map "C-c o" :exit t color pink :hint nil)
    "Org commands:"
    ("o" my/open-org-file)
    ("l" org-store-link "store link")
    ("i" org-insert-link "insert link")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("m" org-info "read info manual")
    ("e" org-export-dispatch "export")
    ("p" org-preview-html-mode "toggle preview mode")
    ("s" org-insert-source-code-block "insert source code block"))
  )
(require 'org-tempo) ; required for Structure Templates
                                        ; See https://orgmode.org/manual/Structure-Templates.html
(use-package htmlize) ; required for colorized HTML code blocks
(use-package org-preview-html :after org)
(use-package ob-async)
(progn ; electic pairs for org-mode
  (modify-syntax-entry ?/ "\"" org-mode-syntax-table)
  (modify-syntax-entry ?* "\"" org-mode-syntax-table)
  (modify-syntax-entry ?= "\"" org-mode-syntax-table)
  (modify-syntax-entry ?+ "\"" org-mode-syntax-table)
  (modify-syntax-entry ?_ "\"" org-mode-syntax-table)
  (modify-syntax-entry ?~ "\"" org-mode-syntax-table))

(defun my/emacs-org-tangle ()
  "Tangle all code blocks in 'emacs.org' and export this document to HTML."
  (let* ((org-file (expand-file-name "emacs.org" user-emacs-directory))
         (modules-dir (expand-file-name "modules" user-emacs-directory))
         (export-dir (expand-file-name "export" user-emacs-directory))
         (export-file (expand-file-name "index.html" user-emacs-directory)))
    (when (file-exists-p org-file)
      (with-current-buffer (find-file-noselect org-file)
        (delete-directory modules-dir t)
        (org-babel-tangle)
        (org-export-to-file 'html export-file)
        (unless (file-directory-p export-dir)
          (make-directory export-dir))
        (make-symbolic-link "index.html" "emacs.html" t)
        (make-symbolic-link "../index.html" "export/index.html" t)
        (make-symbolic-link "../favicon.ico" "export/favicon.ico" t)
        (make-symbolic-link "../index.html" "export/emacs.html" t)
        (make-symbolic-link "../modules" "export/modules" t)
        (make-symbolic-link "../theme" "export/theme" t)
        (make-symbolic-link "../LICENSE.txt" "export/LICENSE.txt" t)
        (make-symbolic-link "../LICENSE_GPLv3.txt" "export/LICENSE_GPLv3.txt" t)
        (make-symbolic-link "../early-init.el" "export/early-init.el" t)
        (make-symbolic-link "../init.el" "export/init.el" t)
        ;; No reason to save the buffer again, but maybe in the future,
        ;; we will want to run code blocks automatically and capture output?
        ;;(save-buffer)
        ))))
(with-eval-after-load 'ox-html
  (defun my/org-html-src-block (orig-fun src-block contents info)
    "Advice for `org-html-src-block' to add a header.
If a :tangle header is specified (and not \"no\"), it shows the tangle file.
If the block is a shell block, it prints 'Run in Bash shell:'.
Otherwise, it prints the code block's language.
ORIG-FUN is the original function; SRC-BLOCK is the source block;
INFO is the export options plist."
    (let* ((parameters (org-element-property :parameters src-block))
           (header-args (org-babel-parse-header-arguments parameters))
           (tangle (cdr (assoc :tangle header-args)))
           (lang (org-element-property :language src-block))
           (header (cond
                    ((and tangle (not (string= tangle "no")))
                     (format "<div class=\"code-block-header tangle\"><span class=\"org-parameter\">:tangle</span> <span class=\"filename\">%s</span></div>\n"
                             (org-html-encode-plain-text tangle)))
                    ((string= lang "shell")
                     "<div class=\"code-block-header shell\">Run this in your shell ::</div>\n")
                    ((string= lang "example")
                     "<div class=\"code-block-header example\">Example ::</div>\n")
                    (lang
                     (format "<div class=\"code-block-header lang\">(untangled) %s</div>\n"
                             (org-html-encode-plain-text lang)))
                    (t "")))
           (code (funcall orig-fun src-block contents info)))
      (if (not (string= header ""))
          (format "<div class=\"code-block-container\">%s%s</div>" header code)
        code)))
  (advice-add 'org-html-src-block :around #'my/org-html-src-block))

;; Tell Emacs to trust this code in all buffer local vars:
(add-to-list 'safe-local-variable-values
             '(eval add-hook 'after-save-hook 'my/emacs-org-tangle nil t))
(add-to-list 'safe-local-variable-values
             '(org-confirm-babel-evaluate))

(my/cargo-dependency "live-server") ; defers install of live-server Rust crate
(defvar my/emacs-org-html-server-host "127.0.0.1") ; Set to 0.0.0.0 to serve publicly
(defvar my/emacs-org-html-server-port "7776")
(defun my/emacs-org-html-server ()
  "Start a local live-server for the Emacs org HTML export."
  (interactive)
  (let ((live-server-path (executable-find "live-server"))
        (log-buffer-name "*my/emacs/org-html-server*"))
    (with-current-buffer (get-buffer-create log-buffer-name)
      (if live-server-path
          (progn
            (message "live-server found at: %s" live-server-path)
            (let ((host "127.0.0.1")  ; Set to "0.0.0.0" to serve publicly
                  (port "7776")
                  (html-file "index.html")
                  (export-dir (expand-file-name "export" user-emacs-directory)))
              (start-process "live-server" log-buffer-name "live-server"
                             "-H" host "-p" port "-o" "" export-dir)
              (message "Started live-server on http://%s:%s" host port)))
        (unless (executable-find "live-server")
          (message "live-server NOT found - please run: cargo install live-server"))))))
