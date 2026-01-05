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
  (org-export-with-email nil)
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
  (defcustom my/org-notes-directory "~/Org/notes" "My org notes directory")
  (defcustom my/org-notes-export-directory "~/Org/export/notes" "My org notes HTML export directory")
  ;; Hydra for commonly used org commands:
  (defhydra
    hydra-org
    (global-map "C-c o" :exit t color pink :hint nil)
    "Org commands:"
    ("o" my/org-open-file)
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

(defcustom my/org-html-theme "simple_dark"
  "The name of my custom org theme (CSS).")

(defun my/emacs-org-tangle (&optional file)
  "Tangle all code blocks in emacs.org (or FILE) and export this document to HTML.

Important: output paths are rooted at the *real* directory where emacs.org lives
(i.e. after following symlinks), so this works even if ~/.emacs.d is read-only."
  (interactive)
  (let* (;; Prefer: explicit FILE, then if you're *in* emacs.org use that, else fall back.
         (org-file (or file
                     (and (buffer-file-name)
                          (string-equal (file-name-nondirectory (buffer-file-name)) "emacs.org")
                          (buffer-file-name))
                     ;; Use baked-in “source emacs.org” if available:
                     (and (fboundp 'my/emacs-org-file) (my/emacs-org-file))
                     ;; last resort:
                     (locate-user-emacs-file "emacs.org")))
         ;; Follow symlinks so we write next to the real file, not into the nix store.
         (org-file (file-truename org-file))
         (root-dir (file-name-directory org-file))

         ;; Everything below is now based on root-dir (where emacs.org actually resides).
         (modules-dir (expand-file-name "modules" root-dir))
         (export-dir (expand-file-name my/org-notes-export-directory))
         (export-emacs-dir (expand-file-name "emacs" export-dir))
         (export-file (expand-file-name "index.html" root-dir)))

    (when (file-exists-p org-file)
      (with-current-buffer (find-file-noselect org-file)
        ;; Make sure relative :tangle paths resolve relative to emacs.org’s real dir.
        (let ((default-directory root-dir))
          (when (file-directory-p modules-dir)
            (delete-directory modules-dir t))
          ;; Record origin dir/file at *tangle time* by writing literal strings.
          (let ((origin-el (expand-file-name "modules/origin.el" root-dir)))
            (make-directory (file-name-directory origin-el) t)
            (with-temp-file origin-el
              (insert ";; Auto-generated at tangle time. DO NOT EDIT.\n")
              (insert (format "(defconst my/emacs-org-origin-dir %S\n" root-dir))
              (insert "  \"Directory of the original emacs.org (git checkout), recorded at tangle time.\")\n\n")
              (insert "(defconst my/emacs-org-origin-file\n")
              (insert "  (expand-file-name \"emacs.org\" my/emacs-org-origin-dir)\n")
              (insert "  \"Full path to the original emacs.org, recorded at tangle time.\")\n")))
          (org-babel-tangle)
          (org-export-to-file 'html export-file)

          (unless (file-directory-p export-dir)
            (make-directory export-dir t))
          (unless (file-directory-p export-emacs-dir)
            (make-directory export-emacs-dir t))

          (my/org-create-theme-file org-file)

          ;; Links created relative to root-dir/export dirs:
          (make-symbolic-link "index.html" "emacs.html" t)

          (make-symbolic-link (expand-file-name "index.html" root-dir)
                              (expand-file-name "index.html" export-emacs-dir) t)
          (make-symbolic-link (expand-file-name "index.html" root-dir)
                              (expand-file-name "emacs.html" export-emacs-dir) t)

          ;; If these files are also in your org repo dir, link from root-dir:
          (make-symbolic-link (expand-file-name "early-init.el" root-dir)
                              (expand-file-name "early-init.el" export-emacs-dir) t)
          (make-symbolic-link (expand-file-name "init.el" root-dir)
                              (expand-file-name "init.el" export-emacs-dir) t)
          (make-symbolic-link (expand-file-name "modules" root-dir)
                              (expand-file-name "modules" export-emacs-dir) t)

          (make-symbolic-link (expand-file-name "LICENSE.txt" root-dir)
                              (expand-file-name "LICENSE.txt" export-emacs-dir) t)
          (make-symbolic-link (expand-file-name "LICENSE_GPLv3.txt" root-dir)
                              (expand-file-name "LICENSE_GPLv3.txt" export-emacs-dir) t)

          (make-symbolic-link (expand-file-name "theme" root-dir)
                              (expand-file-name "theme" export-dir) t)
          (make-symbolic-link (expand-file-name "theme" root-dir)
                              (expand-file-name "theme" my/org-notes-directory) t))))))

(with-eval-after-load 'ox-html
  (defun my/org-html-src-block (orig-fun src-block contents info)
    "Advice for `org-html-src-block' to add a header."
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
(defvar my/org-html-server-host "127.0.0.1") ; Set to 0.0.0.0 to serve publicly
(defvar my/org-html-server-port "7776")
(defun my/org-html-server (&optional redirect)
  "Start a local live-server for my org notes.
  If the server is already running, open the URL."
  (interactive)
  (let* ((host my/org-html-server-host)
         (port my/org-html-server-port)
         (redirect (or redirect ""))
         (url (format "http://%s:%s/%s" host port redirect))
         (live-server-proc (get-process "live-server")))
    (if (and live-server-proc (process-live-p live-server-proc))
        (progn
          (message "live-server already running; opening %s" url)
          (browse-url url))
      (let ((live-server-path (executable-find "live-server"))
            (log-buffer-name "*my/org-html-server*")
            (export-dir (expand-file-name my/org-notes-export-directory)))
        (with-current-buffer (get-buffer-create log-buffer-name)
          (if live-server-path
              (progn
                (message "live-server found at: %s" live-server-path)
                (start-process "live-server" log-buffer-name "live-server"
                               "-H" host "-p" port "-o" redirect export-dir)
                (message "Started live-server on %s" url))
            (message "live-server NOT found – please run: cargo install live-server")))))))
(defun my/emacs-org-html-server ()
  "Start a local live-server and redirect to the emacs page"
  (interactive)
  (my/org-html-server "emacs"))
(defun my/org-notes-html-server ()
  "Start a local live-server and redirect to the current Org note file.
If the current buffer is an Org file in `my/org-notes-directory`, tangle
it and export to HTML before serving it."
  (interactive)
  (let ((org-file (buffer-file-name)))
    (if (and org-file
             (string= "org" (file-name-extension org-file))
             (string-prefix-p (expand-file-name my/org-notes-directory) (expand-file-name org-file)))
        (let ((html-file (concat (file-name-sans-extension org-file) ".html")))
          (my/org-babel-tangle org-file)
          (my/org-html-server (file-name-nondirectory html-file)))
      (message "Current buffer is not an Org file in %s" my/org-notes-directory))))

(defcustom my/org-template
  (concat "#+TITLE: {{title}}\n"
          "#+PROPERTY: header-args :exports both :results both :eval never-export\n"
          "#+OPTIONS: noweb:t\n")
  "My default Org template.")

(defun my/org--real-dir (&optional org-file)
  "Return the truename directory for ORG-FILE (or the current buffer file)."
  (let ((f (or org-file (buffer-file-name))))
    (when (and f (stringp f))
      (file-name-directory (file-truename f)))))

(defun my/org-create-theme-file (&optional org-file)
  "Create the theme file in a 'theme' directory next to ORG-FILE.

Theme dir is: <org-file-dir>/theme/
Theme file is: <org-file-dir>/theme/<my/org-html-theme>.theme
CSS file is:   <org-file-dir>/theme/<my/org-html-theme>.css

Returns the absolute theme file path."
  (let* ((base-dir (or (my/org--real-dir org-file)
                       ;; Fallback if called outside a file buffer:
                       (file-name-as-directory (expand-file-name org-directory))))
         (theme-dir (expand-file-name "theme" base-dir))
         (theme-file (expand-file-name (concat my/org-html-theme ".theme") theme-dir))
         (css-file   (expand-file-name (concat my/org-html-theme ".css") theme-dir)))
    (unless (file-directory-p theme-dir)
      (make-directory theme-dir t))
    (with-temp-file theme-file
      (insert "#+HTML_HEAD: <style>\n")
      (if (file-exists-p css-file)
          (let ((css-content (with-temp-buffer
                               (insert-file-contents css-file)
                               (buffer-string))))
            (dolist (line (split-string css-content "\n" t))
              (insert (format "#+HTML_HEAD: %s\n" line))))
        (insert "#+HTML_HEAD: /* CSS file not found */\n"))
      (insert "#+HTML_HEAD: </style>\n"))
    theme-file))

(defun my/org-open-file ()
  "Open a new Org file with the default notes template and include the theme file.

Theme file will be created at: <new-file-dir>/theme/<theme>.theme
And the new file will include: #+SETUPFILE: theme/<theme>.theme"
  (interactive)
  (let* ((formatted-date (format-time-string "%Y-%m-%d"))
         (user-title (read-string "Title for new note: "))
         (safe-title (replace-regexp-in-string " " "-" (downcase user-title)))
         (notes-dir (file-name-as-directory (concat org-directory "/notes")))
         (_ (unless (file-directory-p notes-dir)
              (make-directory notes-dir t)))
         (filename (format "%s/%s-%s.org"
                           notes-dir
                           (format-time-string "%Y-%m-%d-%H-%M-%S")
                           safe-title))
         (section (read-string "Section: " formatted-date))
         (title-section (mapconcat #'capitalize (split-string section " ") " ")))
    (unless (file-directory-p my/org-notes-directory)
      (make-directory my/org-notes-directory t))
    (find-file filename)
    (when (zerop (buffer-size))
      (insert
       (replace-regexp-in-string
        "{{title}}" user-title
        (replace-regexp-in-string
         "{{date}}" formatted-date
         (replace-regexp-in-string
          "{{section}}" section
          (replace-regexp-in-string
           "{{safe-title}}" safe-title
           (replace-regexp-in-string
            "{{title-section}}" title-section
            my/org-template))))))
      ;; Create the theme file next to this new org file, and include it.
      (let ((theme-file (my/org-create-theme-file filename)))
        (insert (format "#+SETUPFILE: theme/%s\n\n"
                        (file-name-nondirectory theme-file)))))))

(defun my/org-babel-tangle (org-file)
  "Tangle and export ORG-FILE to HTML.

Creates/updates theme in <org-file-dir>/theme/ before exporting."
  (when (file-exists-p org-file)
    (let* ((org-file (file-truename org-file))
           (base-dir (file-name-directory org-file)))
      (with-current-buffer (find-file-noselect org-file)
        ;; Ensure relative :tangle paths resolve next to the org file.
        (let ((default-directory base-dir))
          (org-babel-tangle)
          (my/org-create-theme-file org-file)
          (org-export-to-file
              'html
            (expand-file-name
             (file-name-nondirectory (org-export-output-file-name ".html" nil))
             my/org-notes-export-directory)))))))

(defun my/save-buffer-after-code-execution (orig-fun &rest args)
  "Execute ORIG-FUN with ARGS and save the buffer afterward.
If the code block is executed asynchronously (i.e. returns a process),
attach a sentinel so that `save-buffer` is called when the process finishes.
Otherwise, call `save-buffer` immediately."
  (let ((result (apply orig-fun args)))
    (if (processp result)
        (set-process-sentinel
         result
         (lambda (_proc event)
           ;; Adjust this if your process returns a different finished event.
           (when (string-match-p "finished" event)
             (save-buffer))))
      (save-buffer))
    result))
(advice-add 'org-babel-execute-src-block :around #'my/save-buffer-after-code-execution)
(advice-add 'rustic-babel-run-update-result-block :around #'my/save-buffer-after-code-execution)
