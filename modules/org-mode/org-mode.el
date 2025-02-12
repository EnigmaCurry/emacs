(defcustom my/org-template
  (concat "#+TITLE: {{title}}\n"
          "#+PROPERTY: header-args :results none :eval yes\n"
          "#+OPTIONS: noweb:t\n")
  "My default Org template.")

(defun my/org-create-theme-file ()
  "Create the theme file in the 'theme' directory under `user-emacs-directory`.
This file wraps the contents of the theme CSS (also in the theme directory)
with HTML <style> tags for use with Org export.
Additionally, create a symlink to the .theme file in `my/org-notes-directory`.
For example, if `my/org-html-theme` is \"simple_dark\", then
~/Org/notes/simple_dark.theme will point to ~/.emacs.d/theme/simple_dark.theme."
  (let* ((theme-dir (expand-file-name "theme" user-emacs-directory))
         (theme-file (expand-file-name (concat my/org-html-theme ".theme") theme-dir))
         (css-file (expand-file-name (concat my/org-html-theme ".css") theme-dir)))
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
    (let ((symlink (expand-file-name (concat my/org-html-theme ".theme")
                                     my/org-notes-directory)))
      (unless (file-directory-p my/org-notes-directory)
        (make-directory my/org-notes-directory t))
      (when (file-exists-p symlink)
        (delete-file symlink))
      (make-symbolic-link theme-file symlink))
    theme-file))

(defun my/org-open-file ()
  "Open a new Org file with the default notes template and include the theme file.
  This function does the following:
  1. Opens a new Org file in `org-directory/notes` and inserts the template,
     replacing placeholders like {{title}}, {{date}}, etc.
  2. If the directory `org-directory/notes` does not exist, it is created.
  3. It creates (or updates) the theme file via `my/org-create-theme-file`, which is
     stored in `user-emacs-directory/theme/{{theme}}.theme`.
  4. The Org file then includes a line: \"#+SETUPFILE: {{theme}}.theme\" so that
     when you export, Org loads the theme file."
  (interactive)
  (let* ((formatted-date (format-time-string "%Y-%m-%d"))
         (user-title (read-string "Title for new note: "))
         (safe-title (replace-regexp-in-string " " "-" (downcase user-title)))
         ;; Define the notes directory and ensure it exists.
         (notes-dir (file-name-as-directory (concat org-directory "/notes")))
         (_ (unless (file-directory-p notes-dir)
              (make-directory notes-dir t)))
         (filename (format "%s/%s-%s.org"
                           notes-dir
                           (format-time-string "%Y-%m-%d-%H-%M-%S")
                           safe-title))
         (section (read-string "Section: " formatted-date))
         (title-section (mapconcat #'capitalize (split-string section " ") " ")))
    (find-file filename)
    (when (zerop (buffer-size))
      ;; Insert the Org template with placeholders replaced.
      (insert
       (replace-regexp-in-string
        "{{title}}"
        user-title
        (replace-regexp-in-string
         "{{date}}"
         formatted-date
         (replace-regexp-in-string
          "{{section}}"
          section
          (replace-regexp-in-string
           "{{safe-title}}"
           safe-title
           (replace-regexp-in-string
            "{{title-section}}"
            title-section
            my/org-template))))))
      ;; Create the theme file and insert the setup line.
      (let ((theme-file (my/org-create-theme-file)))
        (insert (format "#+SETUPFILE: %s\n\n"
                        (file-name-nondirectory theme-file)))))))
