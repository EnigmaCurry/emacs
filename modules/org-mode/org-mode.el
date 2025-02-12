(defcustom my/org-template
  (concat "#+TITLE: {{title}}\n"
          "#+PROPERTY: header-args :results none :eval yes\n"
          "#+OPTIONS: noweb:t\n\n")
  "My default Org template.")

(defun my/open-org-file ()
  "Open a new Org file with the default notes template and create a theme file.
This function does the following:
1. Opens a new Org file in the directory ‘org-directory/notes’ and inserts the template
   (replacing placeholders like {{title}}, {{date}}, etc.).
2. If the directory ‘org-directory/notes’ does not exist, it is created.
3. In the same directory, it creates a file named 'simple_dark.theme'. This file
   is populated with a series of #+HTML_HEAD: lines that wrap the contents of your CSS
   file (located at `user-emacs-directory/themes/simple_dark.css`) between a starting
   <style> tag and a closing </style> tag.
4. The Org file then includes a line: \"#+SETUPFILE: simple_dark.theme\"
   so that when you export, Org loads the theme file."
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
      ;; Create the theme file.
      (let* ((theme-file (expand-file-name "simple_dark.theme"
                                             (file-name-directory filename)))
             (css-file (expand-file-name "theme/simple_dark.css"
                                          user-emacs-directory)))
        (with-temp-file theme-file
          ;; Start the CSS block.
          (insert "#+HTML_HEAD: <style>\n")
          (if (file-exists-p css-file)
              (let ((css-content (with-temp-buffer
                                   (insert-file-contents css-file)
                                   (buffer-string))))
                (dolist (line (split-string css-content "\n"))
                  (insert (format "#+HTML_HEAD: %s\n" line))))
            (insert "#+HTML_HEAD: /* CSS file not found */\n"))
          ;; End the CSS block.
          (insert "#+HTML_HEAD: </style>\n"))
        (message "Theme file created at: %s" theme-file)
        ;; Insert a reference to the theme file into the Org file.
        (insert (format "\n#+SETUPFILE: %s\n"
                        (file-name-nondirectory theme-file)))))))
