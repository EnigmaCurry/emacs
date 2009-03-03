;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Occur mode helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun occurrences (regexp &rest ignore)
  "Show all matches for REGEXP in an `occur' buffer."
  ;; keep text covered by occur-prefix and match text-properties
  (interactive (occur-read-primary-args))
  (occur regexp)
  (with-current-buffer (get-buffer "*Occur*")
    (let ((inhibit-read-only t)
	  delete-from
	  pos)
      (save-excursion
	(while (setq pos (next-property-change (point)))
	  (goto-char pos)
	  (if (not (or (get-text-property (point) 'occur-prefix)
		       (get-text-property (point) 'occur-match)))
	      (if delete-from
		  (delete-region delete-from (point))
		(setq delete-from (point)))
	    (when delete-from
	      (delete-region delete-from (point))
	      (if (get-text-property (point) 'occur-prefix)
		  (insert "\n")
		(insert " ")))
	    (setq delete-from nil)))))))
 (defun occur-mode-clean-buffer ()
   "Removes all commentary from the *Occur* buffer, leaving the
 unadorned lines."
   (interactive)
   (if (get-buffer "*Occur*")
       (save-excursion
         (set-buffer (get-buffer "*Occur*"))
         (goto-char (point-min))
         (toggle-read-only 0)
         (if (looking-at "^[0-9]+ lines matching \"")
             (kill-line 1))
         (while (re-search-forward "^[ \t]*[0-9]+:"
                                   (point-max)
                                   t)
           (replace-match "")
           (forward-line 1)))
     (message "There is no buffer named \"*Occur*\".")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open files as root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
	"*The filename prefix used to open a file with `find-file-root'.")

   (defvar find-file-root-history nil
     "History list for files found using `find-file-root'.")

   (defvar find-file-root-hook nil
     "Normal hook for functions to run after finding a \"root\" file.")

   (defun find-file-root ()
     "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

   (interactive)
   (require 'tramp)
   (let* (;; We bind the variable `file-name-history' locally so we can
   	 ;; use a separate history list for "root" files.
   	 (file-name-history find-file-root-history)
   	 (name (or buffer-file-name default-directory))
   	 (tramp (and (tramp-tramp-file-p name)
   		     (tramp-dissect-file-name name)))
   	 path dir file)

     ;; If called from a "root" file, we need to fix up the path.
     (when tramp
       (setq path (tramp-file-name-path tramp)
   	    dir (file-name-directory path)))

     (when (setq file (read-file-name "Find file (UID = 0): " dir path))
       (find-file (concat find-file-root-prefix file))
       ;; If this all succeeded save our new history list.
       (setq find-file-root-history file-name-history)
       ;; allow some user customization
       (run-hooks 'find-file-root-hook))))

   (global-set-key [(control x) (control r)] 'find-file-root)


(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))