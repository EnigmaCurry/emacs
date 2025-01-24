(defvar my/modules-dir (expand-file-name "modules/" user-emacs-directory))
(defun my/load-modules ()
  "Load all Emacs Lisp files in the modules sub-directory in lexicographic order."
  (when (file-directory-p my/modules-dir)
    (let ((files (directory-files my/modules-dir t "\\.el\\'")))
      (dolist (file (sort files #'string<))
	(message "Loading module: %s" file)
	(load file nil 'nomessage)))))
(my/load-modules)
