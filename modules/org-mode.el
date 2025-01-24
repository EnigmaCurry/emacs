(defun my/tangle-emacs-org ()
  "Tangle the 'emacs.org' file in the user-emacs-directory."
  (let ((org-file (expand-file-name "emacs.org" user-emacs-directory)))
    (when (file-exists-p org-file)
      (org-babel-tangle-file org-file))))

(require 'org-tempo) ; required for Structure Templates
                     ; See https://orgmode.org/manual/Structure-Templates.html
