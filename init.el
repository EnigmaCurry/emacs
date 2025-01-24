(require 'cl-lib)
;; Nice defaults
(setq confirm-kill-emacs #'yes-or-no-p)
(setq vc-follow-symlinks t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default visible-bell t)
(setq-default dired-listing-switches "-al --group-directories-first")
(setq-default tramp-default-method "ssh")
(column-number-mode)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(electric-pair-mode 1)
;; Enable native compilation for all elisp files:
(setq-default native-comp-deferred-compilation-deny-list nil)

(defvar my/machine-labels
  (with-temp-buffer
    (ignore-errors
      (insert-file-contents (expand-file-name "~/.config/machine-labels"))
      (cl-remove-if (lambda (line) (string-match-p "^#" line))
                    (split-string (buffer-string) "\n" t))))
  "List of machine-specific labels read from .machine-labels, ignoring comments.")
(defun my/machine-has-label (label)
  "Check if the current machine is labeled with LABEL."
  (if (member label my/machine-labels)
      t
    nil))

(defvar my/modules-dir (expand-file-name "modules/" user-emacs-directory))
(defun my/load-modules ()
  "Load all Emacs Lisp files in the modules/ directory and subdirectories matching labels."
  (when (file-directory-p my/modules-dir)
    ;; Load all .el files in the root modules/ directory
    (let ((root-files (directory-files my/modules-dir t "\\.el\\'")))
      (dolist (file (sort root-files #'string<))
        (message "Loading module: %s" file)
        (load file nil 'nomessage)))
    ;; Load .el files from subdirectories matching machine labels
    (dolist (subdir (directory-files my/modules-dir t "^[^.]" t)) ;; Skip . and ..
      (when (and (file-directory-p subdir)
                 (my/machine-has-label (file-name-nondirectory subdir)))  ;; Check label
        (let ((files (directory-files subdir t "\\.el\\'")))
          (dolist (file (sort files #'string<))
            (message "Loading module from %s: %s" (file-name-nondirectory subdir) file)
            (load file nil 'nomessage)))))))

  (my/load-modules)
