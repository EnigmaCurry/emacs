;; core libraries
(require 'cl-lib)

;; Nice defaults
(setq-default confirm-kill-emacs #'yes-or-no-p)
(setq-default vc-follow-symlinks t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default visible-bell t)
(setq-default dired-listing-switches "-al --group-directories-first")
(setq-default tramp-default-method "ssh")
(setq-default native-comp-deferred-compilation-deny-list nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Backups and auto-save
;; Reference: https://www.emacswiki.org/emacs/BackupDirectory
;; Reference: https://www.emacswiki.org/emacs/ForceBackups
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)
(add-hook 'before-save-hook
          (lambda () (setq buffer-backed-up nil)))
;; autosaves go in a separate directory
(let ((auto-save-dir (expand-file-name "auto-save" user-emacs-directory)))
  (make-directory auto-save-dir t)
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

;; Store customizations in custom.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
;; Shortcut to open custom settings:
(defun my/custom-settings ()
  "Open the Emacs customization interface for my custom settings."
  (interactive)
  (customize-group 'my/custom-settings))

;; Global minor modes
(column-number-mode)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(electric-pair-mode t)

;; Customize which emacs config modules to load per-machine:
(defcustom my/machine-labels '()
  "List of machine-specific labels to configure which modules to load."
  :type '(repeat string)
  :group 'my/custom-settings)
(defun my/machine-labels ()
  "Return the list of machine-specific labels."
  (interactive)
  my/machine-labels)
(defun my/machine-has-label (label)
  "Check if the current machine is labeled with LABEL."
  (if (member label my/machine-labels)
      t
    nil))
(defun my/machine-labels-available ()
  "List all available machine labels"
  (let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
    (mapcar #'file-name-nondirectory
            (seq-filter #'file-directory-p
                        (directory-files modules-dir t "^[^.]" t)))))

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

;; load the modules configured for this macchine
(my/load-modules)
