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
(setq-default browse-url-browser-function 'browse-url-firefox)
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
;(electric-pair-mode t)

;; Function to bootstrap straight.el only when needed
(defun my/bootstrap-straight (&rest _)
  "Bootstrap straight.el if it's not already installed."
  (unless (bound-and-true-p straight--build-dir)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el"
                             user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent
             'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)
      (setq straight-use-package-by-default t)
      (straight-use-package 'use-package)
      (straight-pull-recipe-repositories))))
;; Advise `use-package` to initialize straight.el when first called
(advice-add 'use-package :before #'my/bootstrap-straight)

(defvar my/use-package-tracked-list nil
  "A list to track the names of packages declared via `use-package`.")
(defun my/use-package-tracked-list ()
  "Display the tracked `use-package` packages in a new buffer, one per line.
If the buffer already exists, delete it and recreate it."
  (interactive)
  (let ((buffer-name "*Tracked Packages*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "# Packages tracked via use-package:\n")
        (if my/use-package-tracked-list
            (dolist (pkg (sort my/use-package-tracked-list #'string<))
              (insert (format "%s\n" pkg)))
          (insert "No packages tracked.\n"))
        (read-only-mode 1))
      (pop-to-buffer buffer))))
(defun my/use-package-advice (orig-fun &rest args)
  "Advice around `use-package' to track package names."
  (when (symbolp (car args))
    (push (symbol-name (car args)) my/use-package-tracked-list)
    (setq my/use-package-tracked-list (delete-dups my/use-package-tracked-list)))
  (apply orig-fun args))

(advice-add 'use-package :around #'my/use-package-advice)

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
