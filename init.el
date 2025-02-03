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
;; Store all customizations under my/custom-settings group
(defgroup my/custom-settings nil
  "My custom Emacs settings"
  :group 'emacs)
;; Shortcut to open custom settings:
(defun my/custom-settings ()
  "Open the Emacs customization interface for my custom settings."
  (interactive)
  (customize-group 'my/custom-settings))
(defalias 'my/settings 'my/custom-settings)

;; Global minor modes
(column-number-mode)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
;(electric-pair-mode t)

;; Function to add a directory to PATH and exec-path
(defun my/add-exec-path (dir)
  "Add DIR to the environment PATH and exec-path if not already present."
  (unless (member dir exec-path)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))
    (add-to-list 'exec-path dir)))

(defun my/bootstrap-straight (&rest _)
  "Bootstrap straight.el only if it's not already installed."
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
          (eval-print-last-sexp)
          ))
      (load bootstrap-file nil 'nomessage)
      (setq straight-use-package-by-default t)
      (straight-use-package 'use-package))))
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
    (cl-sort (mapcar #'file-name-nondirectory
                     (seq-filter #'file-directory-p
                                 (directory-files modules-dir t "^[^.]" t))) #'string<)))
(defun my/machine-labels-enable-all nil
  "Adds ALL existing machine labels to the custom my/machine-labels"
  (interactive)
  (let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
    (when (y-or-n-p (format "Do you want to enable ALL Emacs modules from %s? " modules-dir))
      (progn
        (customize-set-variable 'my/machine-labels (my/machine-labels-available))
        (customize-save-customized)))))

(defvar my/modules-dir (expand-file-name "modules/" user-emacs-directory))
(defvar my/module-priority-list '("general" "fonts")
  "List of prioritized modules to install first.")
(defun my/load-modules (requested-modules)
  "Load user-requested modules in a priority order.
  REQUESTED-MODULES is a list of module names to load."
  ;; Find and load all single file modules and load them regardless of any config
  ;; (these onesshouldn't have any extra dependencies)
  (when (file-directory-p my/modules-dir)
    (let ((files (directory-files my/modules-dir t "\\.el\\'")))
      (dolist (file (sort files #'string<))
        (message "Loading module: %s" file)
        (condition-case err
            (load file nil 'nomessage)
          (error (message "Error loading %s: %s" file err))))))
  ;; Prioritize and install the requested third party / optional modules:
  (let ((prioritized-modules
         (seq-filter (lambda (mod) (member mod my/module-priority-list))
                     requested-modules))
        (remaining-modules
         (seq-remove (lambda (mod) (member mod my/module-priority-list))
                     requested-modules)))
    ;; Sort prioritized modules based on `my/module-priority-list`
    (setq prioritized-modules
          (sort prioritized-modules
                (lambda (a b)
                  (< (or (cl-position a my/module-priority-list)
                         most-positive-fixnum)
                     (or (cl-position b my/module-priority-list)
                         most-positive-fixnum)))))
    ;; Combine prioritized and remaining modules
    (let ((ordered-modules (append prioritized-modules
                                   remaining-modules)))
      (dolist (mod ordered-modules)
        (let ((mod-path (expand-file-name mod my/modules-dir)))
          (if (file-directory-p mod-path)
              (progn
                (message "Loading module: %s" mod)
                (let ((files (directory-files mod-path t "\\.el\\'")))
                  (dolist (file (sort files #'string<))
                    (condition-case err
                        (load file nil 'nomessage)
                      (error (message "Error loading %s: %s" file err))))))
            (message "Skipping module: %s (not found)" mod)))))))

;; load the modules configured for this macchine
(my/load-modules my/machine-labels)

;; Install rust dependencies that were declared by modules
(when my/cargo-dependencies
  (my/cargo-install my/cargo-dependencies))
