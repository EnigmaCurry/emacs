(my/add-exec-path "~/.cargo/bin")
(unless (executable-find "cargo")
  (message "cargo binary NOT found."))

(require 'seq)
 (require 'comint)

 (defun my/cargo-package-installed-p (package)
   "Check if a given cargo PACKAGE is installed.
       PACKAGE can be either a string or a cons cell (CRATE . GIT-URL).
       In either case, this function uses the crate name for the check."
   (let ((pkg (if (consp package) (car package) package)))
     (with-temp-buffer
       (when (eq 0 (call-process "cargo" nil t nil "install" "--list"))
         (goto-char (point-min))
         (search-forward pkg nil t)))))
(defun my/cargo-install (programs)
   "Install a list of PROGRAMS via `cargo install`, skipping those already installed.
 PROGRAMS can be:
 - a single string (with space-separated program names),
 - a list of strings,
 - or a list where some elements are cons cells (CRATE . GIT-REPO).

 For cons cell entries, Cargo is invoked with the --git flag."
   (unless programs
     (user-error "No programs specified for cargo install"))
   ;; If PROGRAMS is a string, split it into a list.
   (setq programs (if (stringp programs)
                      (split-string programs)
                    programs))
   ;; Remove already installed programs.
   (setq programs (seq-remove #'my/cargo-package-installed-p programs))
   ;; Separate into two groups:
   ;;   - crates from crates.io (plain strings or cons cells treated as a name only)
   ;;   - git dependencies (explicit cons cells)
   (let ((crate-names (mapcar (lambda (dep)
                                (if (consp dep)
                                    (car dep)
                                  dep))
                              (seq-filter (lambda (dep)
                                            (not (consp dep)))
                                          programs)))
         (git-deps (seq-filter #'consp programs)))
     ;; Install crates from crates.io in one command, if any.
     (when crate-names
       (let ((command (concat "cargo install " (string-join crate-names " "))))
         (my/shell-execute command)))
     ;; Install each git dependency separately.
     (dolist (dep git-deps)
       (let* ((crate (car dep))
              (git-repo (cdr dep))
              (command (format "cargo install %s --git %s" crate git-repo)))
         (my/shell-execute command)))))
 (defvar my/cargo-dependencies nil
   "A list of Rust crate dependencies to be installed.
         Each dependency is either a crate name (a symbol or string) or a cons cell
         of the form (CRATE . GIT-REPO).")

 (defun my/cargo-dependency (crate &optional git-repo)
   "Add a Rust CRATE to the list of dependencies.
         If GIT-REPO is provided, the dependency is stored as (CRATE . GIT-REPO).
         Otherwise, only CRATE is stored.
         If the dependency (by crate name) already exists, do nothing."
   (unless (cl-find crate my/cargo-dependencies
                    :test (lambda (d c)
                            (if (consp d)
                                (equal (car d) c)
                              (equal d c))))
     (push (if git-repo (cons crate git-repo) crate)
           my/cargo-dependencies)))
