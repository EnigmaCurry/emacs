(my/add-exec-path "~/.cargo/bin")
(unless (executable-find "cargo")
  (message "cargo binary NOT found."))

(defun my/cargo-package-installed-p (package)
  "Check if a Cargo PACKAGE is installed."
  (with-temp-buffer
    (let ((exit-code (call-process "cargo" nil t nil "install" "--list")))
      (if (zerop exit-code)
          (progn
            (goto-char (point-min))
            (search-forward package nil t))
        (error "Failed to run 'cargo install --list'")))))
(defun my/cargo-install (programs)
  "Install a list of PROGRAMS via `cargo install`, skipping those already installed.
PROGRAMS can be a single string (with space-separated programs) or a list of strings."
  (unless programs
    (user-error "No programs specified for cargo install"))
  ;; Handle single string: split by spaces and convert to list
  (setq programs (if (stringp programs)
                     (split-string programs) ;; Split a single string into a list
                   programs))
  ;; Filter out installed packages
  (let ((to-install (seq-remove #'my/cargo-package-installed-p programs)))
    (if (null to-install)
        (message "All specified programs are already installed")
      (let* ((buffer-name (concat "*cargo install "
                                  (string-join to-install " ") "*"))
             (existing-buffer (get-buffer buffer-name)))
        (if (and existing-buffer (get-buffer-process existing-buffer))
            (user-error "Cargo install already in progress for programs: %s"
                        (string-join to-install ", "))
          (when existing-buffer
            (kill-buffer existing-buffer))
          (let ((buffer (get-buffer-create buffer-name)))
            (start-process "cargo-install" buffer
                           "/bin/bash" "-c" (concat "cargo install "
                                            (string-join to-install " ")))
            (display-buffer buffer)
            (with-current-buffer buffer
              (comint-mode)
              (goto-char (point-min)))))))))
(defvar my/cargo-dependencies nil
  "A list of Rust crate dependencies to be installed.")
(defun my/cargo-dependency (crate)
  "Add a Rust CRATE to the list of dependencies."
  (unless (member crate my/cargo-dependencies)
    (setq my/cargo-dependencies (cons crate my/cargo-dependencies))))
