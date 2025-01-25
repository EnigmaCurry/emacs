;; Ensure that cargo (Rust) is installed and found in PATH.
(let ((cargo-bin (expand-file-name "~/.cargo/bin"))
      (log-buffer-name "*Cargo Setup Log*"))
  (with-current-buffer (get-buffer-create log-buffer-name)
    (erase-buffer)  ;; Clear previous content if the buffer exists
    (insert "Checking for Cargo environment setup...\n\n")
    (unless (member cargo-bin exec-path)
      (insert "Adding ~/.cargo/bin to PATH\n")
      (setenv "PATH" (concat cargo-bin path-separator (getenv "PATH")))
      (add-to-list 'exec-path cargo-bin)
      (insert "~/.cargo/bin added to PATH.\n"))
    (if (member cargo-bin exec-path)
        (insert "~/.cargo/bin is already in PATH.\n")
      (insert "Failed to add ~/.cargo/bin to PATH.\n"))
    ;; Check if `cargo` is available
    (if (executable-find "cargo")
        (progn
          (insert "Cargo is available in PATH.\n")
          (kill-buffer log-buffer-name))
      (progn
        (message "cargo binary is NOT found.")
        (insert "cargo binary is NOT found.\n")
        (display-buffer log-buffer-name)))))
