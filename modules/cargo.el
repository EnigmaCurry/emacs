;; Ensure that cargo (Rust) is installed and found in PATH.
(unless (executable-find "cargo")
  (let ((cargo-bin (expand-file-name "~/.cargo/bin"))
        (log-buffer-name "*Cargo Setup Log*"))
    (with-current-buffer (get-buffer-create log-buffer-name)
      (erase-buffer)  ;; Clear previous content if the buffer exists
      (insert "Checking for Cargo environment setup...\n\n")
      (my/add-exec-path "~/.cargo/bin")
      (my/check-binary-availability "cargo" log-buffer-name))))
