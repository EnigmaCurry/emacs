(my/add-exec-path "~/.cargo/bin")
(unless (executable-find "cargo")
  (message "cargo binary NOT found."))
