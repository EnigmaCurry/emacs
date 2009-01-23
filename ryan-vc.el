(defadvice log-edit-done (around enforce-non-blank-log-message activate)
  "Enforce that commit logs are not blank"
  (if (not (string-match "\\w" (buffer-string)))
      (message "Commit message must not be blank.")
    ad-do-it))
