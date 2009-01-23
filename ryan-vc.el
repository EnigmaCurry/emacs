(add-hook 'vc-logentry-check-hook 'flyspell-mode)

(defadvice log-edit-done (around enforce-non-blank-log-message activate)
  "Enforce that commit logs are not blank"
  (if (string-match "\\w" (buffer-string))
      (message "Commit message must not be blank.")
    ad-do-it))
