;; Change the abbreviation table on the fly depending on mode
;; as well as if we are in a comment or quoted string
(require 'pabbrev)
(add-hook 'pre-abbrev-expand-hook 'abbrev-table-change)
(defun abbrev-table-change (&optional args)
  (setq local-abbrev-table
        (if (eq major-mode 'jde-mode)
            (if (jde-parse-comment-or-quoted-p)
                text-mode-abbrev-table
              java-mode-abbrev-table)
          (if (eq major-mode 'python-mode)
              (if (py-in-literal)
                  text-mode-abbrev-table
                python-mode-abbrev-table)
            )
          )
        )
  )
