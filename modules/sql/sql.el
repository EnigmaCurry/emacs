;; https://github.com/purcell/sqlformat
;; https://github.com/darold/pgFormatter
(use-package
  sqlformat
  :config
  (general-define-key
   :keymaps 'sql-mode-map
   "C-c f" 'sqlformat)
  :init
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-f" "1"))
  ;(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
  )
