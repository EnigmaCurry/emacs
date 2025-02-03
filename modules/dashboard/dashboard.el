(use-package
  dashboard
  :init
  (defun my-dashboard-insert-vocabulary (list-size)
    (when (fboundp 'latin-word-of-the-day)
      (dashboard-insert-heading "Word of the day:" nil)
      (insert "\n")
      (let* ((char-limit 100000)
             (word (latin-word-of-the-day))
             (description (latin-word-get-description word)))
        (insert
         (substring description
                    0
                    (min char-limit (length description)))))))
  (dashboard-setup-startup-hook)
  :hook
  ((dashboard-mode . (lambda () (setq-local show-trailing-whitespace nil))))
  :custom
  (dashboard-center-content t)
  (dashboard-set-heading-icons nil)
  (dashboard-set-file-icons nil)
  (dashboard-icon-type nil)
  (dashboard-footer-messages (list "    "))
  (dashboard-items '((recents . 5) (bookmarks . 5) (vocabulary)))
  (dashboard-startup-banner (+ 1 (random 3)))
  (dashboard-item-generators
   '((vocabulary . my-dashboard-insert-vocabulary)
     (recents . dashboard-insert-recents)
     (bookmarks . dashboard-insert-bookmarks))))
