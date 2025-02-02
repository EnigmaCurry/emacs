(defun my/default-text-scale-reset nil
  (setq default-text-scale--complement 0)
  (set-face-attribute 'default
                      nil
                      :height my/default-text-height)
  (message "Text height reset: %d" my/default-text-height)
  )
;; Scale text sizes in all buffers :: https://github.com/purcell/default-text-scale
(use-package
  default-text-scale
  :general
  ("C-="
   'default-text-scale-increase
   "C--"
   (lambda ()
     "Reset text scale if C-u is used, otherwise decrease it."
     (interactive)
     (let ((prefix current-prefix-arg))
       ;; Intercept and clear the prefix argument before calling the function
       (setq current-prefix-arg nil)
       (if prefix
           (my/default-text-scale-reset)
         (default-text-scale-decrease)))))
  :init (setq default-text-scale-amount 5))
