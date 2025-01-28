;; Magit (git version control system) :: https://magit.vc/
(use-package
  magit
  :general ("C-c g" 'magit-status)
  :config
  ;; open magit in a full frame always:
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))
