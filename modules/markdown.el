(defvar markdown-electric-pairs '((96 . 96) (?* . ?*))
  "Electric pairs for markdown-mode.")
(defun markdown-add-electric-pairs ()
  (setq-local electric-pair-pairs
              (append electric-pair-pairs markdown-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))
(add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)
