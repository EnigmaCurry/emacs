;;; function to determine the current git branch -- ignore this in context
(defun my/emacs-git-branch ()
  (substring (shell-command-to-string "git branch --show-current") 0 -1))
