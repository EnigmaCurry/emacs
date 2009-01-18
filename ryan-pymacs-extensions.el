;add ~/.emacs.d/ryan-python-extensions to python path
(pymacs-exec "import sys, os")
(pymacs-exec "sys.path.append(os.path.join(os.path.expanduser('~'),'.emacs.d','ryan-pymacs-extensions'))")


;;Shorten URLs with is.gd
(pymacs-exec "import shorten_url")
(defun shorten-url (start end)
  (interactive "r")
  (let ((region (buffer-substring start end)))
    (let ((rt (pymacs-eval (format "shorten_url.shorten_in_text('''%s''')" region))))
      (kill-region start end)
      (insert rt)
      )
  ))


