;; This is my attempt to make the holy grail of tab completion possible
;; -Ryan

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

;;;
;; Smart Tab
;; Taken from http://www.emacswiki.org/cgi-bin/wiki/TabCompletion
(global-set-key [(tab)] 'smart-tab)
(defvar smart-tab-completion-functions
  '((emacs-lisp-mode lisp-complete-symbol)
    (lisp-mode slime-complete-symbol)
    (python-mode rope-code-assist)
    (text-mode dabbrev-completion))
  "List of major modes in which to use a mode specific completion
  function.")

(defun get-completion-function()
  "Get a completion function according to current major mode."
  (let ((completion-function
         (second (assq major-mode smart-tab-completion-functions))))
    (if (null completion-function)
        'dabbrev-completion
        completion-function)))

(defun smart-tab (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`smart-indent'. Else if point is at the end of a symbol,
expands it. Else calls `smart-indent'."
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (if (smart-tab-must-expand prefix)
        (let ((dabbrev-case-fold-search t)
              (dabbrev-case-replace nil))
          (funcall (get-completion-function))))
    (smart-indent)))

(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
    (looking-at "\\_>")))
(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))


;;;
;; (defvar smart-tab-using-hippie-expand nil
;;   "turn this on if you want to use hippie-expand completion.")
;; (global-set-key [(tab)] 'smart-tab)
;; (defun smart-tab (prefix)
;;   "Needs `transient-mark-mode' to be on. This smart tab is
;; minibuffer compliant: it acts as usual in the minibuffer.
;; In all other buffers: if PREFIX is \\[universal-argument], calls
;; `smart-indent'. Else if point is at the end of a symbol,
;; expands it. Else calls `smart-indent'."
;;   (interactive "P")
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (smart-tab-must-expand prefix)
;;         (if smart-tab-using-hippie-expand
;;             (hippie-expand nil)
;;           (dabbrev-expand nil))
;;       (smart-indent))))
;; (defun smart-indent ()
;;   "Indents region if mark is active, or current line otherwise."
;;   (interactive)
;;   (if mark-active
;;       (indent-region (region-beginning)
;;                      (region-end))
;;     (indent-for-tab-command)))
;; (defun smart-tab-must-expand (&optional prefix)
;;   "If PREFIX is \\[universal-argument], answers no.
;; Otherwise, analyses point position and answers."
;;   (unless (or (consp prefix)
;;               mark-active)
;;     (looking-at "\\_>")))
;; ;;; End smart tab


