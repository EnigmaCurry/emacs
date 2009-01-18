;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;There are TWO python modes
; 1) Tim Peter's python-mode.el -- this is the standard/legacy way
; 2) Dave Love's python.el -- this is when Dave Love got frustrated
;                             that python-mode wasn't accepting his patches
;
;The following directory has a .nosearch file in it therefore it not in
;the current load-path and the default python-mode will be used instead
;The following loads Dave Love's python mode:
(add-to-list 'load-path "~/.emacs.d/dave-loves-python-mode")
(load-library "python")

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
	    interpreter-mode-alist)
      python-mode-hook
      '(lambda () (progn
		    (set-variable 'py-indent-offset 4)
		    (set-variable 'py-smart-indentation nil)
		    (set-variable 'indent-tabs-mode nil) 
		    ;;(highlight-beyond-fill-column)
                    (define-key python-mode-map "\C-m" 'newline-and-indent)
		    ;(pabbrev-mode)
		    ;(abbrev-mode)
	 )
      )
)


;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)


;;Ryan's python specific tab completion
(defun ryan-python-complete ()
  (interactive)
  (if (eq (yas/expand) nil)
      nil
    (rope-code-assist nil)
    )
  )

;;Autofill comments
;;TODO: make this work for docstrings too.
;;      but docstrings just use font-lock-string-face unfortunately
(add-hook 'python-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))

;; (brm-init)
;; (require 'pycomplete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put the following in your .emacs so that the
;; abbrev table is set correctly in all modes.
;; (Not just for java)
;;
;; (add-hook 'pre-abbrev-expand-hook 'abbrev-table-change)
;; (defun abbrev-table-change (&optional args)
;;   (setq local-abbrev-table
;; 	(if (eq major-mode 'jde-mode)
;; 	    (if (jde-parse-comment-or-quoted-p)
;; 		text-mode-abbrev-table
;; 	      java-mode-abbrev-table)
;; 	  (if (eq major-mode 'python-mode)
;; 	      (if (inside-comment-p)
;; 		  text-mode-abbrev-table
;; 		python-mode-abbrev-table
;; 		))))
;;   )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(snippet-with-abbrev-table 'python-mode-abbrev-table
;			   ("hdr" .  "#!/usr/bin/env python\n# -*- coding: utf-8 -*-\n\"\"\"\n\"\"\"\n\n__author__ = \"Ryan McGuire <ryan@enigmacurry.com>\"")
;			   ("main" .  "if __name__ == \"__main__\":\n$>")
;			   ("cls" . "class $${class_name}($${object}):\n$>\"\"\"\n$>$${class documentation}\n$>\"\"\"\n$>$.")
;			   )


