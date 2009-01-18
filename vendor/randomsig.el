;;; randomsig.el --- insert a randomly selected signature

;; Copyright (C) 2001, 2002 Hans-Jürgen Ficker

;; Emacs Lisp Archive Entry
;; Author: Hans-Juergen Ficker <hj@backmes.de>
;; Version: 0.7.0
;; X-CVS-Version: $Id: randomsig.el,v 1.35 2003/07/17 14:54:15 hjficker Exp $
;; Keywords: mail random signature

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is yet another implementation to insert a randomly choosen 
;; signature into a mail.

;; It is only tested with gnus.

;; To make it work, put the following lines into your ~/.gnus:

;; (require 'randomsig)
;; (define-key message-mode-map (kbd "C-c s") 'randomsig-replace-sig)
;; (define-key message-mode-map (kbd "C-c S") 'randomsig-select-sig)
;; (require 'gnus-sum) ; probably required for `gnus-summary-save-map'
;; (define-key gnus-summary-save-map "-" 'gnus/randomsig-summary-read-sig)
;; (setq randomsig-dir "/some/directory")
;; (setq randomsig-files '("some" "files"))
;; ;; or (setq randomsig-files (randomsig-search-sigfiles))
;; ;; or (setq randomsig-files 'randomsig-search-sigfiles)
;; (setq message-signature 'randomsig-signature)

;; This will also define the shortcut `C-c s' in message-mode to
;; change the signature, `C-c S' in message-mode to interactively
;; select the signature to replace the current signature, and `O -' in
;; gnus-summary-mode to read the signature from the selected mail.

;; `randomsig-files' must be a list of existing files, an existing
;; file, or a function returning a list of existing files. If these
;; don't have absolute paths, they are located in `randomsig-dir'.

;; File format: Each file must contain at least one signature.
;; Signatures are separated with `randomsig-delimiter-pattern'. If
;; there is only one signature in the file, the delimiter can be
;; omitted, so real .signature-files can be used.

;; `randomsig-delimiter' is used when inserting new signatures with
;; `randomsig-message-read-sig' into the signature file. So
;; `randomsig-delimiter' should match `randomsig-delimiter-pattern'.

;; `randomsig-static-string' is put in front of every random signature
;; if non-`nil'.

;; The *-read-sig functions read the signature of a message, or use
;; the marked text, and write it to a signature-file, for which the
;; name is asked. If the file does not exist, it will be generated.
;; When called with any prefix, the signatures will be offered to edit
;; before saving.

;; if `randomsig-replace-sig' is called with any prefix, it will ask
;; for a file to get the signature from.

;; `randomsig-select-sig' will offer a list of signatures to select
;; from in an extra buffer. n will jump to the next signature, p to
;; the previous, RET will insert the selected signature, q will exit
;; the selection buffer without replacing the current signature, R
;; will reload the signature-files, and e will open a buffer for
;; editing the signature at the point. When called with any prefix, it
;; will ask for a file to get the signatures from

;; `randomsig-search-sigfiles' will search for regular files in
;; `randomsig-dir', which do not match `randomsig-search-unwanted'. A
;; subdirectory of `randomsig-dir' can be given as optional argument.

;; Completion will only work for files in `randomsig-files', though
;; others files can be used, too.

;;; Changelog:

;; 2001/04/12   0.1   
;; * Initial release

;; 2001/04/19   0.2  
;; * inserted `randomsig-delimiter' to add the capability to change
;;   the delimiter between the signatures (thanks to Andreas Büsching
;;   <crunchy@tzi.de>)

;; 2001/04/25   0.3
;; * new function `randomsig-search-sigfiles', to search all regular files 
;;   in directory `randomsig-dir'
;; * normal signatures only worked, when using only one signature. Fixed.

;; 2001/04/25   0.3.1
;; * Fixed a bug in `randomsig-search-sigfiles'

;; 2001/04/26   0.3.2
;; * replaced `point-at-eol' with `line-end-position' (Don't know where 
;;   `point-at-eol' is defined)
;; * require cl
;; * require message in some functions

;; 2001/07/09   0.3.3
;; * don't (setq message-signature 'randomsig-signature) by default,
;;   the user can do this in his .gnus
;; * remove unnecessary optional arguments to `find-file-noselect' to 
;;   make it work with XEmacs
;; (Thanks to Micha Wiedenmann <Micha.Wiedenmann@gmx.net> for both
;; suggestions)
;; * documentation updates

;; 2001/07/12   0.3.4
;; * more fixes for XEmacs
;; * more documentation Updates

;; 2001/07/20   0.4.0
;; * new command `randomsig-select-sig' to interactively select a signature
;; * new mode `randomsig-select-mode' (for `randomsig-select-sig')
;; * `randomsig-files' can also be function returning a list of 
;;   Signature files
;; * `randomsig-replace-sig' does not remove old signature when interrupted

;; 2001/07/22   0.4.1
;; * (require 'message) only when needed

;; 2001/08/13   0.5.0
;; * doesn't require message anymore, so it should work without gnus

;; 2001/08/20   0.5.1
;; * add (random t) to initialize random seed (thanks to Evgeny
;;   Roubinchtein <evgenyr@cs.washington.edu> for pointing this out
;; * insert a newline if it is missing at the end of a signature file

;; 2001/09/17   0.5.2
;; * new variable `randomsig-static-string' (thanks to Raymond Scholz
;;   <rscholz@zonix.de>)

;; 2001/10/01   0.5.3
;; * Documentation updates

;; 2002/01/20   0.5.99
;; * It is now possible to edit signatures before saving, or to edit 
;;   single signatures from the selection buffer.
;; * Mark many variables as user option
;; * randomsig-files-to-list works recursive

;; 2002/03/04   0.6.0
;; * `randomsig-replace-signature-in-signature-files' should be safer now
;; * `randomsig-files-to-list' did endless recursion when called 
;;   with nil. Fixed. 
;; * Some error-handling for non-existing `randomsig-dir'.

;; 2002/09/21   0.7.0
;; * most variables customizable 
;; * `randomsig-static-string' works for `randomsig-select-sig', too
;;   (thanks to Mark Trettin <mtr-dev0@gmx.de> for pointing this out)
;; * documentation updates

;; 2003/07/17   0.7.1
;; * handle errors when reading files
;; * `lambda' functions are allowed, too
;; * add customizations for hooks `randomsig-select-mode-hook' and 
;;   `randomsig-edit-mode-hook'


(eval-when-compile 
  (require 'cl))


(defconst randomsig-version "0.7.1")


(defvar randomsig-dir "~/.signatures"
  "*Directory for signature-files. See also `randomsig-files'")


(defgroup randomsig nil
  "insert a randomly choosen signature into a mail."
  :group 'mail
  :group 'news)

(defcustom randomsig-files '("default")
  "*Files with random signatures.  
This variable may be a list of strings, a string, or a function returning a
list of strings.
The files are searched in `randomsig-dir', if they don't have absolute paths.
The signatures have to be separated by lines matching
`randomsig-delimiter-pattern' at the beginning."
  :type '(choice 
	  (repeat 
	   :tag "List of filenames" 
	   (string :tag "filename"))
	  (function 
	   :tag "function returning the signature files" 
	   :value randomsig-search-sigfiles))
  :group 'randomsig)

(defcustom randomsig-delimiter "-- "
  "*delimiter used when adding new signatures in signature file.
You have to change `randomsig-delimiter-pattern', too, if you change this."
  :type '(string)
  :group 'randomsig)


(defcustom randomsig-delimiter-pattern 
  (concat "^" (regexp-quote randomsig-delimiter) "$")
  "*Regular expression that matches the delimiters between signatures. 
`randomsig-delimiter' must match `randomsig-delimiter-pattern'."
  :type '(regexp)
  :group 'randomsig)


(defcustom randomsig-search-unwanted "\\(/\\|^\\)\\(CVS\\|RCS\\|.*~\\)$"
  "*Regular expression matching unwanted files when scanning with 
`randomsig-search-sigfiles'"
  :type '(regexp)
  :group 'randomsig)


(defcustom randomsig-static-string nil
  "*Static string to be inserted above every random signature.
You probably want to have a newline at the end of it."
  :type '(choice
	  (const :tag "none" nil)
	  (string))
  :group 'randomsig)

(defcustom randomsig-select-mode-hook nil
  "*Hook run when entering randomsig select mode."
  :type 'hook
  :group 'randomsig)

(defcustom randomsig-edit-mode-hook nil
  "*Hook run when entering randomsig select mode."
  :type 'hook
  :group 'randomsig)

(defvar randomsig-buffer-name "*Signatures*"
  "Name for the (temporary) buffer for the signatures")

(defvar randomsig-edit-buffer-name "*Edit Signature*"
  "Name for the (temporary) buffer for editing the signatures")

(defvar randomsig-select-original-buffer nil)
(defvar randomsig-select-original-position nil)

(defvar randomsig-history nil)

(defvar randomsig-buffer-file-pos-list nil)

(defvar randomsig-select-edit-bufferpos nil)

(defvar randomsig-loaded-files nil)

;; definitions for XEmacs:
(unless (fboundp 'line-end-position)
  (defalias 'line-end-position 'point-at-eol))

(defun randomsig-mark-active-p ()
  (if (fboundp 'region-active-p)	

      (region-active-p)			; XEmacs

    mark-active))			; Gnu Emacs


(require 'cl)

(random t)				; Initialize random seed

;;; Helper Functions

(defun randomsig-files-to-list (files)
  ;; return a list of strings
  (cond ((and (listp files)
	      (eq (car files) 'lambda)) (randomsig-files-to-list (funcall files)))
	((listp files) files)
	((and (symbolp files)
	      (fboundp files)) (randomsig-files-to-list (funcall files)))
	((and (symbolp files)
	      (boundp files)) (randomsig-files-to-list (symbol-value files)))
	((stringp files) (list files))
	(t nil)))


(defun randomsig-prompt (&optional prompt)
  ;; Prompt for a signature file.
  (completing-read (if prompt
		       prompt
		     "signature: ")
		   (mapcar 
		    (lambda (x) (list x))
		    (randomsig-files-to-list randomsig-files))
		   nil
		   nil
		   nil
		   randomsig-history))



(defun randomsig-read-signatures-to-buffer (buffer-name &optional files) 
  ;; read the signatures into the signature buffer
  ;; save possibly local variables `randomsig-files' and `randomsig-dir'
  (let ((sigfiles randomsig-files) (sigdir randomsig-dir))
    (if (get-buffer buffer-name)
	(progn
	  (set-buffer buffer-name)
	  (setq buffer-read-only nil)
	  (delete-region (point-min) (point-max)))
      (progn
	(get-buffer-create buffer-name)
	(set-buffer buffer-name)))
    (set (make-local-variable 'randomsig-files) sigfiles)
    (set (make-local-variable 'randomsig-dir) sigdir))

  (setq randomsig-buffer-file-pos-list nil)

  (unless files
    (setq files randomsig-files))

  (setq randomsig-loaded-files files)

  ;; get a list with file names of signature files
  (let ((sigfiles (randomsig-files-to-list files)))
    ;; Insert all files into the newly created buffer
    (mapcar 
     (lambda (fname)
       
       (let ((pos (point-max)))
	 ;;(add-to-list 'randomsig-buffer-file-pos-list (cons fname pos) t)
					; this does not work with XEmacs
	 (goto-char pos)
	 (condition-case err
	     (insert-file-contents (expand-file-name fname randomsig-dir))
	   (file-error
	    (message "%s" (error-message-string err))
	    (ding)
	    (sit-for 1.0)))

	 ;; No delimiter at the beginning? Insert one.
	 (unless (string-match randomsig-delimiter-pattern
			       (buffer-substring (goto-char pos)
						 (line-end-position)))
	   (goto-char pos)
	   (insert randomsig-delimiter)
	   (insert "\n")
	   ;; Correct position...
	   (setq pos (+ pos (length randomsig-delimiter) 1)))

	 (setq randomsig-buffer-file-pos-list 
	       (append randomsig-buffer-file-pos-list 
		       (list (cons fname pos))))
	 (goto-char (point-max))
 	 (unless (and (char-before)
 		      (char-equal (char-before) ?\n)) ; Newline?
 	   (insert "\n"))))
     sigfiles)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (current-buffer)))


  
(defun randomsig-insert-signature (sig)
  ;; Insert SIG as signature in current buffer
  (save-excursion
    (goto-char (point-max))
    (insert "\n-- \n" sig)))



(defun randomsig-goto-signature ()
;; This function is stolen fom message-goto signature.
;; Go to beginnig of the signature, and return t.
;; If there is no signature in current buffer, go to end of buffer,
;; and return nil.
  (goto-char (point-min))
  (if (re-search-forward "^-- $" nil t)
      (progn 
	(forward-line 1)
	t)
    (progn
      (goto-char (point-max))
      nil)))



(defun randomsig-replace-signature (sig) 
  ;; Replace the current signature with SIG
  (save-excursion
    (when (randomsig-goto-signature)
      (forward-line -1)
      (backward-char)
      (delete-region (point) (point-max)))
    
    (randomsig-insert-signature sig)))


(defun randomsig-signature (&optional files)
  "Return a randomly choosen signature.
If FILES is non-nil, a signature out of FILES will be choosen.
Else a signature out of `randomsig-files' will be choosen."
  (save-excursion

    (randomsig-read-signatures-to-buffer randomsig-buffer-name files)

    (goto-char (point-min))
    (let '(count 0) 'selected 
	      
	 ;; Count number of signatures
	 (while (search-forward-regexp randomsig-delimiter-pattern nil t) 
	   (setq count (1+ count)))

	 ;; Select random signature out out these
	 (setq selected (1+ (random count)))
	 (goto-char (point-min))
	 (if (search-forward-regexp randomsig-delimiter-pattern nil t selected)
	     (forward-char))

	 ;; Cut signature and return it
	 (let '(here (point)) 'signature-string
	      
	      (if (not (search-forward-regexp randomsig-delimiter-pattern 
					      nil t))
		  (goto-char (point-max))
		(beginning-of-line))
	      (setq signature-string
		    (concat randomsig-static-string 
			    (buffer-substring here (point))))
	      (kill-buffer randomsig-buffer-name)
	      signature-string))))


(defun randomsig-replace-sig (arg)
  "Replace the actual signature with a new one. 
When called with prefix, read the filename of the signature-file 
that should be used"
  (interactive "P")
  (save-excursion
    
    (randomsig-replace-signature 
     (randomsig-signature
      (if arg
	  (randomsig-prompt "read from signature-lib: ")
	randomsig-files)))))



(defun randomsig-message-read-sig (arg)
  "Get the signature of current message and copy it to a file.
If mark is active, get the marked region instead.
When called with prefix, let the user edit the signature before saving"
  (interactive "P")
  (save-excursion
    (let '(signature-string
	   (if (randomsig-mark-active-p)

	       (buffer-substring (point) (mark))

	     (progn
	       (if (randomsig-goto-signature)
		   (let `(here (point))
		     (goto-char (point-max))
		     (while (char-equal (char-before) 10)
		       (backward-char))
		     (buffer-substring here (point)))
		 nil))))
      (when signature-string
	(if arg
	    (progn
	      ;; make sure this is nil...
	      (setq randomsig-select-edit-bufferpos nil)
	      (randomsig-edit signature-string))
	  (randomsig-write-signature signature-string))))))


(defun randomsig-write-signature (signature-string)
  (set-buffer (find-file-noselect
	       (expand-file-name 
		(randomsig-prompt "Write to signature-lib: ")
		randomsig-dir)))
  
  (goto-char (point-max))
  (insert (concat randomsig-delimiter "\n"))
  (insert signature-string)
  (insert "\n")
  (save-buffer))


(defun gnus/randomsig-summary-read-sig (arg)
  "Get the signature of current message and copy it to a file"
  (interactive "P")
  (progn ;save-excursion
    ;; FIXME: Doesn't return to summary buffer (save-excursion should do this)
    (gnus-summary-select-article-buffer)
    (randomsig-message-read-sig arg)))

  
(defun randomsig-search-sigfiles (&optional file)
  "Scan `randomsig-dir' and its subdirectories for regular files.
If FILE is given, only FILE and its subdirectory will be searched."
  (unless (file-exists-p randomsig-dir)
    (error "\"%s\" does not exist" randomsig-dir))
  (unless (file-directory-p randomsig-dir)
    (error "\"%s\" is not a directory" randomsig-dir))
  (unless file
    (setq file ""))

  (if (or (string-match "\\(/\\|^\\)\\(\\.\\|\\.\\.\\)$" file)
	  (string-match randomsig-search-unwanted file))
      ;; unwanted...
      nil
    
    (let '(path (expand-file-name file randomsig-dir))
      (if (file-directory-p path)
	  (mapcan (lambda (f)
		    (randomsig-search-sigfiles (if (string= file "")
						   f
						 (concat file "/" f))))
		  (directory-files path))
	(if (file-regular-p path)
	    (list file)
	  nil)))))


;;; Commands/Function for randomsig-edit-mode

(defun randomsig-edit (signature)
  (if (get-buffer randomsig-edit-buffer-name)
      (kill-buffer randomsig-edit-buffer-name))
  (switch-to-buffer (get-buffer-create randomsig-edit-buffer-name))
  (insert signature)
  (goto-char (point-min))
  (set-buffer-modified-p t)
  (setq buffer-read-only nil)
  (randomsig-edit-mode))



(defun randomsig-replace-signature-in-signature-files (signature)
  (if (not randomsig-select-edit-bufferpos)
      (error "Not in select buffer previously"))
  (set-buffer randomsig-buffer-name)
  (let* ((fname (randomsig-buffer-which-file))
	 (sig_end 
	  ;; point in selection buffer, where signature ends
	  (progn
	    (if (search-forward-regexp randomsig-delimiter-pattern nil t)
		(search-backward-regexp randomsig-delimiter-pattern nil nil))
	    (point)))
	 (sig_start 
	  ;; point in selection buffer, where signature starts
	  (progn
	    (if (search-backward-regexp randomsig-delimiter-pattern nil t)
		(progn
		  (search-forward-regexp randomsig-delimiter-pattern nil nil)
		  (forward-char)))
	    (point)))
	 (f_start 
	  ;; point in selection buffer, where signature file starts
	  (- (cdr (assoc fname randomsig-buffer-file-pos-list))
	     (point-min)))
	 ;; point in file, where Signature starts/ends
	 (f_sig_start (- sig_start f_start))
	 (f_sig_end (- sig_end f_start))
	 ;; old signature
	 (old_sig (randomsig-signature-at-point)))
    (set-buffer (find-file-noselect (expand-file-name fname randomsig-dir)))

    (if (not (string= old_sig (buffer-substring f_sig_start f_sig_end)))
	(error "Signature file has changed"))
    (delete-region f_sig_start f_sig_end)
    (goto-char f_sig_start)
    (insert signature)
    (save-buffer))
  (randomsig-select-reload))


(defun randomsig-edit-done ()
  (interactive)
  (let ((signature-string (buffer-string))
	(edit-buffer (current-buffer)))
    (if randomsig-select-edit-bufferpos
	(randomsig-replace-signature-in-signature-files signature-string)
      (randomsig-write-signature signature-string))
    (kill-buffer edit-buffer)))


(define-derived-mode randomsig-edit-mode text-mode
  "Randomsig Edit"
  "A major mode for editing signatures.
You most likely do not want to call `randomsig-edit-mode' directly.

\\{randomsig-edit-mode-map}"
  (define-key randomsig-edit-mode-map 
    (kbd "C-c C-c") 'randomsig-edit-done))


;;; Commands for randomsig-select-mode	    

(defun randomsig-select-next ()
  "Goto next signature."
  (interactive)
  (if (search-forward-regexp randomsig-delimiter-pattern nil t)
      (forward-char)))
  

(defun randomsig-select-prev ()
  "Goto next signature."
  (interactive)
  (if (search-backward-regexp randomsig-delimiter-pattern nil t 2)
      (forward-line)))


(defun randomsig-signature-at-point() 
  ;; Return the signature at current cursor position
  (save-excursion
    (if (search-backward-regexp randomsig-delimiter-pattern nil t)
	(forward-line))
    (let ((beginning (point)))
      (if (search-backward-regexp randomsig-delimiter-pattern nil t)
	  (forward-line))
      (if (not (search-forward-regexp randomsig-delimiter-pattern 
				      nil t))
	  (goto-char (point-max))
	(beginning-of-line))
      (buffer-substring beginning (point)))))


(defun randomsig-select-replace ()
  "Replace the signature in `randomsig-select-original-buffer' 
with the signature at the current position, and quit selection."
  (interactive)
  (let ((sig (randomsig-signature-at-point)))
    (kill-buffer randomsig-buffer-name)
    (switch-to-buffer randomsig-select-original-buffer)
    (randomsig-replace-signature (concat randomsig-static-string sig))
    (goto-char randomsig-select-original-position)))


(defun randomsig-select-quit ()
  "Quit the signature-buffer without selection of a signature."
  (interactive)
  (kill-buffer randomsig-buffer-name))


(defun randomsig-select-abort ()
  "Abort the selection from signature-buffer."
  (interactive)
  (ding)
  (kill-buffer randomsig-buffer-name))


(defun randomsig-select-reload ()
  "Reload the current randomsig-buffer"
  (interactive)
  (set-buffer randomsig-buffer-name)
  (let ((pos (point)))
    (randomsig-read-signatures-to-buffer randomsig-buffer-name 
					 randomsig-loaded-files)
    (goto-char pos)))


(defun randomsig-select-edit ()
  "Edit the signature at point"
  (interactive)
  (setq randomsig-select-edit-bufferpos (point))
  (randomsig-edit (randomsig-signature-at-point)))


(defun randomsig-buffer-which-file ()
  (let ((p 0)
	(fname "")
	(l randomsig-buffer-file-pos-list))
    (while (progn 
	     (setq fname (car (car l)))
	     (setq l (cdr l))
	     (setq p (cdr (car l)))
	     (and l (<= p (point)))))
    fname))


(define-derived-mode randomsig-select-mode fundamental-mode
  "Randomsig Select"
  "A major mode for selecting signatures.
You most likely do not want to call `randomsig-select-mode' directly; use
`randomsig-select-sig' instead.

\\{randomsig-select-mode-map}"

  (define-key randomsig-select-mode-map (kbd "n") 'randomsig-select-next)
  (define-key randomsig-select-mode-map (kbd "p") 'randomsig-select-prev)
  (define-key randomsig-select-mode-map (kbd "?") 'describe-mode)
  (define-key randomsig-select-mode-map (kbd "h") 'describe-mode)
  (define-key randomsig-select-mode-map (kbd "RET") 'randomsig-select-replace)
  (define-key randomsig-select-mode-map (kbd "R") 'randomsig-select-reload)
  (define-key randomsig-select-mode-map (kbd "e") 'randomsig-select-edit)
  (define-key randomsig-select-mode-map (kbd "q") 'randomsig-select-quit)
  (define-key randomsig-select-mode-map (kbd "C-g") 'randomsig-select-abort)

  ;; Experimental: show the file
  ;; FIXME: this does only work for Gnu Emacs 21
  (and (not (boundp 'xemacs-codename))
       (>= emacs-major-version 21)
       (setq mode-line-buffer-identification 
	     '(:eval (format "%-12s" 
			     (concat "[" 
				     (randomsig-buffer-which-file) 
				     "]"))))))

(defun randomsig-select-sig (arg) 
  "Select a new signature from a list.
If called with prefix argument, read the filename of the signature-file 
that should be used."
  (interactive "P")

  (setq randomsig-select-original-buffer (current-buffer))
  (setq randomsig-select-original-position (point))


  (switch-to-buffer
   (randomsig-read-signatures-to-buffer 
    randomsig-buffer-name
    (if arg 
	(randomsig-prompt "read from signature-lib: ")
      randomsig-files)))
  (goto-char 0)
  (forward-line)
  (randomsig-select-mode))



(provide 'randomsig)


;;; randomsig.el ends here
