;;; epa.el --- the EasyPG Assistant
;; Copyright (C) 2006 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG

;; This file is part of EasyPG.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'epg)
(require 'font-lock)
(require 'widget)
(eval-when-compile (require 'wid-edit))
(require 'derived)

(defgroup epa nil
  "The EasyPG Assistant"
  :group 'epg)

(defcustom epa-popup-info-window t
  "If non-nil, status information from epa commands is displayed on
the separate window."
  :type 'boolean
  :group 'epa)

(defcustom epa-info-window-height 5
  "Number of lines used to display status information."
  :type 'integer
  :group 'epa)

(defgroup epa-faces nil
  "Faces for epa-mode."
  :group 'epa)

(defface epa-validity-high
  `((((class color) (background dark))
     (:foreground "PaleTurquoise"
		  ,@(if (assq ':weight custom-face-attributes)
			'(:weight bold)
		      '(:bold t))))
    (t
     (,@(if (assq ':weight custom-face-attributes)
	    '(:weight bold)
	  '(:bold t)))))
  "Face used for displaying the high validity."
  :group 'epa-faces)

(defface epa-validity-medium
  `((((class color) (background dark))
     (:foreground "PaleTurquoise"
		  ,@(if (assq ':slant custom-face-attributes)
			'(:slant italic)
		      '(:italic t))))
    (t
     (,@(if (assq ':slant custom-face-attributes)
	    '(:slant italic)
	  '(:italic t)))))
  "Face used for displaying the medium validity."
  :group 'epa-faces)

(defface epa-validity-low
  `((t
     (,@(if (assq ':slant custom-face-attributes)
	    '(:slant italic)
	  '(:italic t)))))
  "Face used for displaying the low validity."
  :group 'epa-faces)

(defface epa-validity-disabled
  `((t
     (,@(if (assq ':slant custom-face-attributes)
	    '(:slant italic)
	  '(:italic t))
	:inverse-video t)))
  "Face used for displaying the disabled validity."
  :group 'epa-faces)

(defface epa-string
  '((((class color) (background dark))
     (:foreground "lightyellow"))
    (((class color) (background light))
     (:foreground "blue4")))
  "Face used for displaying the string."
  :group 'epa-faces)

(defface epa-mark
  `((((class color) (background dark))
     (:foreground "orange"
		  ,@(if (assq ':weight custom-face-attributes)
			'(:weight bold)
		      '(:bold t))))
    (((class color) (background light))
     (:foreground "red"
		  ,@(if (assq ':weight custom-face-attributes)
			'(:weight bold)
		      '(:bold t))))
    (t
     (,@(if (assq ':weight custom-face-attributes)
	    '(:weight bold)
	  '(:bold t)))))
  "Face used for displaying the high validity."
  :group 'epa-faces)

(defface epa-field-name
  `((((class color) (background dark))
     (:foreground "PaleTurquoise"
		  ,@(if (assq ':weight custom-face-attributes)
			'(:weight bold)
		      '(:bold t))))
    (t
     (,@(if (assq ':weight custom-face-attributes)
	    '(:weight bold)
	  '(:bold t)))))
  "Face for the name of the attribute field."
  :group 'epa)

(defface epa-field-body
  `((((class color) (background dark))
     (:foreground "turquoise"
		  ,@(if (assq ':slant custom-face-attributes)
			'(:slant italic)
		      '(:italic t))))
    (t
     (,@(if (assq ':slant custom-face-attributes)
	    '(:slant italic)
	  '(:italic t)))))
  "Face for the body of the attribute field."
  :group 'epa)

(defcustom epa-validity-face-alist
  '((unknown . epa-validity-disabled)
    (invalid . epa-validity-disabled)
    (disabled . epa-validity-disabled)
    (revoked . epa-validity-disabled)
    (expired . epa-validity-disabled)
    (none . epa-validity-low)
    (undefined . epa-validity-low)
    (never . epa-validity-low)
    (marginal . epa-validity-medium)
    (full . epa-validity-high)
    (ultimate . epa-validity-high))
  "An alist mapping validity values to faces."
  :type '(repeat (cons symbol face))
  :group 'epa)

(defvar epa-font-lock-keywords
  '(("^\\*"
     (0 'epa-mark))
    ("^\t\\([^\t:]+:\\)[ \t]*\\(.*\\)$"
     (1 'epa-field-name)
     (2 'epa-field-body)))
  "Default expressions to addon in epa-mode.")

(defconst epa-pubkey-algorithm-letter-alist
  '((1 . ?R)
    (2 . ?r)
    (3 . ?s)
    (16 . ?g)
    (17 . ?D)
    (20 . ?G)))

(defvar epa-protocol 'OpenPGP
  "*The default protocol.
The value can be either OpenPGP or CMS.

You should bind this variable with `let', but do not set it globally.")

(defvar epa-armor nil
  "*If non-nil, epa commands create ASCII armored output.

You should bind this variable with `let', but do not set it globally.")

(defvar epa-textmode nil
  "*If non-nil, epa commands treat input files as text.

You should bind this variable with `let', but do not set it globally.")

(defvar epa-keys-buffer nil)
(defvar epa-key-buffer-alist nil)
(defvar epa-key nil)
(defvar epa-list-keys-arguments nil)
(defvar epa-info-buffer nil)
(defvar epa-last-coding-system-specified nil)

(defvar epa-key-list-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "m" 'epa-mark-key)
    (define-key keymap "u" 'epa-unmark-key)
    (define-key keymap "d" 'epa-decrypt-file)
    (define-key keymap "v" 'epa-verify-file)
    (define-key keymap "s" 'epa-sign-file)
    (define-key keymap "e" 'epa-encrypt-file)
    (define-key keymap "r" 'epa-delete-keys)
    (define-key keymap "i" 'epa-import-keys)
    (define-key keymap "o" 'epa-export-keys)
    (define-key keymap "g" 'revert-buffer)
    (define-key keymap "n" 'next-line)
    (define-key keymap "p" 'previous-line)
    (define-key keymap " " 'scroll-up)
    (define-key keymap [delete] 'scroll-down)
    (define-key keymap "q" 'epa-exit-buffer)
    keymap))

(defvar epa-key-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'epa-exit-buffer)
    keymap))

(defvar epa-info-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'delete-window)
    keymap))

(defvar epa-exit-buffer-function #'bury-buffer)

(define-widget 'epa-key 'push-button
  "Button for representing a epg-key object."
  :format "%[%v%]"
  :button-face-get 'epa--key-widget-button-face-get
  :value-create 'epa--key-widget-value-create
  :action 'epa--key-widget-action
  :help-echo 'epa--key-widget-help-echo)

(defun epa--key-widget-action (widget &optional event)
  (epa--show-key (widget-get widget :value)))

(defun epa--key-widget-value-create (widget)
  (let* ((key (widget-get widget :value))
	 (primary-sub-key (car (epg-key-sub-key-list key)))
	 (primary-user-id (car (epg-key-user-id-list key))))
    (insert (format "%c "
		    (if (epg-sub-key-validity primary-sub-key)
			(car (rassq (epg-sub-key-validity primary-sub-key)
				    epg-key-validity-alist))
		      ? ))
	    (epg-sub-key-id primary-sub-key)
	    " "
	    (if primary-user-id
		(if (stringp (epg-user-id-string primary-user-id))
		    (epg-user-id-string primary-user-id)
		  (epg-decode-dn (epg-user-id-string primary-user-id)))
	      ""))))

(defun epa--key-widget-button-face-get (widget)
  (let ((validity (epg-sub-key-validity (car (epg-key-sub-key-list
					      (widget-get widget :value))))))
    (if validity
	(cdr (assq validity epa-validity-face-alist))
      'default)))

(defun epa--key-widget-help-echo (widget)
  (format "Show %s"
	  (epg-sub-key-id (car (epg-key-sub-key-list
				(widget-get widget :value))))))

(eval-and-compile
  (if (fboundp 'encode-coding-string)
      (defalias 'epa--encode-coding-string 'encode-coding-string)
    (defalias 'epa--encode-coding-string 'identity)))

(eval-and-compile
  (if (fboundp 'decode-coding-string)
      (defalias 'epa--decode-coding-string 'decode-coding-string)
    (defalias 'epa--decode-coding-string 'identity)))

(defun epa-key-list-mode ()
  "Major mode for `epa-list-keys'."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'epa-key-list-mode
	mode-name "Keys"
	truncate-lines t
	buffer-read-only t)
  (use-local-map epa-key-list-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(epa-font-lock-keywords t))
  ;; In XEmacs, auto-initialization of font-lock is not effective
  ;; if buffer-file-name is not set.
  (font-lock-set-defaults)
  (make-local-variable 'epa-exit-buffer-function)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'epa--key-list-revert-buffer)
  (run-hooks 'epa-key-list-mode-hook))

(defun epa-key-mode ()
  "Major mode for a key description."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'epa-key-mode
	mode-name "Key"
	truncate-lines t
	buffer-read-only t)
  (use-local-map epa-key-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(epa-font-lock-keywords t))
  ;; In XEmacs, auto-initialization of font-lock is not effective
  ;; if buffer-file-name is not set.
  (font-lock-set-defaults)
  (make-local-variable 'epa-exit-buffer-function)
  (run-hooks 'epa-key-mode-hook))

(defun epa-info-mode ()
  "Major mode for `epa-info-buffer'."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'epa-info-mode
	mode-name "Info"
	truncate-lines t
	buffer-read-only t)
  (use-local-map epa-info-mode-map)
  (run-hooks 'epa-info-mode-hook))

(defun epa-mark-key (&optional arg)
  "Mark a key on the current line.
If ARG is non-nil, unmark the key."
  (interactive "P")
  (let ((inhibit-read-only t)
	buffer-read-only
	properties)
    (beginning-of-line)
    (unless (get-text-property (point) 'epa-key)
      (error "No key on this line"))
    (setq properties (text-properties-at (point)))
    (delete-char 1)
    (insert (if arg " " "*"))
    (set-text-properties (1- (point)) (point) properties)
    (forward-line)))

(defun epa-unmark-key (&optional arg)
  "Unmark a key on the current line.
If ARG is non-nil, mark the key."
  (interactive "P")
  (epa-mark-key (not arg)))

(defun epa-exit-buffer ()
  "Exit the current buffer.
`epa-exit-buffer-function' is called if it is set."
  (interactive)
  (funcall epa-exit-buffer-function))

(defun epa--insert-keys (keys)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      (let (point)
	(while keys
	  (setq point (point))
	  (insert "  ")
	  (add-text-properties point (point)
			       (list 'epa-key (car keys)
				     'front-sticky nil
				     'rear-nonsticky t
				     'start-open t
				     'end-open t))
	  (widget-create 'epa-key :value (car keys))
	  (insert "\n")
	  (setq keys (cdr keys))))      
      (add-text-properties (point-min) (point-max)
			   (list 'epa-list-keys t
				 'front-sticky nil
				 'rear-nonsticky t
				 'start-open t
				 'end-open t)))))

(defun epa--list-keys (name secret)
  (unless (and epa-keys-buffer
	       (buffer-live-p epa-keys-buffer))
    (setq epa-keys-buffer (generate-new-buffer "*Keys*")))
  (set-buffer epa-keys-buffer)
  (epa-key-list-mode)
  (let ((inhibit-read-only t)
	buffer-read-only
	(point (point-min))
	(context (epg-make-context epa-protocol)))
    (unless (get-text-property point 'epa-list-keys)
      (setq point (next-single-property-change point 'epa-list-keys)))
    (when point
      (delete-region point
		     (or (next-single-property-change point 'epa-list-keys)
			 (point-max)))
      (goto-char point))
    (epa--insert-keys (epg-list-keys context name secret))
    (widget-setup)
    (set-keymap-parent (current-local-map) widget-keymap))
  (make-local-variable 'epa-list-keys-arguments)
  (setq epa-list-keys-arguments (list name secret))
  (goto-char (point-min))
  (pop-to-buffer (current-buffer)))

;;;###autoload
(defun epa-list-keys (&optional name)
  "List all keys matched with NAME from the public keyring."
  (interactive
   (if current-prefix-arg
       (let ((name (read-string "Pattern: "
				(if epa-list-keys-arguments
				    (car epa-list-keys-arguments)))))
	 (list (if (equal name "") nil name)))
     (list nil)))
  (epa--list-keys name nil))

;;;###autoload
(defun epa-list-secret-keys (&optional name)
  "List all keys matched with NAME from the private keyring."
  (interactive
   (if current-prefix-arg
       (let ((name (read-string "Pattern: "
				(if epa-list-keys-arguments
				    (car epa-list-keys-arguments)))))
	 (list (if (equal name "") nil name)))
     (list nil)))
  (epa--list-keys name t))

(defun epa--key-list-revert-buffer (&optional ignore-auto noconfirm)
  (apply #'epa--list-keys epa-list-keys-arguments))

(defun epa--marked-keys ()
  (or (save-excursion
	(set-buffer epa-keys-buffer)
	(goto-char (point-min))
	(let (keys key)
	  (while (re-search-forward "^\\*" nil t)
	    (if (setq key (get-text-property (match-beginning 0)
					     'epa-key))
		(setq keys (cons key keys))))
	  (nreverse keys)))
      (save-excursion
	(beginning-of-line)
	(let ((key (get-text-property (point) 'epa-key)))
	  (if key
	      (list key))))))

(defun epa--select-keys (prompt keys)
  (save-excursion
    (unless (and epa-keys-buffer
		 (buffer-live-p epa-keys-buffer))
      (setq epa-keys-buffer (generate-new-buffer "*Keys*")))
    (set-buffer epa-keys-buffer)
    (epa-key-list-mode)
    (let ((inhibit-read-only t)
	  buffer-read-only)
      (erase-buffer)
      (insert prompt "\n"
	      (substitute-command-keys "\
- `\\[epa-mark-key]' to mark a key on the line
- `\\[epa-unmark-key]' to unmark a key on the line\n"))
      (widget-create 'link
		     :notify (lambda (&rest ignore) (abort-recursive-edit))
		     :help-echo
		     (substitute-command-keys
		      "Click here or \\[abort-recursive-edit] to cancel")
		     "Cancel")
      (widget-create 'link
		     :notify (lambda (&rest ignore) (exit-recursive-edit))
		     :help-echo
		     (substitute-command-keys
		      "Click here or \\[exit-recursive-edit] to finish")
		     "OK")
      (insert "\n\n")
      (epa--insert-keys keys)
      (widget-setup)
      (set-keymap-parent (current-local-map) widget-keymap)
      (setq epa-exit-buffer-function #'abort-recursive-edit)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))
    (unwind-protect
	(progn
	  (recursive-edit)
	  (epa--marked-keys))
      (if (get-buffer-window epa-keys-buffer)
	  (delete-window (get-buffer-window epa-keys-buffer)))
      (kill-buffer epa-keys-buffer))))

;;;###autoload
(defun epa-select-keys (context prompt &optional names secret)
  "Display a user's keyring and ask him to select keys.
CONTEXT is an epg-context.
PROMPT is a string to prompt with.
NAMES is a list of strings to be matched with keys.  If it is nil, all
the keys are listed.
If SECRET is non-nil, list secret keys instead of public keys."
  (let ((keys (epg-list-keys context names secret)))
    (if (> (length keys) 1)
	(epa--select-keys prompt keys)
      keys)))

(defun epa--format-fingerprint-1 (fingerprint unit-size block-size)
  (let ((unit 0))
    (with-temp-buffer
      (insert fingerprint)
      (goto-char (point-min))
      (while (progn
	       (goto-char (+ (point) unit-size))
	       (not (eobp)))
	(setq unit (1+ unit))
	(insert (if (= (% unit block-size) 0) "  " " ")))
      (buffer-string))))

(defun epa--format-fingerprint (fingerprint)
  (if fingerprint
      (if (= (length fingerprint) 40)
	  ;; 1234 5678 9ABC DEF0 1234  5678 9ABC DEF0 1234 5678
	  (epa--format-fingerprint-1 fingerprint 4 5)
	;; 12 34 56 78 9A BC DE F0  12 34 56 78 9A BC DE F0
	(epa--format-fingerprint-1 fingerprint 2 8))))

(defun epa--show-key (key)
  (let* ((primary-sub-key (car (epg-key-sub-key-list key)))
	 (entry (assoc (epg-sub-key-id primary-sub-key)
		       epa-key-buffer-alist))
	 (inhibit-read-only t)
	 buffer-read-only
	 pointer)
    (unless entry
      (setq entry (cons (epg-sub-key-id primary-sub-key) nil)
	    epa-key-buffer-alist (cons entry epa-key-buffer-alist)))
    (unless (and (cdr entry)
		 (buffer-live-p (cdr entry)))
      (setcdr entry (generate-new-buffer
		     (format "*Key*%s" (epg-sub-key-id primary-sub-key)))))
    (set-buffer (cdr entry))
    (epa-key-mode)
    (make-local-variable 'epa-key)
    (setq epa-key key)
    (erase-buffer)
    (setq pointer (epg-key-user-id-list key))
    (while pointer
      (if (car pointer)
	  (insert " "
		  (if (epg-user-id-validity (car pointer))
		      (char-to-string
		       (car (rassq (epg-user-id-validity (car pointer))
				   epg-key-validity-alist)))
		    " ")
		  " "
		  (if (stringp (epg-user-id-string (car pointer)))
		      (epg-user-id-string (car pointer))
		    (epg-decode-dn (epg-user-id-string (car pointer))))
		  "\n"))
      (setq pointer (cdr pointer)))
    (setq pointer (epg-key-sub-key-list key))
    (while pointer
      (insert " "
	      (if (epg-sub-key-validity (car pointer))
		  (char-to-string
		   (car (rassq (epg-sub-key-validity (car pointer))
			       epg-key-validity-alist)))
		" ")
	      " "
	      (epg-sub-key-id (car pointer))
	      " "
	      (format "%dbits"
		      (epg-sub-key-length (car pointer)))
	      " "
	      (cdr (assq (epg-sub-key-algorithm (car pointer))
			 epg-pubkey-algorithm-alist))
	      "\n\tCreated: "
	      (condition-case nil
		  (format-time-string "%Y-%m-%d"
				      (epg-sub-key-creation-time (car pointer)))
		(error "????-??-??"))
	      (if (epg-sub-key-expiration-time (car pointer))
		  (format "\n\tExpires: %s"
			  (condition-case nil
			      (format-time-string "%Y-%m-%d"
						  (epg-sub-key-expiration-time
						   (car pointer)))
			    (error "????-??-??")))
		"")
	      "\n\tCapabilities: "
	      (mapconcat #'symbol-name
			 (epg-sub-key-capability (car pointer))
			 " ")
	      "\n\tFingerprint: "
	      (epa--format-fingerprint (epg-sub-key-fingerprint (car pointer)))
	      "\n")
      (setq pointer (cdr pointer)))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun epa-display-info (info)
  (if epa-popup-info-window
      (save-selected-window
	(unless (and epa-info-buffer (buffer-live-p epa-info-buffer))
	  (setq epa-info-buffer (generate-new-buffer "*Info*")))
	(if (get-buffer-window epa-info-buffer)
	    (delete-window (get-buffer-window epa-info-buffer)))
	(save-excursion
	  (set-buffer epa-info-buffer)
	  (let ((inhibit-read-only t)
		buffer-read-only)
	    (erase-buffer)
	    (insert info))
	  (epa-info-mode)
	  (goto-char (point-min)))
	(if (> (window-height)
	       epa-info-window-height)
	    (set-window-buffer (split-window nil (- (window-height)
						    epa-info-window-height))
			       epa-info-buffer)
	  (pop-to-buffer epa-info-buffer)
	  (if (> (window-height) epa-info-window-height)
	      (shrink-window (- (window-height) epa-info-window-height)))))
    (message "%s" info)))

(defun epa-display-verify-result (verify-result)
  (epa-display-info (epg-verify-result-to-string verify-result)))
(make-obsolete 'epa-display-verify-result 'epa-display-info)

(defun epa-passphrase-callback-function (context key-id handback)
  (if (eq key-id 'SYM)
      (read-passwd "Passphrase for symmetric encryption: "
		   (eq (epg-context-operation context) 'encrypt))
    (read-passwd
     (if (eq key-id 'PIN)
	"Passphrase for PIN: "
       (let ((entry (assoc key-id epg-user-id-alist)))
	 (if entry
	     (format "Passphrase for %s %s: " key-id (cdr entry))
	   (format "Passphrase for %s: " key-id)))))))

(defun epa-progress-callback-function (context what char current total
					       handback)
  (message "%s%d%% (%d/%d)" (or handback
				(concat what ": "))
	   (if (> total 0) (floor (* (/ current (float total)) 100)) 0)
	   current total))

;;;###autoload
(defun epa-decrypt-file (file)
  "Decrypt FILE."
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  (let* ((default-name (file-name-sans-extension file))
	 (plain (expand-file-name
		 (read-file-name
		  (concat "To file (default "
			  (file-name-nondirectory default-name)
			  ") ")
		  (file-name-directory default-name)
		  default-name)))
	 (context (epg-make-context epa-protocol)))
    (epg-context-set-passphrase-callback context
					 #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       #'epa-progress-callback-function
				       (format "Decrypting %s..."
					       (file-name-nondirectory file)))
    (message "Decrypting %s..." (file-name-nondirectory file))
    (epg-decrypt-file context file plain)
    (message "Decrypting %s...wrote %s" (file-name-nondirectory file)
	     (file-name-nondirectory plain))
    (if (epg-context-result-for context 'verify)
	(epa-display-info (epg-verify-result-to-string
			   (epg-context-result-for context 'verify))))))

;;;###autoload
(defun epa-verify-file (file)
  "Verify FILE."
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  (let* ((context (epg-make-context epa-protocol))
	 (plain (if (equal (file-name-extension file) "sig")
		    (file-name-sans-extension file))))
    (epg-context-set-progress-callback context
				       #'epa-progress-callback-function
				       (format "Verifying %s..."
					       (file-name-nondirectory file)))
    (message "Verifying %s..." (file-name-nondirectory file))
    (epg-verify-file context file plain)
    (message "Verifying %s...done" (file-name-nondirectory file))
    (if (epg-context-result-for context 'verify)
	(epa-display-info (epg-verify-result-to-string
			   (epg-context-result-for context 'verify))))))

(defun epa--read-signature-type ()
  (let (type c)
    (while (null type)
      (message "Signature type (n,c,d,?) ")
      (setq c (read-char))
      (cond ((eq c ?c)
	     (setq type 'clear))
	    ((eq c ?d)
	     (setq type 'detached))
	    ((eq c ??)
	     (with-output-to-temp-buffer "*Help*"
	       (save-excursion
		 (set-buffer standard-output)
		 (insert "\
n - Create a normal signature
c - Create a cleartext signature
d - Create a detached signature
? - Show this help
"))))
	    (t
	     (setq type 'normal))))))

;;;###autoload
(defun epa-sign-file (file signers mode)
  "Sign FILE by SIGNERS keys selected."
  (interactive
   (let ((verbose current-prefix-arg))
     (list (expand-file-name (read-file-name "File: "))
	   (if verbose
	       (epa-select-keys (epg-make-context epa-protocol)
				"Select keys for signing.
If no one is selected, default secret key is used.  "
				nil t))
	   (if verbose
	       (epa--read-signature-type)
	     'clear))))
  (let ((signature (concat file
			   (if (eq epa-protocol 'OpenPGP)
			       (if (or epa-armor
				       (not (memq mode
						  '(nil t normal detached))))
				   ".asc"
				 (if (memq mode '(t detached))
				     ".sig"
				   ".gpg"))
			     (if (memq mode '(t detached))
				 ".p7s"
			       ".p7m"))))
	(context (epg-make-context epa-protocol)))
    (epg-context-set-armor context epa-armor)
    (epg-context-set-textmode context epa-textmode)
    (epg-context-set-signers context signers)
    (epg-context-set-passphrase-callback context
					 #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       #'epa-progress-callback-function
				       (format "Signing %s..."
					       (file-name-nondirectory file)))
    (message "Signing %s..." (file-name-nondirectory file))
    (epg-sign-file context file signature mode)
    (message "Signing %s...wrote %s" (file-name-nondirectory file)
	     (file-name-nondirectory signature))))

;;;###autoload
(defun epa-encrypt-file (file recipients)
  "Encrypt FILE for RECIPIENTS."
  (interactive
   (list (expand-file-name (read-file-name "File: "))
	 (epa-select-keys (epg-make-context epa-protocol)
			  "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  ")))
  (let ((cipher (concat file (if (eq epa-protocol 'OpenPGP)
				 (if epa-armor ".asc" ".gpg")
			       ".p7m")))
	(context (epg-make-context epa-protocol)))
    (epg-context-set-armor context epa-armor)
    (epg-context-set-textmode context epa-textmode)
    (epg-context-set-passphrase-callback context
					 #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       #'epa-progress-callback-function
				       (format "Encrypting %s..."
					       (file-name-nondirectory file)))
    (message "Encrypting %s..." (file-name-nondirectory file))
    (epg-encrypt-file context file recipients cipher)
    (message "Encrypting %s...wrote %s" (file-name-nondirectory file)
	     (file-name-nondirectory cipher))))

;;;###autoload
(defun epa-decrypt-region (start end)
  "Decrypt the current region between START and END.

Don't use this command in Lisp programs!"
  (interactive "r")
  (save-excursion
    (let ((context (epg-make-context epa-protocol))
	  plain)
      (epg-context-set-passphrase-callback context
					   #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback context
					 #'epa-progress-callback-function
					 "Decrypting...")
      (message "Decrypting...")
      (setq plain (epg-decrypt-string context (buffer-substring start end)))
      (message "Decrypting...done")
      (setq plain (epa--decode-coding-string
		   plain
		   (or coding-system-for-read
		       (get-text-property start 'epa-coding-system-used))))
      (if (y-or-n-p "Replace the original text? ")
	  (let ((inhibit-read-only t)
		buffer-read-only)
	    (delete-region start end)
	    (goto-char start)
	    (insert plain))
	(with-output-to-temp-buffer "*Temp*"
	  (set-buffer standard-output)
	  (insert plain)
	  (epa-info-mode)))
      (if (epg-context-result-for context 'verify)
	  (epa-display-info (epg-verify-result-to-string
			     (epg-context-result-for context 'verify)))))))

(defun epa--find-coding-system-for-mime-charset (mime-charset)
  (if (featurep 'xemacs)
      (if (fboundp 'find-coding-system)
	  (find-coding-system mime-charset))
    (let ((pointer (coding-system-list)))
      (while (and pointer
		  (eq (coding-system-get (car pointer) 'mime-charset)
		      mime-charset))
	(setq pointer (cdr pointer)))
      pointer)))

;;;###autoload
(defun epa-decrypt-armor-in-region (start end)
  "Decrypt OpenPGP armors in the current region between START and END.

Don't use this command in Lisp programs!"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (armor-start armor-end)
	(while (re-search-forward "-----BEGIN PGP MESSAGE-----$" nil t)
	  (setq armor-start (match-beginning 0)
		armor-end (re-search-forward "^-----END PGP MESSAGE-----$"
					     nil t))
	  (unless armor-end
	    (error "No armor tail"))
	  (goto-char armor-start)
	  (let ((coding-system-for-read
		 (or coding-system-for-read
		     (if (re-search-forward "^Charset: \\(.*\\)" armor-end t)
			 (epa--find-coding-system-for-mime-charset
			  (intern (downcase (match-string 1))))))))
	    (goto-char armor-end)
	    (epa-decrypt-region armor-start armor-end)))))))

;;;###autoload
(defun epa-verify-region (start end)
  "Verify the current region between START and END.

Don't use this command in Lisp programs!"
  (interactive "r")
  (let ((context (epg-make-context epa-protocol))
	plain)
    (epg-context-set-progress-callback context
				       #'epa-progress-callback-function
				       "Verifying...")
    (message "Verifying...")
    (setq plain (epg-verify-string
		 context
		 (epa--encode-coding-string
		  (buffer-substring start end)
		  (or coding-system-for-write
		      (get-text-property start 'epa-coding-system-used)))))
    (message "Verifying...done")
    (setq plain (epa--decode-coding-string
		 plain
		 (or coding-system-for-read
		     (get-text-property start 'epa-coding-system-used))))
    (if (y-or-n-p "Replace the original text? ")
	(let ((inhibit-read-only t)
	      buffer-read-only)
	  (delete-region start end)
	  (goto-char start)
	  (insert plain))
      (with-output-to-temp-buffer "*Temp*"
	(set-buffer standard-output)
	(insert plain)
	(epa-info-mode)))
    (if (epg-context-result-for context 'verify)
	(epa-display-info (epg-verify-result-to-string
			   (epg-context-result-for context 'verify))))))

;;;###autoload
(defun epa-verify-cleartext-in-region (start end)
  "Verify OpenPGP cleartext signed messages in the current region
between START and END.

Don't use this command in Lisp programs!"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (cleartext-start cleartext-end)
	(while (re-search-forward "-----BEGIN PGP SIGNED MESSAGE-----$"
				  nil t)
	  (setq cleartext-start (match-beginning 0))
	  (unless (re-search-forward "^-----BEGIN PGP SIGNATURE-----$"
					   nil t)
	    (error "Invalid cleartext signed message"))
	  (setq cleartext-end (re-search-forward
			   "^-----END PGP SIGNATURE-----$"
			   nil t))
	  (unless cleartext-end
	    (error "No cleartext tail"))
	  (epa-verify-region cleartext-start cleartext-end))))))

(eval-and-compile
  (if (fboundp 'select-safe-coding-system)
      (defalias 'epa--select-safe-coding-system 'select-safe-coding-system)
    (defun epa--select-safe-coding-system (from to)
      buffer-file-coding-system)))

;;;###autoload
(defun epa-sign-region (start end signers mode)
  "Sign the current region between START and END by SIGNERS keys selected.

Don't use this command in Lisp programs!"
  (interactive
   (let ((verbose current-prefix-arg))
     (setq epa-last-coding-system-specified
	   (or coding-system-for-write
	       (epa--select-safe-coding-system
		(region-beginning) (region-end))))
     (list (region-beginning) (region-end)
	   (if verbose
	       (epa-select-keys (epg-make-context epa-protocol)
				"Select keys for signing.
If no one is selected, default secret key is used.  "
				nil t))
	   (if verbose
	       (epa--read-signature-type)
	     'clear))))
  (save-excursion
    (let ((context (epg-make-context epa-protocol))
	  signature)
      ;;(epg-context-set-armor context epa-armor)
      (epg-context-set-armor context t)
      ;;(epg-context-set-textmode context epa-textmode)
      (epg-context-set-textmode context t)
      (epg-context-set-signers context signers)
      (epg-context-set-passphrase-callback context
					   #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback context
					 #'epa-progress-callback-function
					 "Signing...")
      (message "Signing...")
      (setq signature (epg-sign-string context
				       (epa--encode-coding-string
					(buffer-substring start end)
					epa-last-coding-system-specified)
				       mode))
      (message "Signing...done")
      (delete-region start end)
      (goto-char start)
      (add-text-properties (point)
			   (progn
			     (insert (epa--decode-coding-string
				      signature
				      (or coding-system-for-read
					  epa-last-coding-system-specified)))
			     (point))
			   (list 'epa-coding-system-used
				 epa-last-coding-system-specified
				 'front-sticky nil
				 'rear-nonsticky t
				 'start-open t
				 'end-open t)))))

(eval-and-compile
  (if (fboundp 'derived-mode-p)
      (defalias 'epa--derived-mode-p 'derived-mode-p)
    (defun epa--derived-mode-p (&rest modes)
      "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
      (let ((parent major-mode))
	(while (and (not (memq parent modes))
		    (setq parent (get parent 'derived-mode-parent))))
	parent))))

;;;###autoload
(defun epa-encrypt-region (start end recipients sign signers)
  "Encrypt the current region between START and END for RECIPIENTS.

Don't use this command in Lisp programs!"
  (interactive
   (let ((verbose current-prefix-arg)
	 (context (epg-make-context epa-protocol))
	 sign)
     (setq epa-last-coding-system-specified
	   (or coding-system-for-write
	       (epa--select-safe-coding-system
		(region-beginning) (region-end))))
     (list (region-beginning) (region-end)
	   (epa-select-keys context
			    "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  ")
	   (setq sign (if verbose (y-or-n-p "Sign? ")))
	   (if sign
	       (epa-select-keys context
				"Select keys for signing.  ")))))
  (save-excursion
    (let ((context (epg-make-context epa-protocol))
	  cipher)
      ;;(epg-context-set-armor context epa-armor)
      (epg-context-set-armor context t)
      ;;(epg-context-set-textmode context epa-textmode)
      (epg-context-set-textmode context t)
      (if sign
	  (epg-context-set-signers context signers))
      (epg-context-set-passphrase-callback context
					   #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback context
					 #'epa-progress-callback-function
					 "Encrypting...")
      (message "Encrypting...")
      (setq cipher (epg-encrypt-string context
				       (epa--encode-coding-string
					(buffer-substring start end)
					epa-last-coding-system-specified)
				       recipients
				       sign))
      (message "Encrypting...done")
      (delete-region start end)
      (goto-char start)
      (add-text-properties (point)
			   (progn
			     (insert cipher)
			     (point))
			   (list 'epa-coding-system-used
				 epa-last-coding-system-specified
				 'front-sticky nil
				 'rear-nonsticky t
				 'start-open t
				 'end-open t)))))

;;;###autoload
(defun epa-delete-keys (keys &optional allow-secret)
  "Delete selected KEYS.

Don't use this command in Lisp programs!"
  (interactive
   (let ((keys (epa--marked-keys)))
     (unless keys
       (error "No keys selected"))
     (list keys
	   (eq (nth 1 epa-list-keys-arguments) t))))
  (let ((context (epg-make-context epa-protocol)))
    (message "Deleting...")
    (epg-delete-keys context keys allow-secret)
    (message "Deleting...done")
    (apply #'epa-list-keys epa-list-keys-arguments)))

;;;###autoload
(defun epa-import-keys (file)
  "Import keys from FILE.

Don't use this command in Lisp programs!"
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  (let ((context (epg-make-context epa-protocol)))
    (message "Importing %s..." (file-name-nondirectory file))
    (condition-case nil
	(progn
	  (epg-import-keys-from-file context file)
	  (message "Importing %s...done" (file-name-nondirectory file)))
      (error
       (message "Importing %s...failed" (file-name-nondirectory file))))
    (if (epg-context-result-for context 'import)
	(epa-display-info (epg-import-result-to-string
			   (epg-context-result-for context 'import))))
    (if (eq major-mode 'epa-key-list-mode)
	(apply #'epa-list-keys epa-list-keys-arguments))))

;;;###autoload
(defun epa-import-keys-region (start end)
  "Import keys from the region.

Don't use this command in Lisp programs!"
  (interactive "r")
  (let ((context (epg-make-context epa-protocol)))
    (message "Importing...")
    (condition-case nil
	(progn
	  (epg-import-keys-from-string context (buffer-substring start end))
	  (message "Importing...done"))
      (error
       (message "Importing...failed")))
    (if (epg-context-result-for context 'import)
	(epa-display-info (epg-import-result-to-string
			   (epg-context-result-for context 'import))))))

;;;###autoload
(defun epa-import-armor-in-region (start end)
  "Import keys in the OpenPGP armor format in the current region
between START and END.

Don't use this command in Lisp programs!"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (armor-start armor-end)
	(while (re-search-forward
		"-----BEGIN \\(PGP \\(PUBLIC\\|PRIVATE\\) KEY BLOCK\\)-----$"
		nil t)
	  (setq armor-start (match-beginning 0)
		armor-end (re-search-forward
			   (concat "^-----END " (match-string 1) "-----$")
			   nil t))
	  (unless armor-end
	    (error "No armor tail"))
	  (epa-import-keys-region armor-start armor-end))))))

;;;###autoload
(defun epa-export-keys (keys file)
  "Export selected KEYS to FILE.

Don't use this command in Lisp programs!"
  (interactive
   (let ((keys (epa--marked-keys))
	 default-name)
     (unless keys
       (error "No keys selected"))
     (setq default-name
	   (expand-file-name
	    (concat (epg-sub-key-id (car (epg-key-sub-key-list (car keys))))
		    (if epa-armor ".asc" ".gpg"))
	    default-directory))
     (list keys
	   (expand-file-name
	    (read-file-name
	     (concat "To file (default "
		     (file-name-nondirectory default-name)
		     ") ")
	     (file-name-directory default-name)
	     default-name)))))
  (let ((context (epg-make-context epa-protocol)))
    (epg-context-set-armor context epa-armor)
    (message "Exporting to %s..." (file-name-nondirectory file))
    (epg-export-keys-to-file context keys file)
    (message "Exporting to %s...done" (file-name-nondirectory file))))

;;;###autoload
(defun epa-insert-keys (keys)
  "Insert selected KEYS after the point.

Don't use this command in Lisp programs!"
  (interactive
   (list (epa-select-keys (epg-make-context epa-protocol)
			  "Select keys to export.  ")))
  (let ((context (epg-make-context epa-protocol)))
    ;;(epg-context-set-armor context epa-armor)
    (epg-context-set-armor context t)
    (insert (epg-export-keys-to-string context keys))))

;;;###autoload
(defun epa-sign-keys (keys &optional local)
  "Sign selected KEYS.
If a prefix-arg is specified, the signature is marked as non exportable.

Don't use this command in Lisp programs!"
  (interactive
   (let ((keys (epa--marked-keys)))
     (unless keys
       (error "No keys selected"))
     (list keys current-prefix-arg)))
  (let ((context (epg-make-context epa-protocol)))
    (epg-context-set-passphrase-callback context
					 #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       #'epa-progress-callback-function
				       "Signing keys...")
    (message "Signing keys...")
    (epg-sign-keys context keys local)
    (message "Signing keys...done")))
(make-obsolete 'epa-sign-keys "Do not use.")

(provide 'epa)

;;; epa.el ends here
