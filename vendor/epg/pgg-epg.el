;;; pgg-epg.el --- Gnus' PGG backend of EasyPG.
;; Copyright (C) 1999, 2000, 2002, 2003, 2004,
;;   2005, 2006 Free Software Foundation, Inc.
;; Copyright (C) 2006 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG, Gnus

;; This file is part of EasyPG.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use, add (setq pgg-scheme 'epg) to your ~/.gnus.

;;; Code:

(require 'epa)
(eval-when-compile (require 'pgg))

(defvar pgg-epg-secret-key-id-list nil)

(defun pgg-epg-passphrase-callback (context key-id ignore)
  (if (eq key-id 'SYM)
      (epa-passphrase-callback-function context key-id nil)
    (let* ((entry (assoc key-id epg-user-id-alist))
	   (passphrase
	    (pgg-read-passphrase
	     (format "GnuPG passphrase for %s: "
		     (if entry
			 (cdr entry)
		       key-id))
	     (if (eq key-id 'PIN)
		 "PIN"
	       key-id))))
      (when passphrase
	(pgg-add-passphrase-to-cache key-id passphrase)
	(setq pgg-epg-secret-key-id-list
	      (cons key-id pgg-epg-secret-key-id-list))
	(copy-sequence passphrase)))))

(defvar inhibit-redisplay)
(defun pgg-epg-encrypt-region (start end recipients &optional sign passphrase)
  "This function is for internal use only.

Encrypt the current region between START and END.

If optional argument SIGN is non-nil, do a combined sign and encrypt.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (let ((context (epg-make-context))
	(inhibit-redisplay t)		;Gnus users don't like flickering
	cipher recipient-keys)
    (epg-context-set-armor context t)
    (epg-context-set-textmode context pgg-text-mode)
    (epg-context-set-passphrase-callback context #'pgg-epg-passphrase-callback)
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (erase-buffer)
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (erase-buffer))
    (condition-case error
	(setq cipher
	      (epg-encrypt-string
	       context
	       (buffer-substring start end)
	       (apply #'nconc
		      (mapcar
		       (lambda (recipient)
			 (setq recipient-keys
			       (epg-list-keys context recipient))
			 (unless (or recipient-keys
				     (y-or-n-p
				      (format "No public key for %s; skip it? "
					      recipient)))
			   (error "No public key for %s" recipient))
			 recipient-keys)
		       (if pgg-encrypt-for-me
			   (cons pgg-default-user-id recipients)
			 recipients)))
	       sign t)
	      pgg-epg-secret-key-id-list nil)
      (error
       (while pgg-epg-secret-key-id-list
	 (pgg-remove-passphrase-from-cache (car pgg-epg-secret-key-id-list))
	 (setq pgg-epg-secret-key-id-list (cdr pgg-epg-secret-key-id-list)))
       (signal (car error) (cdr error))))
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (insert cipher))
    t))

(defun pgg-epg-encrypt-symmetric-region (start end &optional passphrase)
  "This function is for internal use only.

Encrypt the current region between START and END with symmetric cipher.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (pgg-epg-encrypt-region start end nil))

(defun pgg-epg-decrypt-region (start end &optional passphrase)
  "This function is for internal use only.

Decrypt the current region between START and END.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (let ((context (epg-make-context))
	(inhibit-redisplay t)		;Gnus users don't like flickering
	plain)
    (epg-context-set-armor context t)
    (epg-context-set-textmode context pgg-text-mode)
    (epg-context-set-passphrase-callback context #'pgg-epg-passphrase-callback)
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (erase-buffer)
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (erase-buffer))
    (condition-case error
	(setq plain
	      (epg-decrypt-string context (buffer-substring start end))
	      pgg-epg-secret-key-id-list nil)
      (error
       (while pgg-epg-secret-key-id-list
	 (pgg-remove-passphrase-from-cache (car pgg-epg-secret-key-id-list))
	 (setq pgg-epg-secret-key-id-list (cdr pgg-epg-secret-key-id-list)))
       (signal (car error) (cdr error))))
    (if (and pgg-text-mode
	     (fboundp 'decode-coding-string))
	(setq plain (decode-coding-string plain 'raw-text)))
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (insert plain))
    t))

(defun pgg-epg-sign-region (start end &optional cleartext passphrase)
  "This function is for internal use only.

Make detached signature from text between START and END.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (let ((context (epg-make-context))
	(inhibit-redisplay t)		;Gnus users don't like flickering
	signature)
    (epg-context-set-armor context t)
    (epg-context-set-textmode context pgg-text-mode)
    (epg-context-set-passphrase-callback context #'pgg-epg-passphrase-callback)
    (epg-context-set-signers
     context
     (list (car (epg-list-keys context pgg-default-user-id t))))
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (erase-buffer)
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (erase-buffer))
    (condition-case error
	(setq signature
	      (epg-sign-string context
			       (buffer-substring start end)
			       (if cleartext
				   'clear
				 'detached))
	      pgg-epg-secret-key-id-list nil)
      (error
       (while pgg-epg-secret-key-id-list
	 (pgg-remove-passphrase-from-cache (car pgg-epg-secret-key-id-list))
	 (setq pgg-epg-secret-key-id-list (cdr pgg-epg-secret-key-id-list)))
       (signal (car error) (cdr error))))
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (insert signature))
    t))

(defvar pgg-epg-signatures nil)

(defun pgg-epg-verify-region (start end &optional signature)
  "This function is for internal use only.

Verify region between START and END as the detached signature SIGNATURE."
  (let ((context (epg-make-context))
	(inhibit-redisplay t))		;Gnus users don't like flickering
    (epg-context-set-armor context t)
    (epg-context-set-textmode context pgg-text-mode)
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (erase-buffer)
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (erase-buffer))
    (if signature
	(epg-verify-string context
			   (with-temp-buffer
			     (insert-file-contents signature)
			     (buffer-string))
			   (buffer-substring start end))
      (epg-verify-string context (buffer-substring start end)))
    (save-excursion
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (make-local-variable 'pgg-epg-signatures)
      (setq pgg-epg-signatures (epg-context-result-for context 'verify))
      (insert (epg-verify-result-to-string pgg-epg-signatures)))
    t))

(defun pgg-epg-insert-key ()
  "This function is for internal use only.

Insert public key at point."
  (let ((context (epg-make-context))
	(inhibit-redisplay t)		;Gnus users don't like flickering
	)
    (epg-context-set-armor context t)
    (epg-context-set-textmode context pgg-text-mode)
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (erase-buffer)
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (erase-buffer))
    (insert (epg-export-keys-to-string context pgg-default-user-id))))

(defun pgg-epg-snarf-keys-region (start end)
  "This function is for internal use only.

Add all public keys in region between START and END to the keyring."
  (let ((context (epg-make-context))
	(inhibit-redisplay t)		;Gnus users don't like flickering
	)
    (epg-context-set-armor context t)
    (epg-context-set-textmode context pgg-text-mode)
    (save-excursion
      (set-buffer (get-buffer-create pgg-output-buffer))
      (erase-buffer)
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (erase-buffer))
    (epg-import-keys-from-string context (buffer-substring start end))))

(eval-when-compile
  (autoload 'mml2015-gpg-pretty-print-fpr "mml2015"))
(defun mml2015-gpg-extract-signature-details ()
  (if pgg-epg-signatures
      (let* ((expired (eq (epg-signature-status (car pgg-epg-signatures))
			  'key-expired))
	     (signer (cons (epg-signature-key-id (car pgg-epg-signatures))
			   (cdr (assoc (epg-signature-key-id
					(car pgg-epg-signatures))
				       epg-user-id-alist))))
	     (fprint (epg-signature-fingerprint (car pgg-epg-signatures)))
	     (trust-good-enough-p
	      (memq (epg-signature-validity (car pgg-epg-signatures))
		    '(marginal full ultimate))))
	(cond ((and signer fprint)
	       (concat (cdr signer)
		       (unless trust-good-enough-p
			 (concat "\nUntrusted, Fingerprint: "
				 (mml2015-gpg-pretty-print-fpr fprint)))
		       (when expired
			 (format "\nWARNING: Signature from expired key (%s)"
				 (car signer)))))
	      (t
	       "From unknown user")))
    "From unknown user"))

(defun pgg-epg-lookup-key (string &optional type)
  "Search keys associated with STRING."
  (mapcar (lambda (key)
	    (epg-sub-key-id (car (epg-key-sub-key-list key))))
	  (epg-list-keys (epg-make-context) string (not (null type)))))

(provide 'pgg-epg)

;;; pgg-epg.el ends here
