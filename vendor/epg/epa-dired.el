;;; epa-dired.el --- the EasyPG Assistant, dired extension
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

(require 'epa)
(require 'dired)

(defvar epa-dired-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "d" 'epa-dired-do-decrypt)
    (define-key keymap "v" 'epa-dired-do-verify)
    (define-key keymap "s" 'epa-dired-do-sign)
    (define-key keymap "e" 'epa-dired-do-encrypt)
    keymap))

(fset 'epa-dired-prefix epa-dired-map)

(defun epa-dired-mode-hook ()
  (define-key dired-mode-map ":" 'epa-dired-prefix))

(defun epa-dired-do-decrypt ()
  "Decrypt marked files."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (while file-list
      (epa-decrypt-file (expand-file-name (car file-list)))
      (setq file-list (cdr file-list)))
    (revert-buffer)))

(defun epa-dired-do-verify ()
  "Verify marked files."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (while file-list
      (epa-verify-file (expand-file-name (car file-list)))
      (setq file-list (cdr file-list)))))

(defun epa-dired-do-sign ()
  "Sign marked files."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (while file-list
      (epa-sign-file
       (expand-file-name (car file-list))
       (epa-select-keys (epg-make-context) "Select keys for signing.
If no one is selected, default secret key is used.  "
			nil t)
       (y-or-n-p "Make a detached signature? "))
      (setq file-list (cdr file-list)))
    (revert-buffer)))

(defun epa-dired-do-encrypt ()
  "Encrypt marked files."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (while file-list
      (epa-encrypt-file
       (expand-file-name (car file-list))
       (epa-select-keys (epg-make-context) "Select recipents for encryption.
If no one is selected, symmetric encryption will be performed.  "))
      (setq file-list (cdr file-list)))
    (revert-buffer)))

(provide 'epa-dired)

;;; epa-dired.el ends here
