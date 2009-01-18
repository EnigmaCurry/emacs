;;; growl.el --- Emacs interface to Growl via growlnotify

;; Copyright (C) 2005  Edward O'Connor

;; Author: Edward O'Connor <ted@evdb.com>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(unless (executable-find "growlnotify")
  (error "growl.el requires that you install the `growlnotify' program."))

(defsubst growl-ensure-quoted-string (arg)
  (shell-quote-argument
   (cond ((null arg) "")
         ((stringp arg) arg)
         (t (format "%s" arg)))))

(defun growl (subject message &optional sticky)
  "Notify the user of something via Growl."
  (shell-command
   (concat "growlnotify -a Emacs -n Emacs"
           (if sticky " -s" "")
           " " (growl-ensure-quoted-string subject)
           (if message
               (concat " -m" (growl-ensure-quoted-string message))
             ""))
   (get-buffer-create " *growl output*"))
  message)

(provide 'growl)
;;; growl.el ends here
