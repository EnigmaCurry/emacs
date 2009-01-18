;;; buffer-butler.el -- Notify on buffer changes

;; Copyright (c) 2008 Ryan McGuire
;; Author: Ryan McGuire <ryan@enigmacurry.com>
;; Created: 2008.08.07

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;;
;; The following functions allow you to watch an idle buffer for changes
;; and notify you when it does, or to watch an actively growing buffer
;; and notify when it stops (like a compilation finishes)
;;
;; THIS VERSION IS DEPRECATED!! I should have used after-change-functions instead
;; The next version will do this.


(unless (boundp 'buffer-butlers) (setq buffer-butlers (make-hash-table :test 'equal)))

(defun buffer-butler-notify (begin end)
  (setq buffer-name "DefaultBuffer")
  (notify-desktop (format "Activity in %s" buffer-name) (format "Modified %s" (format-time-string "%b %d %I:%M %p")))
  )

(defun buffer-butler (interval &optional buffer-name)
  "Start a buffer butler on the given buffer (the current one if unspecified)"
  (interactive "nCheck interval (seconds): ")
  (unless buffer-name (setq buffer-name (buffer-name)))
  ;Check if a butler is already running
  (let ((existing-butler (gethash buffer-name buffer-butler-timers)))
    (if existing-butler
        (message (format "Butler already running for '%s', you must buffer-butler-stop first" buffer-name))
      ;Start butler
      (message (format "Butler started on '%s'" buffer-name))
      )))

(defun buffer-butler-stop (&optional buffer-name)
  "Stop the buffer-butler for the given buffer (the current one if unspecified)"
  (interactive)
  (unless buffer-name (setq buffer-name (buffer-name)))
  (let ((current-butler (gethash buffer-name buffer-butler-timers)))
    (if current-butler
        (progn
          ;Cancel butler
          (message "Butler for %s stopped." buffer-name)
          )
      (message "No Butler for %s running." buffer-name)
      )))

