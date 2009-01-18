;;; pair-mode.el --- insertion of paired characters

;; Copyright (C) 2004  Dave Love

;; Author: Dave Love <fx@gnu.org>
;; Keywords: convenience
;; $Revision: 1.3 $
;; URL: http://www.loveshack.ukfsn.org/emacs

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A trivial (local) minor mode, and a global version, which binds
;; suitable characters to `skeleton-pair-insert-maybe'.  Thus typing
;; `(' will normally insert `()' and put point between them.
;; Otherwise, when the region is active, it will be wrapped in the
;; parentheses.

;;; Code:

(require 'skeleton)

(defgroup pair-mode ()
  "Insertion of paired characters"
  :group 'convenience
  :group 'editing)

(defvar pair-mode-map (make-sparse-keymap))

(defcustom pair-mode-chars
  (if (boundp 'skeleton-pair-default-alist) ; Emacs 22
      (delete nil (mapcar (lambda (elt)
			    (if (= 3 (length elt))
				(car elt)))
			  skeleton-pair-default-alist))
    '(?\( ?\[ ?\{ ?\< ?`))		; Emacs 21.3's hardwired list
  "List of characters which self-insert pairs in Pair mode."
  :type '(repeat character)
  :set (lambda (symbol value)
	 (if (boundp 'pair-mode-chars)	; empty existing map
	     (dolist (c pair-mode-chars)
	       (define-key pair-mode-map (vector c) nil)))
	 (set-default symbol value)
	 (dolist (c value)		; repopulate map from new list
	   (define-key pair-mode-map (vector c) 'skeleton-pair-insert-maybe)))
  :group 'pair-mode)

(define-minor-mode pair-mode
  "Toggle Pair mode.
See `pair-mode-chars' for the characters concerned and
`skeleton-pair-insert-maybe' for the behaviour when you type one
of them."
  :group 'pair-mode
  (setq skeleton-pair pair-mode))

(define-minor-mode global-pair-mode
  "Toggle Pair mode.
See `pair-mode-chars' for the characters concerned and
`skeleton-pair-insert-maybe' for the behaviour when you type one
of them."
  nil nil pair-mode-map
  :group 'pair-mode
  :global t
  (setq-default skeleton-pair global-pair-mode))

(provide 'pair-mode)
;;; pair-mode.el ends here
