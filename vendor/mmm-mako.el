;;; mmm-mako.el --- MMM submode class for Mako Templates
;;;     

;; Copyright (C) 2007 by Philip Jenvey

;; Author: Philip Jenvey <pjenvey@groovie.org>
;; Version: 0.2 $Id$

;;{{{ GPL

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

;;}}}

;;; Commentary:

;; Parts of this code was borrowed from mmm-myghty mode. (Thanks Ben
;; Bangert and Michael Abraham Shulman)

;; Bugs -
;; mmm classes with :back delimeters of "$" (such as Mako's ## and %)
;; will carry the :back match over to the next line when <return> is
;; used to end the line (probably because the original "$" marker is
;; moved to the next line and mmm-mode doesn't automatically update
;; the match).

;;; Code:

(require 'mmm-auto)
(require 'mmm-compat)
(require 'mmm-vars)

;;{{{ Mako Tags

(defvar mmm-mako-block-tags
  '("page" "include" "def" "namespace" "inherit" "call" "doc" "text"
    "!"))

(defvar mmm-mako-block-tags-regexp
  (mmm-regexp-opt mmm-mako-block-tags t)
  "Matches any Mako tag name after the \"<%\".")

(defun mmm-mako-verify-python-block ()
  (not (looking-at mmm-mako-block-tags-regexp)))

;;}}}
;;{{{ Add Classes

(mmm-add-group
 'mako
 `((mako-text
    :submode text-mode
    :face mmm-output-submode-face
    :front "<%text>"
    :back "</%text>"
    :insert ((?t mako-<%text> nil @ "<%text>" @ "\n" _ "\n" @
                 "</%text>" @)))
   (mako-doc
    :submode text-mode
    :face mmm-comment-submode-face
    :front "<%doc>"
    :back "</%doc>"
    :insert ((?o mako-<%doc> nil @ "<%doc>" @ "\n" _ "\n" @ "</%doc>"
                 @)))
   (mako-one-line-comment
    :submode text-mode
    :face mmm-comment-submode-face
    :front "^[ \t]*##"
    :back "$"
    :insert ((?# mako-comment nil @ "##" @ " " _ @
                 '(mmm-mako-end-line) "\n" @)))
   (mako-init
    :submode python
    :face mmm-init-submode-face
    :front "<%!"
    :back "%>"
    :insert ((?! mako-<%!-%> nil @ "<%!" @ "\n" _ "\n" @ "%>" @)))
   (mako-python
    :submode python
    :face mmm-code-submode-face
    :front "<%"
    :front-verify mmm-mako-verify-python-block
    :back "%>"
    :insert ((?% mako-<%-%> nil @ "<%" @ "\n" _ "\n" @ "%>" @)))
   (mako-python-expression
    :submode python
    :face mmm-output-submode-face
    :front "${"
    :back "}"
    :insert ((?$ mako-${-} nil @ "${" @ _ @ "}" @)))
   (mako-control
    :submode python
    :face mmm-code-submode-face
    :front "^[ \t]*%[^>]"
    :back "$"
    :insert ((tab mako-%-line nil @ "%" @ " " _ @
                  '(mmm-mako-end-line) "\n" @)))
   (mako-def
    :submode python
    :face mmm-declaration-submode-face
    :front "<%def[ \t]+name=\\([\"']\\)"
    :save-matches 1
    :back "~1[ \t]*>"
    :insert ((?d mako-<%def> nil @ "<%def name=\"" @ _ "()" @ "\">" @
                 "\n</%def>")))
   (mako-call
    :submode python
    :face mmm-output-submode-face
    :front "<%call[ \t]+expr=\\([\"']\\)"
    :save-matches 1
    :back "~1[ \t]*>"
    :insert ((?c mako-<%call> nil @ "<%call expr=\"" @ _ "()" @ "\">"
                 @ "\n</%call>")))
   (mako-page
    :submode python
    :face mmm-declaration-submode-face
    :front "<%page[ \t]+"
    :back "/>"
    :insert ((?p mako-<%page> nil @ "<%page " @ _ @ "/>" @)))
   (mako-include
    :submode text-mode
    :face mmm-output-submode-face
    :front "<%include[ \t]+file=\\([\"']\\)"
    :save-matches 1
    :back "~1[ \t]*/>"
    :insert ((?u mako-<%include> nil @ "<%include file=\"" @ _ @
                 "\"/>" @)))
   (mako-namespace
    :submode text-mode
    :face mmm-special-submode-face
    :front "<%namespace[ \t]+"
    :back "[ \t]*/>"
    :insert ((?n mako-<%namespace> nil @ "<%namespace " @ _ @ "/>"
                 @)))
   (mako-inherit
    :submode text-mode
    :face mmm-init-submode-face
    :front "<%inherit[ \t]+file=\\([\"']\\)"
    :save-matches 1
    :back "~1[ \t]*/>"
    :insert ((?i mako-<%inherit> nil @ "<%inherit file=\"" @ _ @
                 "\"/>" @)))))

;;}}}
;;{{{ One-line Sections

(defun mmm-mako-start-line ()
  (if (bolp)
      ""
    "\n"))

(defun mmm-mako-end-line ()
  (if (eolp)
      (delete-char 1)))

;;}}}
;;{{{ Set Mode Line

(defun mmm-mako-set-mode-line ()
  (setq mmm-buffer-mode-display-name "Mako"))
(add-hook 'mmm-mako-class-hook 'mmm-mako-set-mode-line)

;;}}}

(provide 'mmm-mako)

;;; mmm-mako.el ends here
