;;; doc-view.el --- View PDF/PostScript/DVI files in Emacs

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Tassilo Horn <tassilo@member.fsf.org>
;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>
;; Keywords: files, pdf, ps, dvi
;; Version: <2007-10-06 Sat 16:14>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Requirements:

;; doc-view.el requires GNU Emacs 22.1 or newer.  You also need GhostScript,
;; `dvipdfm' which comes with TeTeX and `pdftotext', which comes with poppler
;; (http://poppler.freedesktop.org/).

;;; Commentary:

;; DocView is a document viewer for Emacs.  It converts PDF, PS and DVI files
;; to a set of PNG files, one PNG for each page, and displays the PNG images
;; inside an Emacs buffer.  This buffer uses `doc-view-mode' which provides
;; convenient key bindings for browsing the document.
;;
;; To use it simply do
;;
;;     M-x doc-view RET
;;
;; and you'll be queried for a document to open.
;;
;; Since conversion may take some time all the PNG images are cached in a
;; subdirectory of `doc-view-cache-directory' and reused when you want to view
;; that file again.  This reusing can be omitted if you provide a prefx
;; argument to `doc-view'.  To delete all cached files use
;; `doc-view-clear-cache'.  To open the cache with dired, so that you can tidy
;; it out use `doc-view-dired-cache'.
;;
;; When conversion in underway the first page will be displayed as soon as it
;; is available and the available pages are refreshed every
;; `doc-view-conversion-refresh-interval' seconds.  If that variable is nil the
;; pages won't be displayed before conversion of the document finished
;; completely.
;;
;; DocView lets you select a slice of the displayed pages.  This slice will be
;; remembered and applied to all pages of the current document.  This enables
;; you to cut away the margins of a document to save some space.  To select a
;; slice you can use `doc-view-set-slice' (bound to `s s') which will query you
;; for the coordinates of the slice's top-left corner and its width and height.
;; A much more convenient way to do the same is offered by the command
;; `doc-view-set-slice-using-mouse' (bound to `s m').  After invokation you
;; only have to press mouse-1 at the top-left corner and drag it to the
;; bottom-right corner of the desired slice.  To reset the slice use
;; `doc-view-reset-slice' (bound to `s r').
;;
;; You can also search within the document.  The command `doc-view-search'
;; (bound to `C-s') queries for a search regexp and initializes a list of all
;; matching pages and messages how many match-pages were found.  After that you
;; can jump to the next page containing a match with
;; `doc-view-search-next-match' (bound to `C-S-n') or to the previous matching
;; page with `doc-view-search-previous-match' (bound to `C-S-p').  This works
;; by searching a plain text representation of the document.  If that doesn't
;; already exist the first invokation of `doc-view-search' starts the
;; conversion.  When that finishes and you're still viewing the document
;; (i.e. you didn't switch to another buffer) you're queried for the regexp
;; then.
;;
;; Dired users can simply hit `v' on a document file.  If it's a PS, PDF or DVI
;; file you're asked to open it with `doc-view' instead of `view-file'.
;;

;;; Configuration:

;; Basically doc-view should be quite usable with its standard settings, so
;; putting
;;
;;     (require 'doc-view)
;;
;; into your `user-init-file' should be enough.  If the images are too small or
;; too big you should set the "-rXXX" option in `doc-view-ghostscript-options'
;; to another value.  (The bigger your screen, the higher the value.)
;;
;; This and all other options can be set with the customization interface.
;; Simply do
;;
;;     M-x customize-group RET doc-view RET
;;
;; and modify them to your needs.

;;; Code:

(require 'dired)
(eval-when-compile (require 'cl))

;;;; Customization Options

(defgroup doc-view nil
  "In-buffer viewer for PDF, PostScript and DVI files."
  :link '(function-link doc-view)
  :version "22.2"
  :group 'applications
  :group 'multimedia
  :prefix "doc-view-")

(defcustom doc-view-ghostscript-program "gs"
  "Program to convert PS and PDF files to PNG."
  :type '(file)
  :group 'doc-view)

(defcustom doc-view-ghostscript-options
  '("-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4"
    "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET"
    "-r100")
  "A list of options to give to ghostview."
  :type '(sexp)
  :group 'doc-view)

(defcustom doc-view-dvipdfm-program "dvipdfm"
  "Program to convert DVI files to PDF.

DVI file will be converted to PDF before the resulting PDF is
converted to PNG."
  :type '(file)
  :group 'doc-view)

(defcustom doc-view-ps2pdf-program "ps2pdf"
  "Program to convert PS files to PDF.

PS files will be converted to PDF before searching is possible."
  :type '(file)
  :group 'doc-view)

(defcustom doc-view-pdftotext-program "pdftotext"
  "Program to convert PDF files to plain text.

Needed for searching."
  :type '(file)
  :group 'doc-view)

(defcustom doc-view-cache-directory (concat temporary-file-directory
					    "doc-view")
  "The base directory, where the PNG images will be saved."
  :type '(directory)
  :group 'doc-view)

(defcustom doc-view-conversion-buffer "*doc-view conversion output*"
  "The buffer where messages from the converter programs go to."
  :type '(string)
  :group 'doc-view)

(defcustom doc-view-conversion-refresh-interval 3
  "Every how much seconds the DocView buffer gets refreshed while conversion.
After such an refresh newly converted pages will be available for
viewing.  If set to nil there won't be any refreshes and the
pages won't be displayed before conversion of the whole document
has finished."
  :type '(string)
  :group 'doc-view)

;;;; Internal Variables

(defvar doc-view-current-files nil
  "Only used internally.")

(defvar doc-view-current-page nil
  "Only used internally.")

(defvar doc-view-current-doc nil
  "Only used internally.")

(defvar doc-view-current-converter-process nil
  "Only used internally.")

(defvar doc-view-current-timer nil
  "Only used internally.")

(defvar doc-view-current-slice nil
  "Only used internally.")

(defvar doc-view-current-cache-dir nil
  "Only used internally.")

(defvar doc-view-current-search-matches nil
  "Only used internally.")

(defvar doc-view-current-image nil
  "Only used internally.")

(defvar doc-view-current-info nil
  "Only used internally.")

;;;; DocView Keymap

(defvar doc-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation in the document
    (define-key map (kbd "n")         'doc-view-next-page)
    (define-key map (kbd "p")         'doc-view-previous-page)
    (define-key map (kbd "<next>")    'doc-view-next-page)
    (define-key map (kbd "<prior>")   'doc-view-previous-page)
    (define-key map (kbd "SPC")       'doc-view-scroll-up-or-next-page)
    (define-key map (kbd "DEL")       'doc-view-scroll-down-or-previous-page)
    (define-key map (kbd "M-<")       'doc-view-first-page)
    (define-key map (kbd "M->")       'doc-view-last-page)
    (define-key map (kbd "g")         'doc-view-goto-page)
    ;; Killing/burying the buffer (and the process)
    (define-key map (kbd "q")         'bury-buffer)
    (define-key map (kbd "k")         'doc-view-kill-proc-and-buffer)
    (define-key map (kbd "C-x k")     'doc-view-kill-proc-and-buffer)
    ;; Slicing the image
    (define-key map (kbd "s s")       'doc-view-set-slice)
    (define-key map (kbd "s m")       'doc-view-set-slice-using-mouse)
    (define-key map (kbd "s r")       'doc-view-reset-slice)
    ;; Searching
    (define-key map (kbd "C-s")       'doc-view-search)
    (define-key map (kbd "<find>")    'doc-view-search)
    (define-key map (kbd "C-S-n")     'doc-view-search-next-match)
    (define-key map (kbd "C-S-p")     'doc-view-search-previous-match)
    ;; Scrolling
    (define-key map (kbd "C-v")       'scroll-up)
    (define-key map (kbd "<mouse-4>") 'mwheel-scroll)
    (define-key map (kbd "<mouse-5>") 'mwheel-scroll)
    (define-key map (kbd "M-v")       'scroll-down)
    ;; Show the tooltip
    (define-key map (kbd "C-t")       'doc-view-show-tooltip)
    (suppress-keymap map)
    map)
  "Keymap used by `doc-view-mode'.")

;;;; Navigation Commands

(defun doc-view-goto-page (page)
  "View the page given by PAGE."
  (interactive "nPage: ")
  (let ((len (length doc-view-current-files)))
    (if (< page 1)
	(setq page 1)
      (when (> page len)
	(setq page len)))
    (setq doc-view-current-page page
	  doc-view-current-info
	  (concat
	   (propertize
	    (format "Page %d of %d."
		    doc-view-current-page
		    len) 'face 'bold)
	   ;; Tell user if converting isn't finished yet
	   (if doc-view-current-converter-process
	       " (still converting...)\n"
	     "\n")
	   ;; Display context infos if this page matches the last search
	   (when (and doc-view-current-search-matches
		      (assq doc-view-current-page
			    doc-view-current-search-matches))
	     (concat (propertize "Search matches:\n" 'face 'bold)
		     (let ((contexts ""))
		       (dolist (m (cdr (assq doc-view-current-page
					     doc-view-current-search-matches)))
			 (setq contexts (concat contexts "  - \"" m "\"\n")))
		       contexts)))))
    ;; Update the buffer
    (setq inhibit-read-only t)
    (erase-buffer)
    (let ((beg (point)))
      (doc-view-insert-image (nth (1- page) doc-view-current-files)
			     :pointer 'arrow)
      (put-text-property beg (point) 'help-echo doc-view-current-info))
    (insert "\n" doc-view-current-info)
    (goto-char (point-min))
    (forward-char)
    (setq inhibit-read-only nil)))

(defun doc-view-next-page (&optional arg)
  "Browse ARG pages forward."
  (interactive "p")
  (doc-view-goto-page (+ doc-view-current-page (or arg 1))))

(defun doc-view-previous-page (&optional arg)
  "Browse ARG pages backward."
  (interactive "p")
  (doc-view-goto-page (- doc-view-current-page (or arg 1))))

(defun doc-view-first-page ()
  "View the first page."
  (interactive)
  (doc-view-goto-page 1))

(defun doc-view-last-page ()
  "View the last page."
  (interactive)
  (doc-view-goto-page (length doc-view-current-files)))

(defun doc-view-scroll-up-or-next-page ()
  "Scroll page up if possible, else goto next page."
  (interactive)
  (condition-case nil
      (scroll-up)
    (error (doc-view-next-page))))

(defun doc-view-scroll-down-or-previous-page ()
  "Scroll page down if possible, else goto previous page."
  (interactive)
  (condition-case nil
      (scroll-down)
    (error (doc-view-previous-page)
	   (goto-char (point-max)))))

(defun doc-view-kill-proc-and-buffer ()
  "Kill the current converter process and buffer."
  (interactive)
  (when (eq major-mode 'doc-view-mode)
    (when doc-view-current-converter-process
      (kill-process doc-view-current-converter-process))
    (when doc-view-current-timer
      (cancel-timer doc-view-current-timer)
      (setq doc-view-current-timer nil))
    (kill-buffer (current-buffer))))

;;;; Conversion Functions

(defun doc-view-file-name-to-directory-name (file)
  "Return the directory where the png files of FILE should be saved.
It's a subdirectory of `doc-view-cache-directory'."
  (if doc-view-current-cache-dir
      doc-view-current-cache-dir
    (file-name-as-directory
     (concat (file-name-as-directory doc-view-cache-directory)
	     (with-temp-buffer
	       (insert-file-contents-literally file)
	       (md5 (current-buffer)))))))

(defun doc-view-dvi->pdf-sentinel (proc event)
  "If DVI->PDF conversion was successful, convert the PDF to PNG now."
  (if (not (string-match "finished" event))
      (message "DocView: dvi->pdf process changed status to %s." event)
    (set-buffer (process-get proc 'buffer))
    (setq doc-view-current-converter-process nil)
    (message "DocView: finished conversion from DVI to PDF!")
    ;; Now go on converting this PDF to a set of PNG files.
    (let* ((pdf (process-get proc 'pdf-file))
	   (png (concat (doc-view-file-name-to-directory-name
			 doc-view-current-doc)
			"page-%d.png")))
      (doc-view-pdf/ps->png pdf png))))

(defun doc-view-dvi->pdf (dvi pdf)
  "Convert DVI to PDF asynchrounously."
  (message "DocView: converting DVI to PDF now!")
  (setq doc-view-current-converter-process
	(start-process "doc-view-dvi->pdf" doc-view-conversion-buffer
		       doc-view-dvipdfm-program
		       "-o" pdf dvi))
  (set-process-sentinel doc-view-current-converter-process
			'doc-view-dvi->pdf-sentinel)
  (process-put doc-view-current-converter-process 'buffer   (current-buffer))
  (process-put doc-view-current-converter-process 'pdf-file pdf))

(defun doc-view-pdf/ps->png-sentinel (proc event)
  "If PDF/PS->PNG conversion was successful, update the display."
  (if (not (string-match "finished" event))
      (message "DocView: converter process changed status to %s." event)
    (set-buffer (process-get proc 'buffer))
    (setq doc-view-current-converter-process nil)
    (when doc-view-current-timer
      (cancel-timer doc-view-current-timer)
      (setq doc-view-current-timer nil))
    (message "DocView: finished conversion from PDF/PS to PNG!")
    ;; Yippie, finished.  Update the display!
    (doc-view-display doc-view-current-doc)))

(defun doc-view-pdf/ps->png (pdf-ps png)
  "Convert PDF-PS to PNG asynchrounously."
  (message "DocView: converting PDF or PS to PNG now!")
  (setq doc-view-current-converter-process
	(apply 'start-process
	       (append (list "doc-view-pdf/ps->png" doc-view-conversion-buffer
			     doc-view-ghostscript-program)
		       doc-view-ghostscript-options
		       (list (concat "-sOutputFile=" png))
		       (list pdf-ps))))
  (process-put doc-view-current-converter-process
	       'buffer (current-buffer))
  (set-process-sentinel doc-view-current-converter-process
			'doc-view-pdf/ps->png-sentinel)
  (when doc-view-conversion-refresh-interval
    (setq doc-view-current-timer
	  (run-at-time "1 secs" doc-view-conversion-refresh-interval
		       'doc-view-display
		       doc-view-current-doc))))

(defun doc-view-pdf->txt-sentinel (proc event)
  (if (not (string-match "finished" event))
      (message "DocView: converter process changed status to %s." event)
    (let ((current-buffer (current-buffer))
	  (proc-buffer    (process-get proc 'buffer)))
      (set-buffer proc-buffer)
      (setq doc-view-current-converter-process nil)
      (message "DocView: finished conversion from PDF to TXT!")
      ;; If the user looks at the DocView buffer where the conversion was
      ;; performed, search anew.  This time it will be queried for a regexp.
      (when (eq current-buffer proc-buffer)
	(doc-view-search)))))

(defun doc-view-pdf->txt (pdf txt)
  "Convert PDF to TXT asynchrounously."
  (message "DocView: converting PDF to TXT now!")
  (setq doc-view-current-converter-process
	(start-process "doc-view-pdf->txt" doc-view-conversion-buffer
		       doc-view-pdftotext-program "-raw"
		       pdf txt))
  (set-process-sentinel doc-view-current-converter-process
			'doc-view-pdf->txt-sentinel)
  (process-put doc-view-current-converter-process 'buffer (current-buffer)))

(defun doc-view-ps->pdf-sentinel (proc event)
  (if (not (string-match "finished" event))
      (message "DocView: converter process changed status to %s." event)
    (set-buffer (process-get proc 'buffer))
    (setq doc-view-current-converter-process nil)
    (message "DocView: finished conversion from PS to PDF!")
    ;; Now we can transform to plain text.
    (doc-view-pdf->txt (process-get proc 'pdf-file)
		       (concat (doc-view-file-name-to-directory-name
				doc-view-current-doc)
			       "doc.txt"))))

(defun doc-view-ps->pdf (ps pdf)
  "Convert PS to PDF asynchronously."
  (message "DocView: converting PS to PDF now!")
  (setq doc-view-current-converter-process
	(start-process "doc-view-ps->pdf" doc-view-conversion-buffer
		       doc-view-ps2pdf-program
		       ps pdf))
  (set-process-sentinel doc-view-current-converter-process
			'doc-view-ps->pdf-sentinel)
  (process-put doc-view-current-converter-process 'buffer   (current-buffer))
  (process-put doc-view-current-converter-process 'pdf-file pdf))

(defun doc-view-convert-doc (doc)
  "Convert DOC to a set of png files, one file per page.

Those files are saved in the directory given by
`doc-view-file-name-to-directory-name'."
  (clear-image-cache)
  (let* ((dir (doc-view-file-name-to-directory-name doc))
	 (png-file (concat (file-name-as-directory dir) "page-%d.png")))
    (when (file-exists-p dir)
      (dired-delete-file dir 'always))
    (make-directory dir t)
    (if (not (string= (file-name-extension doc) "dvi"))
	;; Convert to PNG images.
	(doc-view-pdf/ps->png doc png-file)
      ;; DVI files have to be converted to PDF before GhostScript can process
      ;; it.
      (doc-view-dvi->pdf doc
			 (concat (file-name-as-directory dir)
				 "doc.pdf")))))

;;;; DocView Mode

(define-derived-mode doc-view-mode nil "DocView"
  "Major mode in DocView buffers.

\\{doc-view-mode-map}"
  :group 'doc-view
  (setq buffer-read-only t)
  (make-local-variable 'doc-view-current-files)
  (make-local-variable 'doc-view-current-doc)
  (make-local-variable 'doc-view-current-image)
  (make-local-variable 'doc-view-current-page)
  (make-local-variable 'doc-view-current-converter-process)
  (make-local-variable 'doc-view-current-timer)
  (make-local-variable 'doc-view-current-slice)
  (make-local-variable 'doc-view-current-cache-dir)
  (make-local-variable 'doc-view-current-info)
  (make-local-variable 'doc-view-current-search-matches))

;;;; Slicing

(defun doc-view-set-slice (x y width height)
  "Set the slice of the images that should be displayed.
You can use this function to tell doc-view not to display the
margins of the document.  It prompts for the top-left corner (X
and Y) of the slice to display and its WIDTH and HEIGHT.

See `doc-view-set-slice-using-mouse' for a more convenient way to
do that.  To reset the slice use `doc-view-reset-slice'."
  (interactive
   (let* ((size (image-size doc-view-current-image t))
	  (a (read-number (format "Top-left X (0..%d): " (car size))))
	  (b (read-number (format "Top-left Y (0..%d): " (cdr size))))
	  (c (read-number (format "Width (0..%d): " (- (car size) a))))
	  (d (read-number (format "Height (0..%d): " (- (cdr size) b)))))
     (list a b c d)))
  (setq doc-view-current-slice (list x y width height))
  ;; Redisplay
  (doc-view-goto-page doc-view-current-page))

(defun doc-view-set-slice-using-mouse ()
  "Set the slice of the images that should be displayed.
You set the slice by pressing mouse-1 at its top-left corner and
dragging it to its bottom-right corner.  See also
`doc-view-set-slice' and `doc-view-reset-slice'."
  (interactive)
  (let (x y w h done)
    (while (not done)
      (let ((e (read-event
		(concat "Press mouse-1 at the top-left corner and "
			"drag it to the bottom-right corner!"))))
	(when (eq (car e) 'drag-mouse-1)
	  (setq x (car (posn-object-x-y (event-start e))))
	  (setq y (cdr (posn-object-x-y (event-start e))))
	  (setq w (- (car (posn-object-x-y (event-end e))) x))
	  (setq h (- (cdr (posn-object-x-y (event-end e))) y))
	  (setq done t))))
    (doc-view-set-slice x y w h)))

(defun doc-view-reset-slice ()
  "Reset the current slice.
After calling this function the whole pages will be visible
again."
  (interactive)
  (setq doc-view-current-slice nil)
  ;; Redisplay
  (doc-view-goto-page doc-view-current-page))

;;;; Display

(defun doc-view-insert-image (file &rest args)
  "Insert the given png FILE.
ARGS is a list of image descriptors."
  (let ((image (apply 'create-image file 'png nil args)))
    (setq doc-view-current-image image)
    (insert-image image (concat "[" file "]") nil doc-view-current-slice)))

(defun doc-view-sort (a b)
  "Return non-nil if A should be sorted before B.
Predicate for sorting `doc-view-current-files'."
  (if (< (length a) (length b))
      t
    (if (> (length a) (length b))
	nil
      (string< a b))))

(defun doc-view-display (doc)
  "Start viewing the document DOC."
  (let ((dir (doc-view-file-name-to-directory-name doc)))
    (set-buffer (format "*DocView: %s*" doc))
    (setq doc-view-current-files
	  (sort (directory-files dir t "page-[0-9]+\\.png" t)
		'doc-view-sort))
    (when (> (length doc-view-current-files) 0)
      (doc-view-goto-page doc-view-current-page))))

(defun doc-view-buffer-message ()
  (setq inhibit-read-only t)
  (erase-buffer)
  (insert (propertize "Welcome to DocView!" 'face 'bold)
	  "\n"
	  "
If you  see this buffer  it means that  the document you  want to
view gets  converted to PNG now  and the conversion  of the first
page           hasn't          finished           yet          or
`doc-view-conversion-refresh-interval' is set to nil.

For now these keys are useful:

`q' : Bury this buffer.  Conversion will go on in background.
`k' : Kill the conversion process and this buffer.\n")
  (setq inhibit-read-only nil))

(defun doc-view-show-tooltip ()
  (interactive)
  (tooltip-show doc-view-current-info))

;;;; Searching

(defun doc-view-search-internal (regexp file)
  "Return a list of FILE's pages that contain text matching REGEXP.
The value is an alist of the form (PAGE CONTEXTS) where PAGE is
the pagenumber and CONTEXTS are all lines of text containing a match."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((page 1)
	  (lastpage 1)
	  matches)
      (while (re-search-forward (concat "\\(?:\\([]\\)\\|\\("
					regexp "\\)\\)") nil t)
	(when (match-string 1) (incf page))
	(when (match-string 2)
	  (if (/= page lastpage)
	      (setq matches (push (cons page
					(list (buffer-substring
					       (line-beginning-position)
					       (line-end-position))))
				  matches))
	    (setq matches (cons
			   (append
			    (or
			     ;; This page already is a match.
			     (car matches)
			     ;; This is the first match on page.
			     (list page))
			    (list (buffer-substring
				   (line-beginning-position)
				   (line-end-position))))
			   (cdr matches))))
	  (setq lastpage page)))
      (nreverse matches))))

(defun doc-view-search-no-of-matches (list)
  "Extract the number of matches from the search result LIST."
  (let ((no 0))
    (dolist (p list)
      (setq no (+ no (1- (length p)))))
    no))

(defun doc-view-search ()
  "Query for a regexp and search the current document.
If the current document hasn't been transformed to plain text
till now do that first.  You should try searching anew when the
conversion finished."
  (interactive)
  ;; New search, so forget the old results.
  (setq doc-view-current-search-matches nil)
  (let ((txt (concat (doc-view-file-name-to-directory-name
		      doc-view-current-doc)
		     "doc.txt")))
    (if (file-readable-p txt)
	(progn
	  (setq doc-view-current-search-matches
		(doc-view-search-internal
		 (read-from-minibuffer "Regexp: ")
		 txt))
	  (message "DocView: search yielded %d matches."
		   (doc-view-search-no-of-matches
		    doc-view-current-search-matches)))
      ;; We must convert to TXT first!
      (if doc-view-current-converter-process
	  (message "DocView: please wait till conversion finished.")
	(let ((ext (file-name-extension doc-view-current-doc)))
	  (cond
	   ((string= ext "pdf")
	    ;; Doc is a PDF, so convert it to TXT
	    (doc-view-pdf->txt doc-view-current-doc txt))
	   ((string= ext "ps")
	    ;; Doc is a PS, so convert it to PDF (which will be converted to
	    ;; TXT thereafter).
	    (doc-view-ps->pdf doc-view-current-doc
			      (concat (doc-view-file-name-to-directory-name
				       doc-view-current-doc)
				      "doc.pdf")))
	   ((string= ext "dvi")
	    ;; Doc is a DVI.  This means that a doc.pdf already exists in its
	    ;; cache subdirectory.
	    (doc-view-pdf->txt (concat (doc-view-file-name-to-directory-name
					doc-view-current-doc)
				       "doc.pdf")
			       txt))
	   (t (error "DocView doesn't know what to do"))))))))

(defun doc-view-search-next-match (arg)
  "Go to the ARGth next matching page."
  (interactive "p")
  (let* ((next-pages (remove-if (lambda (i) (<= (car i) doc-view-current-page))
				doc-view-current-search-matches))
	 (page (car (nth (1- arg) next-pages))))
    (if page
	(doc-view-goto-page page)
      (when (and
	     doc-view-current-search-matches
	     (y-or-n-p "No more matches after current page.  Wrap to first match? "))
	(doc-view-goto-page (caar doc-view-current-search-matches))))))

(defun doc-view-search-previous-match (arg)
  "Go to the ARGth previous matching page."
  (interactive "p")
  (let* ((prev-pages (remove-if (lambda (i) (>= (car i) doc-view-current-page))
				doc-view-current-search-matches))
	 (page (car (nth (1- arg) (nreverse prev-pages)))))
    (if page
	(doc-view-goto-page page)
      (when (and
	     doc-view-current-search-matches
	     (y-or-n-p "No more matches before current page.  Wrap to last match? "))
	(doc-view-goto-page (caar (last doc-view-current-search-matches)))))))

;;;; User Interface Commands

;;;###autoload
(defun doc-view (no-cache &optional file)
  "Convert FILE to png and start viewing it.
If no FILE is given, query for on.
If this FILE is still in the cache, don't convert and use the
existing page files.  With prefix arg NO-CACHE, don't use the
cached files and convert anew."
  (interactive "P")
  (if (not (and (image-type-available-p 'png)
		(display-images-p)))
      (message "DocView: your emacs or display doesn't support png images.")
    (let* ((doc (or file
		    (expand-file-name
		     (let ((completion-ignored-extensions
			    ;; Don't hide files doc-view can display
			    (remove-if (lambda (str)
					 (string-match "\\.\\(ps\\|pdf\\|dvi\\)$"
						       str))
				       completion-ignored-extensions)))
		       (read-file-name "File: " nil nil t)))))
	   (buffer (get-buffer-create (format "*DocView: %s*" doc)))
	   (dir (doc-view-file-name-to-directory-name doc)))
      (switch-to-buffer buffer)
      (doc-view-buffer-message)
      (doc-view-mode)
      (setq doc-view-current-doc doc)
      (setq doc-view-current-page 1)
      (if (not (and (file-exists-p dir)
		    (not no-cache)))
	  (progn
	    (setq doc-view-current-cache-dir nil)
	    (doc-view-convert-doc doc-view-current-doc))
	(message "DocView: using cached files!")
	(doc-view-display doc-view-current-doc)))))

(defun doc-view-clear-cache ()
  "Delete the whole cache (`doc-view-cache-directory')."
  (interactive)
  (dired-delete-file doc-view-cache-directory 'always)
  (make-directory doc-view-cache-directory))

(defun doc-view-dired-cache ()
  "Open `dired' in `doc-view-cache-directory'."
  (interactive)
  (dired doc-view-cache-directory))

(provide 'doc-view)

;; Local Variables:
;; mode: outline-minor
;; End:

;; arch-tag: 5d6e5c5e-095f-489e-b4e4-1ca90a7d79be
;;; doc-view.el ends here
