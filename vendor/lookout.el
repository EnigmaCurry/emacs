;;; lookout.el --- Read CSV formatted diary and address data -*-coding: utf-8-*-
;; Copyright (C) 2001-2006 by Ulf Jasper

;; Author:     Ulf Jasper <ulf.jasper@web.de>
;; Filename:   lookout.el
;; Created:    August 19 2001
;; Keywords:   calendar, diary, util

(defconst lookout-version "1.5" "Version number of lookout.el.")

;; Time-stamp: "28. November 2006, 17:53:52 (ulf)"
;; $Id: lookout.el,v 1.10 2006-11-28 16:54:04 ulf Exp $

;; ======================================================================

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;; ======================================================================
;;; Commentary:

;; Some calendar tools such as Outlook allow for exporting diary data as
;; CSV (Comma Separated Value) file.  This package, `lookout.el', allows
;; for importing such files into Emacs.

;; In order to import a CSV file you have to call `lookout-create-diary'.
;; When you call `lookout-create-diary' make sure that the target file
;; does not exist or contain valuable data.  The contents of the target
;; diary file are lost!

;; You have to tell `lookout-create-diary' how to interpret the CSV
;; data.  This is done by configuring the variable
;; `lookout-diary-mapping-table'.  The way how diary data are formatted is
;; determined by the variables `lookout-appointment-format' and
;; `lookout-unmarked-categories'.  You can adjust these parameters by
;; calling

;;   M-x customize-group RET lookout RET

;; In order to make things secure and work properly it is recommended to
;; direct the output of `lookout-create-diary' to a separate file, which
;; gets included from the "real" diary file.  Put the line

;;   #include ".../my-lookout-diary"

;; in your diary file.  You probably want to add the following lines to
;; your `~/.emacs':

;;   (add-hook 'list-diary-entries-hook 'include-other-diary-files)
;;   (add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

;; Here is a sample routine that shows how to use this package.
;; (defun lookout-diary-test ()
;;   "Test routine -- Don't call this! Just look."
;;   (interactive)
;;   (lookout-create-diary "~/kalender.csv"
;; 	                    "~/tmp/my-csv-diary"
;; 			    t)
;;   (save-buffer))

;; There is a similar command for importing address data into the Big
;; Brother Data Base: `lookout-create-bbdb'.  However, it has not been
;; tested very much.  It should not delete or modify existing BBDB
;; entries, but it will add lots of new user fields, one for each
;; column in the input CSV file.  Be careful!

;; ======================================================================
;;; Requirements:

;; In order to use this package you must have installed csv.el, at least
;; version 2.0, which can be found at
;; http://de.geocities.com/ulf_jasper/lisp/csv.el.txt

;; BBDB import requires BBDB 2.35 (or later).

;; ======================================================================
;;; History:

;; 1.5 (2006-11-28)
;;     Eli Napchan added lookout-diary-mapping-table-outlook-2003-english.

;; 1.4 (2003-05-16)
;;     Erik Curiel added lookout-diary-mapping-table-outlook-xp-english
;;     and several new appointment-specifiers.
;;     Made configurable settings `customize'able.
;;     Tested on Emacs 21.2.

;; 1.3 (2001-11-17)
;;     Added BBDB functions (for addresses -- untested?).
;;     Renamed functions.

;; 1.2 (2001-11-17)
;;     Yet another version.

;; 1.1 (2001-08-30)
;;     Next version.
;;     Handle periodic events.
;;     Take care of coding systems (at least for MS Dog files).
;;     Improved this and that.

;; 1.0 (2001-08-20)
;;     First version.
;;     Tested on Emacs 20.7.1 (and 21.1.102 which somehow found its
;;     way to my machine, although I'm not a pretester;), and
;;     XEmacs 21.1.12.

;; ======================================================================
;;; Todo:

;; country-specific date/time? formats

;; ======================================================================
;;; Code:

(require 'csv)
(require 'bbdb)

;; ======================================================================
;; some constants
;; ======================================================================

;; car = BBDB field, cdr = CSV columns
(defconst lookout-diary-mapping-table-outlook-german
  '(("all-day-event"      . "Ganztägiges Ereignis")
    ("categories"         . "Kategorien")
    ("description"        . "Beschreibung")
    ("end-date"           . "Endet am")
    ("end-time"           . "Endet um")
    ("location"           . "Ort")
    ("periodic-event"     . "gibtsnicht")
    ("periodicity"        . "gibtsnicht")
    ("required-attendees" . "Erforderliche Teilnehmer")
    ("optional-attendees" . "Optionale Teilnehmer")
    ("organizer"          . "Besprechungsplanung") ;; ?
    ("priority"           . "Priorität")
    ("sensitivity"        . "Vertraulichkeit") ;; ?
    ("start-date"         . "Beginnt am")
    ("start-time"         . "Beginnt um")
    ("subject"            . "Betreff"))
  "Mapping table, usable for output from German M$ Outlook.")

(defconst lookout-diary-mapping-table-outlook-xp-english
  '(("all-day-event" . "All day event")
    ("billing-information" . "Billing Information")
    ("categories"          . "Categories")
    ("description"         . "Description")
    ("end-date"            . "End Date")
    ("end-time"            . "End Time")
    ("location"            . "Location")
    ("meeting-resources"   . "Meeting Resources")
    ("mileage"             . "Mileage")
    ("optional-attendees"  . "Optional Attendees")
    ("organizer"           . "Meeting Organizer")
    ("priority"            . "Priority")
    ("private"             . "Private")
    ("reminder-date"       . "Reminder Date")
    ("reminder-on-off"     . "Reminder on/off")
    ("reminder-time"       . "Reminder Time")
    ("required-attendees"  . "Required Attendees")
    ("sensitivity"         . "Sensitivity")
    ("show-time-as"        . "Show time as")
    ("start-date"          . "Start Date")
    ("start-time"          . "Start Time")
    ("subject"             . "Subject"))
  "Mapping table, usable for output from English M$ Outlook XP.")

(defconst lookout-diary-mapping-table-outlook-2003-english
  '(("all-day-event" . "All day event")
    ("description" . "Description")
    ("end-date"    . "End Date")
    ("end-time"    . "End Time")
    ("location"    . "Location")
    ("start-date"  . "Start Date")
    ("start-time"  . "Start Time")
    ("subject"     . "Subject"))
  "Mapping table, usable for output from English M$ Outlook 2003P.")

(defconst lookout-diary-mapping-table-pocket-lookout
  '(("all-day-event"  . "AllDayEvent")
    ("categories"     . "Categories")
    ("end-date"       . "End Date")
    ("end-time"       . "End Time")
    ("location"       . "Location")
    ("notes"          . "Body")
    ("periodic-event" . "IsRecurring")
    ("periodicity"    . "Recurrence Pattern")
    ("start-date"     . "Start Date")
    ("start-time"     . "Start Time")
    ("subject"        . "Subject"))
  "Mapping table, usable for input from PocketLookout.")

(defconst lookout-month-table
  '(("Jan\\(uary?\\)?"     . "1")
    ("Feb\\(ruary?\\)?"    . "2")
    ("M[aä]r\\(ch\\|z\\)?" . "3")
    ("Apr\\(il\\)?"        . "4")
    ("Ma[iy]"              . "5")
    ("Jun[ie]?"            . "6")
    ("Jul[iy]?"            . "7")
    ("Aug\\(ust\\)?"       . "8")
    ("Sep\\(tember\\)?"    . "9")
    ("O[ck]t\\(ober\\)?"   . "10")
    ("Nov\\(ember\\)?"     . "11")
    ("De[cz]\\(ember\\)?"  . "12"))
  "Regexps for month names.")

(defconst lookout-boolean-true
  "[tT]rue"
  "Regexp for Boolean value true.
Boolean values are used e.g. for 'All Day Event', 'Reminder
on/off', and 'Private'.")

(defconst lookout-boolean-false
  "[fF]alse"
  "Regexp for Boolean value false.
Boolean values are used e.g. for 'All Day Event', 'Reminder
on/off', and 'Private'.")


;; BBDB related, value may be a list!
;; car = BBDB field, cdr = CSV columns
(defconst lookout-bbdb-mapping-table-pocket-lookout
  '(("lastname" "LastName")
    ("firstname" "FirstName")
    ("company" "CompanyName")
    ("net1" "Email1Address")
    ("phones" "FIXME")
    ("notes" "Body"))
  "Sample mapping table, usable for input from PocketLookout.")

(defconst lookout-bbdb-mapping-table-outlook-german
  '(("lastname" "Nachname" "Suffix")
    ("firstname" "Vorname" "Weitere Vornamen")
    ("company" "Firma" "Abteilung" "Position")
    ("net" "E-Mail-Adresse" "E-Mail 2: Adresse" "E-Mail 3: Adresse")
    ("phones" "Telefon geschäftlich" "Telefon geschäftlich 2"
     "Telefon privat" "Telefon privat 2" "Mobiltelefon" "Weiteres Telefon"
     "Telefon Assistent" "Fax geschäftlich" "Autotelefon" "Telefon Firma"
     "Fax privat" "Weiteres Fax" "Pager" "Haupttelefon" "Mobiltelefon 2"
     "Telefon für Hörbehinderte" "Telex")
    ("addr1" "Straße geschäftlich" "Straße geschäftlich 2"
     "Straße geschäftlich 3" "Postleitzahl geschäftlich" "Ort geschäftlich"
     "Region geschäftlich" "Land geschäftlich")
    ("addr2" "Straße privat" "Straße privat 2" "Straße privat 3"
     "Postleitzahl privat" "Ort privat" "Region privat" "Land privat")
    ("addr3" "Weitere Straße" "Weitere Straße 2" "Weitere Straße 3"
     "Weitere Postleitzahl" "Weiterer Ort" "Weitere Region" "Weiteres Land")
    ("aka" "FIXME")
    ("notes" "Notizen")
    ("otherfields"
     "Rückmeldung" "Abrechnungsinformation" "Benutzer 1"
     "Benutzer 2" "Benutzer 3" "Benutzer 4" "Beruf" "Büro"
     "Empfohlen von" "Geburtstag" "Geschlecht" "Hobby" "Initialen"
     "Internet-Frei/Gebucht" "Jahrestag" "Kategorien" "Kinder" "Konto"
     "Name Assistent" "Name des/der Vorgesetzten"
     "Organisations-Nr." "Ort" "Partner" "Postfach" "Priorität" "Privat"
     "Regierungs-Nr." "Reisekilometer" "Sprache" "Stichwörter"
     "Vertraulichkeit" "Verzeichnisserver" "Webseite"))
  "Sample mapping table, usable for input from German M$ Outlook.")

;; ======================================================================
;; customizable things
;; ======================================================================
(defgroup lookout nil
  "CSV to diary conversion."
  :group 'diary)

(defcustom lookout-diary-mapping-table
  'lookout-diary-mapping-table-outlook-xp-english
  "Defines how the input CSV file is interpreted.
Can be either one of the predefined mapping tables such as
`lookout-diary-mapping-table-outlook-xp-english' or it can be an alist
which defines translation of lookout keys to input keys.  Keys must be
strings."
  :type '(choice (const :tag "Outlook XP"
			lookout-diary-mapping-table-outlook-xp-english)
		 (const :tag "Outlook 2003 English"
			lookout-diary-mapping-table-outlook-2003-english)
		 (const :tag "Outlook (German)"
			lookout-diary-mapping-table-outlook-german)
		 (const :tag "Pocket Lookout"
			lookout-diary-mapping-table-pocket-lookout)
		 (alist :tag "Other"
			:key-type string
			:value-type string
			:value (("start-date" . "Start Date"))))
  :group 'lookout)

(defcustom lookout-unmarked-categories
  '("Ferien")
  "Unmarked Categories.
This is a list of strings which define those categories for which
respective appointments are not marked in the calendar/diary buffer."
  :type '(repeat string)
  :group 'lookout)

(defcustom lookout-appointment-format
  "%o - %s (%c, %p, %y): %l\n %d\n %u\n %v"
  "Format string defining how appointments are inserted into the diary file.
Meaning of the specifiers:
%a All-Day-Event
%c Categories
%d Description
%l Location
%o Organizer
%p Priority
%s Subject
%u Required Attendees
%v Optional Attendees
%y Sensitivity"
  :type 'string
  :group 'lookout)

(defcustom lookout-bbdb-mapping-table
  'lookout-bbdb-mapping-table-pocket-lookout
  "Alist which defines translation of lookout keys to input keys.
Keys must be strings.
Adjust this for your needs."
  :type '(choice (const :tag "Pocket Lookout"
			lookout-bbdb-mapping-table-pocket-lookout)
		 (alist :tag "Other..."
			:key-type string
			:value-type string
			:value (("lastname" . "LastName"))))

  :group 'lookout)

;; ======================================================================
;; code
;; ======================================================================

(defun lookout-create-diary (csv-file diary-file
                                      &optional forced coding-system
                                      close-all-files)
  "Convert CSV data from CSV-FILE into diary file DIARY-FILE.
Beware: Contents of the diary file will be replaced.

Optional argument FORCED if non-nil the target file will be overwritten
without asking.

Optional argument CODING-SYSTEM gives the coding system of the target diary
file.
Optional argument CLOSE-ALL-FILES if non-nil all files will be closed afterwards."
  (interactive "fCSV file containing diary information:
FEmacs diary file (will be erased!): ")
  (or forced
      (yes-or-no-p (format
		    "Warning: Contents of `%s' will be erased! Continue? "
		    diary-file))
      (error "Diary conversion cancelled"))
  (let* ((db (find-file diary-file))
	 (coding-system-for-read coding-system)
	 (b (find-file csv-file))
	 (contents (csv-parse-buffer t b coding-system)))
    (switch-to-buffer db)
    (erase-buffer)
    (mapcar (lambda (i) (lookout-insert-diary-entry i))
	    contents)
    (when close-all-files
      (save-buffer)
      (kill-buffer (current-buffer))
      (switch-to-buffer b)
      (set-buffer-modified-p nil)
      (kill-buffer b))))

(defun lookout-insert-diary-entry (line)
  "Read and convert a single CSV LINE."
  ;;(insert (format "line = %s\n" line))
  (let ((marker "")
	(start-date (lookout-diary-get-value "start-date" line))
	(start-time (lookout-diary-get-value "start-time" line))
	(end-date   (lookout-diary-get-value "end-date"   line))
	(end-time   (lookout-diary-get-value "end-time"   line))
	(categories (lookout-diary-get-value "categories" line))
	(subject    (lookout-diary-get-value "subject"    line))
	(location   (lookout-diary-get-value "location"   line))
	(organizer  (lookout-diary-get-value "organizer"  line))
	(priority   (lookout-diary-get-value "priority"   line))
	(subject    (lookout-diary-get-value "subject"    line))
	(required-attendees (lookout-diary-get-value "required-attendees"
						     line))
	(optional-attendees (lookout-diary-get-value "optional-attendees"
						     line))
	(sensitivity (lookout-diary-get-value "sensitivity"       line))
	(all-day-event (lookout-diary-get-value "all-day-event"   line))
	(periodic-event (lookout-diary-get-value "periodic-event" line))
	(periodicity (lookout-diary-get-value "periodicity"       line))
	(description (lookout-convert-description
		      (lookout-diary-get-value "description" line))))
    ;; FIXME: should split categories. This works only if categories
    ;; contains at most one value
    (if (member categories lookout-unmarked-categories)
	(setq marker "&"))
    ;; fixme: cyclic events
    (cond (;; periodic events
           (string-match lookout-boolean-true periodic-event)
	   (cond (;; daily
                  (string-equal periodicity "daily")
                  (insert (format "%s%%%%(diary-cyclic 1 %s) %s"
                                  marker
                                  (lookout-convert-date start-date)
                                  (lookout-convert-time start-time))))
                 ;; weekly
                 ((string-equal periodicity "weekly")
                  (insert (format "%s%%%%(diary-cyclic 7 %s) %s-%s"
                                  marker
                                  (lookout-convert-date start-date)
                             (lookout-convert-time start-time)
                             (lookout-convert-time end-time))))
                 ;; monthly
                 ((string-equal periodicity "monthly")
                  ;; fixme !_!_!_!__!
                  (insert (format "%s%%%%(diary-cyclic 31 %s)"
                                  marker
                                  (lookout-convert-date start-date))))
                 ;; yearly
                 ((string-equal periodicity "yearly")
                  (insert (format "%s%%%%(diary-anniversary %s)"
                                  marker
                                  (lookout-convert-date start-date))))
                 ;; unknown periodicity
                 ((message (format "unknown periodicity %s" periodicity)))))
          (;; single-day events
           (string-equal start-date end-date);; "real" single-day
           (if (string-match lookout-boolean-true all-day-event)
               (insert (format "%s%s" marker
                               (lookout-convert-date-slash start-date)))
             (insert (format "%s%s %s-%s" marker
                             (lookout-convert-date-slash start-date)
                             (lookout-convert-time start-time)
                             (lookout-convert-time end-time)))))
          (;; We try to be smart.  The CSV-generator probably forgot to
	   ;; set the all-day-event flag if the event spans from 00:00
	   ;; first day to 00:00 the next day.
           (and (= 1 (lookout-time-diff end-date start-date))
                (string= "00:00" (lookout-convert-time start-time))
                (string= "00:00" (lookout-convert-time end-time)))
           (insert (format "%s%s" marker
                           (lookout-convert-date-slash start-date))))
          ;; more-than-one-day events
          ;; Strange: apparently the date must have a blank as separator for a
          ;; diary-block entry... !?
          (t (insert (format "%s%%%%(diary-block %s %s)"
                             marker
                             (lookout-convert-date-slash start-date)
                             (lookout-convert-date-slash end-date)))))
    ;; create and insert formatted appointment
    (insert (format" %s\n" (lookout-formatted-appointment
			    subject (lookout-convert-description description)
			    categories all-day-event location organizer
			    priority required-attendees optional-attendees
			    sensitivity)))))


(defun lookout-formatted-appointment (subject
				      description categories
				      all-day-event location organizer
				      priority required-attendees
				      optional-attendees
				      sensitivity)
  "Return a formatted string of the appointment data.
The arguments SUBJECT, DESCRIPTION, CATEGORIES, ALL-DAY-EVENT, LOCATION,
ORGANIZER, PRIORITY, REQUIRED-ATTENDEES, OPTIONAL-ATTENDEES, and
SENSITIVITY are used to create a formatted appointment, as specified in
`lookout-appointment-format'."
  (let ((string lookout-appointment-format)
	(conversion-list '(("%a" . all-day-event)
			   ("%c" . categories)
			   ("%d" . description)
			   ("%s" . subject)
			   ("%l" . location)
			   ("%o" . organizer)
			   ("%p" . priority)
			   ("%s" . subject)
			   ("%u" . required-attendees)
			   ("%v" . optional-attendees)
			   ("%y" . sensitivity))))
    (mapcar (lambda (i)
	      (let ((value (eval (cdr i))))
		(setq string
		      (replace-regexp-in-string
		       (car i)
		       ;; take care of single backslashs in input strings
		       ;; imagine somebody wrote TeX expressions in the
		       ;; description!
		       (replace-regexp-in-string "\\\\" "\\\\\\\\" value)
		       string))))
	    conversion-list)
    string))

(defun lookout-diary-get-value (key entry)
  "Return the value for a KEY from a lispified CSV ENTRY.
This command uses the mapping table `lookout-diary-mapping-table'."
  (let* ((table (if (listp lookout-diary-mapping-table)
		    lookout-diary-mapping-table
		  (symbol-value lookout-diary-mapping-table)))
	 (mapped-key (cdr (assoc key table)))
	 (result (cdr (assoc mapped-key entry))))
    (unless result
      (message "Cannot find `%s' (%s) -- have you set `%s'?"
	       key mapped-key "lookout-diary-mapping-table")
      (setq result (upcase key)))
    result))

(defun lookout-convert-date (date)
  "Convert DATE to an Emacs compatible diary format."
  (let ((result date))
    (mapcar (lambda (i)
	      (setq result
		    (replace-regexp-in-string (car i) (eval (cdr i)) result)))
	    lookout-month-table)
    ;; removing leading day names
    (setq result (replace-regexp-in-string "^[a-zA-Z]*, " "" result))
    ;; replace dots by blanks
    (setq result (replace-regexp-in-string "[.]" " " result))
    ;; condense whitespace by dots
    (setq result (replace-regexp-in-string "[ \t]+" " " result))
    result))
    
(defun lookout-convert-date-slash (date)
  "Convert DATE to an Emacs compatible diary format.
Replace `/' also."
  (replace-regexp-in-string
   "/" " " (lookout-convert-date date)))

(defun lookout-convert-time (time)
  "Convert TIME to an Emacs compatible diary time format.
Remove leading and trailing whitespace as well as trailing seconds
information, if present."
  (let (result)
    (setq result (replace-regexp-in-string "^\\s-*" "" time))
    (setq result (replace-regexp-in-string "\\s-*$" "" result))
    (if (string-match "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" result)
        (replace-regexp-in-string ":[0-9][0-9]$" "" result)
      result)))

(defun lookout-time-diff (datestring1 datestring2)
  "Compute the number of days between DATESTRING1 and DATESTRING2."
  (let (s1 s2 date1 date2)
    (setq s1 (split-string (lookout-convert-date-slash datestring1)))
    (setq date1 (list (string-to-number (nth 0 s1))
                      (string-to-number (nth 1 s1))
                      (string-to-number (nth 2 s1))))
    (if european-calendar-style
        (setq date1 (list (nth 1 date1) (nth 0 date1) (nth 2 date1))))
    (setq s2 (split-string (lookout-convert-date-slash datestring2)))
    (setq date2 (list (string-to-number (nth 0 s2))
                      (string-to-number (nth 1 s2))
                      (string-to-number (nth 2 s2))))
    (if european-calendar-style
        (setq date2 (list (nth 1 date2) (nth 0 date2) (nth 2 date2))))
    (- (calendar-absolute-from-gregorian date1)
       (calendar-absolute-from-gregorian date2))))

(defun lookout-convert-description (desc)
  "Convert Outlook notes DESC to Emacs diary compatible notes."
  (replace-regexp-in-string "\n" "\n " desc))


;; There is no `replace-regexp-in-string' in Emacs 20. Here's a simplified
;; version -- hope this won't interfere with other implementations...
(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp replacement string)
  (let ((pos 0)
	(result ""))
    (while (string-match regexp string pos)
      (setq result (concat result
			   (substring string pos (match-beginning 0))
			   replacement))
      (setq pos (match-end 0)))
    (setq result (concat result (substring string pos (length string))))
    result)))
    
;; ======================================================================
;; BBDB
;; ======================================================================
(defun lookout-create-bbdb (csv-file &optional coding-system
                                     close-all-files)
  "Read data from a CSV-FILE, convert it, and add it to BBDB.
Optional parameter CODING-SYSTEM gives the coding system for reading the
csv-file.
Optional argument CLOSE-ALL-FILES if non-nil all files will be
closed afterwards."
  (interactive "fCSV file containg contacts information: ")
  (let* ((coding-system-for-read coding-system)
	 (b (find-file csv-file))
	 (contents (csv-parse-buffer t b coding-system)))
    ;; make sure that the other fields are defined
    (lookout-bbdb-create-otherfields)
    ;; go
    (mapcar (lambda (i) (lookout-bbdb-check-entry i))
	    contents)
    (switch-to-buffer ".bbdb")
    (save-buffer)
    (when close-all-files
      (switch-to-buffer b)
      (set-buffer-modified-p nil)
      (kill-buffer b))))

(defun lookout-bbdb-check-entry (entry)
  "Add ENTRY to BBDB if it doesn't exist already."
  (let* ((lastname  (lookout-bbdb-get-value "lastname" entry))
	 (firstname (lookout-bbdb-get-value "firstname" entry))
	 (company   (lookout-bbdb-get-value "company" entry))
	 (net       (lookout-bbdb-get-value "net" entry))
	 (addr1     (lookout-bbdb-get-value "addr1" entry))
	 (addr2     (lookout-bbdb-get-value "addr2" entry))
	 (addr3     (lookout-bbdb-get-value "addr3" entry))
	 (phones    (lookout-bbdb-get-value "phones" entry t)) ;; !
	 (notes     (lookout-bbdb-get-value "notes" entry))
	 (otherfields (lookout-bbdb-get-value "otherfields" entry t))
	 (addrs nil)
	 (n (concat "^" firstname " " lastname))
	 (record (bbdb-search (bbdb-records) n))
	 (message ""))
    (when record
      (if (> (length record) 1)
	  (setq message (format "Record exists and is not unique: `%s %s'"
				firstname lastname))
	(setq message (format "Record already exists: `%s %s'"
			      (bbdb-record-firstname (car record))
			      (bbdb-record-lastname (car record)))))
      (if (y-or-n-p (format "Oops.  %s\nShall I add `%s %s' anyway? "
			    message firstname lastname))
	  (setq record nil)))
    (unless record
      (if (string= company "") (setq company nil))
      (if (string= notes "") (setq notes nil))
      (if (and addr1 (> (length addr1) 0))
	  (add-to-list 'addrs (vector "Address 1" (list addr1) "" "" "" "")))
      (if (and addr2 (> (length addr2) 0))
	  (add-to-list 'addrs (vector "Address 2" (list addr2) "" "" "" "")))
      (if (and addr3 (> (length addr3) 0))
	  (add-to-list 'addrs (vector "Address 3" (list addr3) "" "" "" "")))
      (lookout-bbdb-create-entry (concat firstname " " lastname)
				 company net
				 addrs
				 phones
				 notes
				 otherfields))))

(defun lookout-bbdb-create-entry (name company net addrs phones notes
				       &optional otherfields)
  "Interactively add a BBDB entry.
The new BBDB entry consists of NAME, COMPANY, NET, ADDRS, PHONES,
NOTES, and OTHERFIELDS."
  (when (or t (y-or-n-p (format "Add %s to BBDB? " name)))
    ;;(message "Adding record to BBDB: %s" name)
    (let ((record (bbdb-create-internal name company net addrs phones notes)))
      (unless record (error "Error creating BBDB record"))
      (mapcar (lambda (i)
		(let ((field (make-symbol (aref i 0)))
		      (value (aref i 1)))
		  (when (and value (not (string= "" value)))
		    (bbdb-insert-new-field record field value))))
	      otherfields))))

(defun lookout-bbdb-get-value (key entry &optional as-vector-list)
  "Return the value for a KEY from a lispified CSV ENTRY.
If AS-VECTOR-LIST is non-nil.... FIXME"
  (let* ((table (if (listp lookout-bbdb-mapping-table)
		    lookout-bbdb-mapping-table
		  (symbol-value lookout-bbdb-mapping-table)))
	 (mapped-keys (cdr (assoc key table)))
	 (result nil)
	 (separator ""))
    (unless mapped-keys
      (error
      (format "Cannot find `%s' -- have you set `lookout-bbdb-mapping-table'?"
	      key)))
    (unless as-vector-list
      (setq result ""))
    (if (stringp mapped-keys)
	(setq mapped-keys (list mapped-keys)))
    (mapcar (lambda (i)
	      ;;(message "%s...%s" i (cdr (assoc i entry)))
	      (let ((value (cdr (assoc i entry))))
		(unless (string= "" value)
		  (if as-vector-list
		      (add-to-list 'result (vector i value))
		    (setq result (concat result separator value)))
		  (setq separator " "))))
	    mapped-keys)
    ;;(message "%s" result)
    result))

(defun lookout-bbdb-create-otherfields ()
  "Create necessary fields in BBDB."
  (let* ((table (if (listp lookout-bbdb-mapping-table)
		    lookout-bbdb-mapping-table
		  (symbol-value lookout-bbdb-mapping-table))))
    (mapcar (lambda (field)
	       (bbdb-add-new-field (make-symbol field))
	       ;;(message field)
	      )
	    (assoc "otherfields" table))))


;;(defun lookout-bbdb-test ()
;; "Test routine -- Don't call this! Just look."
;; (interactive)
;; (lookout-create-bbdb "~/mail/contacts.csv" 'undecided-dos t))

(provide 'lookout)

;;; lookout.el ends here
