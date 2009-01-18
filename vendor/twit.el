;;; Twit.el --- interface with twitter.com

;; Copyright (c) 2007 Theron Tlax
;;           (c) 2008 Jonathan Arkell
;; Time-stamp: <2007-03-19 18:33:17 thorne>
;; Author: thorne <thorne@timbral.net>, jonnay <jonnay@jonnay.net>
;; Created: 2007.3.16
;; Keywords: comm
;; Favorite Poet: E. E. Cummings

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

;; This is the beginnings of a library for interfacing with
;; twitter.com from Emacs.  It is also (more importantly) some
;; interactive functions that use that library.  It's a hack, of
;; course; RMS i am not.  Maybe one of you real programmers would
;; like to clean it up?

;; This uses Twitter's XML-based api, not the JSON one because i
;; would like to avoid making the user install third-party libraries
;; to use it.

;;  Use:

;;			      FOR POSTING

;; There are four main interactive functions:

;;   M-x twit-post RET will prompt for you to type your post directly
;;   in the minibuffer.

;;   M-x twit-post-region RET will post the region and

;;   M-x twit-post-buffer RET will post the entire contents of the
;;   current buffer.

;;   M-X twit-show-recent-tweets RET will create a new buffer and 
;;   show your most recent messages in it.

;;   M-x twit-mode RET, if you want to bother, just binds the
;;   interactive functions to some keys.  Do C-h f RET twit-mode RET
;;   for more info.

;;   M-x twit-follow-recent-tweets RET will create a new buffer,
;;   show the most recent tweets, and update it every 5 mins.

;; But remember that your posts can't be longer than 140 characters
;; long.  All of these functions will also prompt you for your user
;; name (usually the email address you signed up to twitter with)
;; and password the first time in a given Emacs session.  Note that
;; twitter uses `Basic Authentication' for user authentication,
;; which translates to, basically none.  It's not secure for
;; anything more than casual attacks.

;;			     FOR READING

;; This is a work in progress.  Just stubs.  I have to figure out
;; how to make some use out of `xml-parse-fragment'.  Until then,
;; `twit-list-followers' is incredibly stupid, but works.

;;			     FOR HACKING

;; See `twit-post-function', which is the backend for posting, and
;; `twit-parse-xml' which grabs an xml file from HTTP and turns it
;; into a list structure (using `xml-parse-fragment').  This is a work
;; in progress.

;; Installing:

;; There's not much to it.  It you want it always there and ready, you
;; can add something to your .emacs file like:

;;  (load-file "/path/to/twit.el")

;; or get fancier, to the extent you want and know how (autoloading,
;; keybinding, etc).

;; You might wasnt to run M-x cutomize-group twit and set your twitter
; username/password. 

;; Notes:

;; `twit-user' gets my vote for variable name of the year.  Ditto
;; `twit-mode' for mode names.

;; Integration:

;; This package integrates nicely with todochiku.el, which you
;; can download here:
;;  http://www.emacswiki.org/cgi-bin/wiki/todochiku.el
;; Just customize twit-new-tweet-hook and set it to twit-todochiku 

;;; History:

;; 2007-3-16 theron tlax <thorne@timbral.net>
;; * 0.0.1 -- Initial release.  Posting only.
;; * 0.0.2 -- Near-total rewrite; better documentation; use standard
;;            Emacs xml and url packages; minor mode; a little
;;            abstraction; some stubs for the reading functions.
;; * 0.0.3 -- Doc and other minor changes.
;; * 0.0.4 -- (released as 0.0.3 -- Added twit-show-recent-tweets 
;;             by Jonathan Arkell)
;; * 0.0.5 -- Add source parameter to posts
;; * 0.0.6 -- Re-working twit-show-recent-tweets to show more info
;;            (and to get it working for me) -- by H Durer
;; * 0.0.7 -- Keymaps in the buffers for twit-show-recent-tweets and
;;            twit-list-followers; encode the post argument so that it 
;;            is a valid post request
;; * 0.0.8 -- faces/overlays to make the *Twit-recent* buffer look
;;            prettier and more readable (at least for me) -- by H Durer
;; * 0.0.9 -- follow-recent-tweets function created so automagickally 
;;            follow tweets every 5 mins.  Also removed twit-mode 
;;            on twit-show-recent-tweets.  (it was setting twit-mode
;;            globally, and interfering with planner)  (JA)
;; * 0.0.10 - There is a hook that is run when a new tweet is seen.
;;            This can be used to interface with the todochiku package
;;            to send a notification that there is a new tweet.
;;            (or anything else for that matter)
;;            Twit-user and Twit-pass are now customizeable variables
;;            that work.  Finally, no more constant re-entry of your
;;            username and password. (JA)
;; * 0.0.11 - Updated to set a customization for the
;;            follow-recent-tweets idle timer.  This is so that you
;;            wont get throttled when twitter changes their throttle
;;            time (JA)
;; * 0.0.12 - Changed syncronous url call to an ascynronous one that
;;            doesn't suck, and polls properly.
;;            You can finally stop following recent tweets. (Rev 22)
;; * 0.0.13 - Fixed twit-debug to be a customizeable variable.  Duh.
;;            Image handling is on the way. (done, just buggy)
;;            Better face definitions, now customizeable.
;;            Zebra-tabling of the recent tweets is half there.
;;            Retrieval of the rate-limiting is working. (Rev 23)
;; * 0.0.14 - Finished zebra-table for recent tweets. (uses overlays)
;;            Fix for a really crazy bug in carbon emacs. (thanks gr3p3)
;;            Tweaked default fonts to not suck, added more faces.
;;            If showing recent tweets fails, the buffer is no longer
;;            set to blank. (JA)
;; * 0.0.15 - Fixed the automatic rate-limiting on 400 messages from
;;            twitter to work.
;;            Updated rate limiting to use the new format
;;            More messages are handed off to todochiku
;;            Most rate limiting (except the initial follow) is done
;;            Asyncronously.
;;            Verified that twit-list-followers works.
;;            URLs now are fontified and hot.
;;            Maybe (maybe?) fixed the bug where following tweets
;;            kills the mark, and dumps the point at the bottom. (JA)
;;
;; Bugs:
;; * Following recent tweets does this really annoying thing where
;;   the point will jump to the bottom on occation.  This seems to
;;   happen if the recent tweets is null data.
;;   Definately has to do with twit-get-and-set-async-rate-limit
;; * Face definitions for zebra tables kinda suck.  I lack experience
;;   in making them not suck.  There needs to be a light and dark
;;   background version of the defnition, instead of the raw default.
;;
;; Please report bugs to the twit emacs wiki page at:
;;   http://www.emacswiki.org/cgi-bin/wiki/TwIt
;;
;;; Roadmap:
;; v1.0 release
;; - Change the layout to be a little nicer.  (almost there)
;; - make sure, 100% sure, that following tweets is rock solid
;;   and doesn't mess with the point. Please send me a report
;;   or at least a message if this is happening to you.
;;   (this is a tough one to debug)
;;
;; Post 1.0
;; - Images
;; - Make @names a different color, and make them hot, so that hitting return on them will take you
;;   to their page
;; - Integrate the other twit.el that is out there.  Looks like it might have some serious sexxxy to it.

;;; Code:

(require 'xml)
(require 'url)
(require 'url-http)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar twit-version-number "0.0.15")

(defvar twit-status-mode-map (make-sparse-keymap))
(defvar twit-followers-mode-map (make-sparse-keymap))

(defgroup twit nil
  "twit.el customizations."
  :version "0.1"
  :group 'twit)

(defcustom twit-user
  ""
  "Your twitter username."
  :group 'twit
  :type 'string)

(defcustom twit-pass
  ""
  "Your twitter password."
  :group 'twit
  :type 'string)

(defcustom twit-new-tweet-hook
  '()
  "Functions to execute when there is a new tweet."
  :type 'hook
  :group 'twit)

(defcustom twit-follow-idle-interval
  90
  "How long in time to wait before checking for new tweets.
Right now it will check every 90 seconds, Which will generate a maximum of 40 requests, leaving you another 30 per hour to play with.

The variable name is a bit of a misnomer, because it is not actually based on idle time (anymore)."
  :type 'integer
  :group 'twit)

(defvar twit-shadow-follow-idle-interval
  twit-follow-idle-interval
  "Shadow definition of `twit-follow-idle-interval' that we can modify on the fly.")

(defcustom twit-user-image-dir
  (concat (car image-load-path) "twitter")
  "Directory where twitter user images are to be stored.  Need not exist."
  :type 'string
  :group 'twit)


(defcustom twit-debug
  nil
  "Whether or not to run twit.el in debug mode"
  :group 'twit
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface twit-message-face
  '((default
	  :family "helv"
	  :height 1.1))
  "The font face to use for a twitter message."
  :group 'twit)

(defface twit-author-face
  '((t
	  (:height 0.8
	   :weight bold
	   :family "mono")))
  "The font face to use for the authors name"
  :group 'twit)

(defface twit-info-face
  '((t (:height 0.8 :slant italic)))
  "Face for displaying where, how and when someone tweeted.")

(defface twit-title-face
  '((t
	  (:background "PowderBlue"
	   :underline "DeepSkyBlue")))
  "Title Area of the recent tweets buffer."
  :group 'twit)

(defface twit-zebra-1-face
  '((t
	  :background "gray89"))
  "Color one of zebra-striping of recent tweets and followers list."
  :group 'twit)

(defface twit-zebra-2-face
  '((t
	  :background "AliceBlue"))
  "Color two of zebra-striping of recent tweets and followers list."
  :group 'twit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explicitly set URL info with customization vars

(if (and (not (assoc "twitter.com:80" (symbol-value url-basic-auth-storage)))
		 (not (string= twit-pass ""))
		 (not (string= twit-user "")))
	(progn
	 (set url-basic-auth-storage
		  (cons (list "twitter.com:80"
					  (cons "Twitter API"
							(base64-encode-string (format "%s:%s" twit-user twit-pass))))
				(symbol-value url-basic-auth-storage)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymaps
;; 'r' key for reloading/refreshing the buffer
(defvar twit-key-list
  '(("s" . twit-show-recent-tweets)
	("f" . twit-list-followers)
	("p" . twit-post)
	("h" . twit-mode-help)
	("?" . twit-mode-help)))

(define-key twit-status-mode-map "r" 'twit-show-recent-tweets)
(define-key twit-followers-mode-map "r" 'twit-list-followers)


(dolist (info twit-key-list)
  (define-key twit-status-mode-map (car info) (cdr info))
  (define-key twit-followers-mode-map (car info) (cdr info)))

(defun twit-mode-help ()
	(interactive)
	(message "Help: %s" (append twit-key-list '(("r" . "Reload Current Page")))))

(defvar twit-timer
  nil
  "Timer object that handles polling the followers")

(defvar twit-rate-limit-timer
  nil
  "Timer object to poll the rate-limiting.")

;; Most of this will be used in the yet-to-be-written twitter
;; reading functions.
(defvar twit-base-url "http://twitter.com")

(defconst twit-update-url 
  (concat twit-base-url "/statuses/update.xml"))
(defconst twit-puplic-timeline-file
  (concat twit-base-url "/statuses/public_timeline.xml"))
(defconst twit-friend-timeline-file
  (concat twit-base-url "/statuses/friends_timeline.xml"))
(defconst twit-followers-file
  (concat twit-base-url "/statuses/followers.xml"))
(defconst twit-friend-list-file
  (concat twit-base-url "/statuses/friends.xml"))
(defconst twit-rate-limit-file
  (concat twit-base-url "/account/rate_limit_status.xml"))

(defconst twit-success-msg 
  "Post sent (no guarantees, though)")
(defconst twit-too-long-msg 
  "Post not sent because length exceeds 140 characters")

(defconst twit-standard-rate-limit 70)

(defconst twit-rate-limit-offset 5
  "Number of seconds to add to a throttled rate limit for insurance.")

(defconst twit-rate-limit-interval (* 2 60 60)
  "Every 2 Hours check for rate limiting.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Library Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun twit-alert (msg &optional title)
  "Send some kind of alert to the user.  
If todochiku is available, use that.  Instead, just message the user."
  (when (null title) (setq title "twit.el"))
  (when (featurep 'todochiku)
	  (todochiku-message title msg (todochiku-icon 'social)))
  (message "%s: %s" title msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General purpose library to wrap twitter.com's api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun twit-parse-xml (url)
  "Retrieve file at URL and parse with `xml-parse-fragment'.
Emacs' url package will prompt for authentication info if required."
  (let ((result nil))
    (save-window-excursion
      (set-buffer (url-retrieve-synchronously url))
      (goto-char (point-min))
      (setq result (xml-parse-fragment))
      (kill-buffer (current-buffer)))
    result))

(defvar twit-async-buffer 'nil
  "Buffer that stores the temporary XML result for tiwt.el")

(defun twit-parse-xml-async (url callback)
  "Retrieve the resource at URL, and when retrieved call callback
This is the asyncronous version of twit-parse-xml.  Once that function is
refactored, and its named changed, so should this one."
  (setq twit-async-buffer (url-retrieve url 'twit-parse-xml-async-retrieve (list url callback))))

(defun twit-parse-xml-async-retrieve (status url callback)
  (if (null status)   ; no news is good news.  
	  (let ((result nil))
		(if (bufferp twit-async-buffer)
			(save-excursion
			 (set-buffer twit-async-buffer)
			 (goto-char (point-min))
			 (setq result (xml-parse-fragment))
			 (kill-buffer (current-buffer))))
		(funcall callback status url result))
	  (progn
	   (twit-alert (format "Cannot retrieve twit URL.  Status is: %S" status))
	   (when (equal status '(:error (error http 400)))
			 (twit-get-and-set-async-rate-limit)))))

(defun twit-post-function (url post)
  (let ((url-request-method "POST")
	(url-request-data (concat "source=twit.el&status=" (url-hexify-string post)))
        ;; these headers don't actually do anything (yet?) -- the 
        ;; source parameter above is what counts
        (url-request-extra-headers `(("X-Twitter-Client" . "twit.el")
                                     ("X-Twitter-Client-Version" . ,twit-version-number)
                                     ("X-Twitter-Client-URL" . "http://www.emacswiki.org/cgi-bin/emacs/twit.el"))))
    (twit-alert url-request-data)
    (url-retrieve url (lambda (arg) (kill-buffer (current-buffer))))))

(defun twit-parse-rate-limit (xml)
  "Parse the rate limit file, and return the hourly limit.  XML should be the twitter ratelimit sxml.
XML should not have any HTTP header information in its car."
  (let ((limit (assoc 'hourly-limit xml)))
	(if twit-debug (message "Parsed limit %s from xml %s" limit xml))
	(if limit
		(string-to-number (caddr limit)))))

(defun twit-get-rate-limit ()
  (interactive)
  "Returns the rate limit as a number from the xml."
  (let ((limit-xml (twit-parse-xml twit-rate-limit-file)))
	(twit-parse-rate-limit (cadr limit-xml))))

(defun twit-get-and-set-async-rate-limit ()
  (interactive)
  "Check rate limiting asyncronously, and automagickally set it."
  (twit-parse-xml-async twit-rate-limit-file 'twit-get-and-set-async-rate-limit-callback))

(defun twit-get-and-set-async-rate-limit-callback (status url result)
  "callback for twit-get-and-set-async-rate-limit"
  (if (null status)
	  (if twit-debug (message "Rate Limit XML is %S" result))
	  (twit-verify-and-set-rate-limit (twit-parse-rate-limit (cadr result)))
	  (twit-alert (format "Cannot retrieve rate limit URL %S! Status: %S" url status))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for the interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun twit-query-for-post ()
  "Query for a Twitter.com post text in the minibuffer."
  (read-string "Post (140 char max): "))

(defvar twit-last-tweet '()
  "The last tweet that was posted.
This is a bit of an ugly hack to store the last tweet that was shown through twit-write-recent-tweets.
It is in the format of (timestamp user-id message) ")

(setq twit-last-tweet '())
; damn.. my latest addition to add the most recent tweet is HACKY.
(defun twit-write-recent-tweets (xml-data)  ;(twit-parse-xml twit-friend-timeline-file)
  (delete-region (point-min) (point-max))
  (twit-insert-with-overlay-attributes (format-time-string "Last updated: %c\n")
									   '((face . "twit-title-face")))
  (let ((last-tweet '())
		(times-through 1)
		(overlay-start 0)
		(overlay-end 0)
		(overlays 'nil))
	(labels ((xml-first-child (node attr)
							  (car (xml-get-children node attr)))
			 (xml-first-childs-value (node addr)
									 (car (xml-node-children (xml-first-child node addr)))))
			(dolist (status-node (xml-get-children (cadr xml-data) 'status))
					(let* ((user-info (xml-first-child status-node 'user))
						   (user-id (or (xml-first-childs-value user-info 'screen_name) "??"))
						   (user-name (xml-first-childs-value user-info 'name))
						   (location (xml-first-childs-value user-info 'location))
						   (src-info (xml-first-childs-value status-node 'source))
										;(user-img (twit-get-user-image (xml-first-childs-value user-info 'profile_image_url)))
						   (user-img nil)y
						   (timestamp (xml-first-childs-value status-node 'created_at))
						   (message (xml-first-childs-value status-node 'text)))
					  (if (= times-through 1)
						  (progn
						   (setq last-tweet (list timestamp user-id message))
						   (setq first-time-through nil)))
					  ;; the string-match is a bit weird, as emacswiki.org won't
					  ;; accept pages with the href in it per se
					  (when (and src-info (string-match (concat "<a h" "ref=\"\\(.*\\)\">\\(.*\\)<" "/a>")
														src-info))
							;; remove the HTML link info; leave just the name (for now)
							(setq src-info (match-string 2 src-info)))
					  ;; Image first.  I wonder how this will work.
					  (when (and user-img (not (bufferp user-img)))
							(insert-image user-img))
					  ;; First line: Name and message
					  (setq overlay-start (point))
					  (twit-insert-with-overlay-attributes (format "%25s" 
																   (concat user-id
																		   (if user-name
																			   (concat " (" user-name ")")
																			   "")))
														   '((face . "twit-author-face")))
					  (insert ": ")
					  (twit-insert-with-overlay-attributes message
														   '((face . "twit-message-face")))
					  (insert "\n")
					  
					  (when (or timestamp location src-info)
							(twit-insert-with-overlay-attributes
							 (concat "                          "
									 (when timestamp (concat " posted " timestamp))
									 (when location (concat " from " location))
									 (when src-info (concat " (via " src-info ")"))
									 "\n")
							 '((face . "twit-info-face"))))
					  (setq overlay-end (point))
					  (let ((o (make-overlay overlay-start overlay-end)))
						(overlay-put o 'face (if (= 0 (% times-through 2))
												 "twit-zebra-1-face"
												 "twit-zebra-2-face"))
						(add-to-list 'overlays o)))
					(setq times-through (+ 1 times-through))))
	(if (not (equal last-tweet twit-last-tweet))
		(progn (setq twit-last-tweet last-tweet)
			   (setq first-time-through nil)
			   (run-hooks 'twit-new-tweet-hook))))
  
  ;; go back to top so we see the latest messages
  (goto-address)
  (beginning-of-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User images.  Not complete.

(defvar twit-user-image-list 'nil
  "List containing all user images")

(setq twit-user-image-list 'nil)

(defun twit-get-user-image (url)
  "Retrieve the user image from the list, or from the URL"
  (let ((img (assoc url twit-user-image-list)))
	(if (and img (not (bufferp (cdr img))))
		(cdr (assoc url twit-user-image-list))
		(let ((url-buffer (url-retrieve url 'twit-write-user-image (list url))))
		  (if url-buffer
			  (progn
			   (add-to-list 'twit-user-image-list (cons url url-buffer))
			   (if twit-debug (message "list is %s" twit-user-image-list)))
			  (twit-alert (format "Warning, couldn't load %s " url)))))))

(defun twit-write-user-image (status url)
  "Called by twit-get-user-image, this performs the actual writing of the status url."
  (debug)
  (let ((buffer (and (assoc url twit-user-image-list)
					 (cdr (assoc url twit-user-image-list))))
		(image-file-name (concat twit-user-image-dir "/" (file-name-nondirectory url))))
	(when (not (file-directory-p twit-user-image-dir))
		  (make-directory twit-user-image-dir))
	(save-window-excursion
	 (set-buffer buffer)
	 (setq buffer-file-name image-file-name)
	 (save-buffer)
	 (delete buffer twit-user-image-list)
	 (add-to-list 'twit-user-image-list (create-image image-file-name)))))

;;;
;; Recent tweets timer funciton and callback

(defun twit-follow-recent-tweets-timer-function ()
  "Timer function for recent tweets, called via a timer"
  (twit-parse-xml-async twit-friend-timeline-file 'twit-follow-recent-tweets-async-callback))

(defun twit-follow-recent-tweets-async-callback (status url xml)
  (when (not status)
		(save-window-excursion
		 (set-buffer (get-buffer-create "*Twit-recent*"))
		 (toggle-read-only 0)
		 (twit-write-recent-tweets xml)
		 (toggle-read-only 1))))

(defvar twit-last-rate-limit
  twit-standard-rate-limit
  "What is the previous rate limit?")

(defun twit-verify-and-set-rate-limit (limit)
  "Check if limiting is in effect, and if so, set the timer."
  (let ((limit-reset nil))
	(if twit-debug (message  "Rate limit is %s, doing ratelimit magic." limit))
	(when (and limit
			   (not (= limit 0))
			   (not (= twit-last-rate-limit limit)))
		  (cond ((< limit twit-standard-rate-limit)
				 (progn
				  (setq twit-shadow-follow-idle-interval (+ (/ (* 60 60) limit)
															twit-rate-limit-offset))
				  (setq limit-reset 't)
				  (twit-alert (format "Twitter is under a rate limit.  Timer set to %s seconds." twit-shadow-follow-idle-interval))))
				((= limit twit-standard-rate-limit)
				 (progn
				  (setq twit-shadow-follow-idle-interval twit-follow-idle-interval)
				  (setq limit-reset 't)
				  (twit-alert (format "Rate limiting relaxed.  Timer set to normal timeout (%s seconds)" twit-shadow-follow-idle-interval))))
				(t ; exceptional case...
				 (progn
				  (setq twit-shadow-follow-idle-interval twit-follow-idle-interval)
				  (setq limit-reset 't)
				  (twit-alert (format "The twitter rate has exceeded its expected maximum.  This is weird."))))))
	(when (and limit-reset (timerp twit-timer))
		  (progn
		   (if twit-debug (message "Cancelling and restarting timer."))
		   (cancel-timer 'twit-timer)
		   (twit-follow-recent-tweets)))
	(setq twit-last-rate-limit limit))) 

;;; funciton to integrade with growl.el or todochiku.el
(defun twit-todochiku ()
  (todochiku-message "twit.el" (format "From %s:\n%s" (cadr twit-last-tweet) (caddr twit-last-tweet)) (todochiku-icon 'social)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun twit-post ()
  "Send a post to twitter.com.
Prompt the first time for password and username \(unless
`twit-user' and/or `twit-pass' is set\) and for the text of the
post; thereafter just for post text.  Posts must be <= 140 chars
long."
  (interactive)
  (let* ((post (twit-query-for-post)))
    (if (> (length post) 140)
	(error twit-too-long-msg)
      (if (twit-post-function twit-update-url post)
		  (twit-alert twit-success-msg)))))

;;;###autoload
(defun twit-post-region (start end)
  "Send text in the region as a post to twitter.com.
Uses `twit-post-function' to do the dirty work and to obtain
needed user and password information.  Posts must be <= 140 chars
long."
  (interactive "r")
  (let ((post (buffer-substring start end)))
    (if (> (length post) 140)
	(error twit-too-long-error)
      (if (twit-post-function twit-update-url post)
		  (twit-alert twit-success-msg)))))

;;;###autoload
(defun twit-post-buffer ()
  "Post the entire contents of the current buffer to twitter.com.
Uses `twit-post-function' to do the dirty work and to obtain
needed user and password information.  Posts must be <= 140 chars
long."
  (interactive)
  (let ((post (buffer-substring (point-min) (point-max))))
    (if (> (length post) 140)
	(error twit-too-long-error)
      (if (twit-post-function twit-update-url post)
		  (twit-alert twit-success-msg)))))

;;;###autoload
(defun twit-list-followers ()
  "Display a list of all your twitter.com followers' names."
  (interactive)
  (pop-to-buffer "*Twit-followers*")
  (kill-region (point-min) (point-max))
  (loop for name in 
        (loop for name in
              (loop for user in 
                    (xml-get-children
                     (cadr (twit-parse-xml twit-followers-file)) 'user)
                    collect (sixth user))
              collect (third name))
        do (insert (concat name "\n")))
  ;; set up mode as with twit-show-recent-tweets
  (text-mode)
  (use-local-map twit-followers-mode-map))

;;; Helper function to insert text into buffer, add an overlay and
;;; apply the supplied attributes to the overlay
(defun twit-insert-with-overlay-attributes (text attributes)
  (let ((start (point)))
    (insert text)
    (let ((overlay (make-overlay start (point))))
      (dolist (spec attributes)
        (overlay-put overlay (car spec) (cdr spec))))))


;;;###autoload
(defun twit-follow-recent-tweets ()
  "Display, and redisplay the tweets.  This might suck if it bounces the point to the bottom all the time."
  (interactive)
  (twit-show-recent-tweets)
  (twit-verify-and-set-rate-limit (twit-get-rate-limit))
  (setq twit-rate-limit-timer (run-with-timer twit-rate-limit-interval twit-rate-limit-interval 'twit-get-and-set-async-rate-limit))
  (setq twit-timer (run-with-timer twit-shadow-follow-idle-interval twit-shadow-follow-idle-interval 'twit-follow-recent-tweets-timer-function)))

(defun twit-stop-following-tweets ()
  "When you want to stop following tweets, you can use this function to turn off the timer."
  (if (featurep 'todochiku)
	  (todochiku-message "Twit.el" "Twit.el Stopped Following Tweets" (todochiku-icon 'social)))
  (interactive)
  (cancel-timer twit-timer))

;;;###autoload
(defun twit-show-recent-tweets ()
  "Display a list of the most recent twewets from your followers."
  (interactive)
  (pop-to-buffer "*Twit-recent*")
  (toggle-read-only 0)
  (twit-write-recent-tweets (twit-parse-xml twit-friend-timeline-file))
  ;; set up some sensible mode and useful bindings
  (text-mode)
  (toggle-read-only 1)
  (use-local-map twit-status-mode-map))

;;;###autoload
(define-minor-mode twit-mode 
  "Toggle twit-mode.
Globally binds some keys to Twit's interactive functions.

With no argument, this command toggles the mode. 
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\\{twit-mode-map}" nil
" Twit" 
'(("\C-c\C-tp" . twit-post)
  ("\C-c\C-tr" . twit-post-region)
  ("\C-c\C-tb" . twit-post-buffer)
  ("\C-c\C-tf" . twit-list-followers))
 :global t
 :group 'twit
 :version twit-version-number)

(provide 'twit)

;;; twit.el ends here
