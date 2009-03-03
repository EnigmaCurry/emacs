(require 'erc)

;;Much of ERC is customized through Emacs customize-mode
;;But here's some extra stuff

(load-library "ryan-libnotify")

;;Set the default path for erc logs
(setq erc-log-channels-directory "~/.erc/logs/")

(defun bip-freenode()
  (interactive)
  "Connect to EnigmaCurry's Bip server"
  (erc :server "localhost" :port "7778" :nick bip-username :password bip-password)
  )

;; Notify me when someone wants to talk to me.
;; Heavily based off of ErcPageMe on emacswiki.org, with some improvements.
;; I wanted to learn and I used my own notification system with pymacs
;; Delay is on a per user, per channel basis now.
(defvar erc-page-nick-alist nil
  "Alist of 'nickname|target' and last time they triggered a notification"
  )
(defun erc-notify-allowed (nick target &optional delay)
  "Return true if a certain nick has waited long enough to notify"
  (unless delay (setq delay 30))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc (format "%s|%s" nick target) erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons (format "%s|%s" nick target) cur-time) erc-page-nick-alist)
      t)
    )
  )
(defun erc-notify-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
	(target (car (erc-response.command-args parsed)))
	(msg (erc-response.contents parsed)))
    ;;Handle true private/direct messages (non channel)
    (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
               (erc-current-nick-p target)
	       (erc-notify-allowed nick target)
	       )
      ;Do actual notification
      (ding)
      (notify-desktop (format "%s - %s" nick
                              (format-time-string "%b %d %I:%M %p"))
                      msg 0 "gnome-emacs")
      )
    ;;Handle channel messages when my nick is mentioned
    (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
               (string-match (erc-current-nick) msg)
               (erc-notify-allowed nick target)
	       )
      ;Do actual notification
      (ding)
      (notify-desktop (format "%s - %s" target
                              (format-time-string "%b %d %I:%M %p"))
                      (format "%s: %s" nick msg) 0 "gnome-emacs")
      )
    )
      
  )

;(add-hook 'erc-server-PRIVMSG-functions 'erc-notify-PRIVMSG)


;; Ignore certain types of messages from being tracked in the modeline:
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
