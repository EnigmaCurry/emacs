;; My passwords are obviously not here. These are for you to override yourself.

;Bip server
(setq bip-username "YourBipUsername")
(setq bip-password "Username:Password:Network")
;Twitter
(setq twit-user "YourTwitterUsername")
(setq twit-pass "YourTwitterPassword")
;Gmail
(setq gmail-user "YourGmailUsername")
(setq gmail-pass "YourGmailPassword")

;; You can also do as I do and keep them in a seperate folder:
(add-to-list 'load-path "~/.emacs.private")
;;Load the password file but don't complain if it doesn't exist
(load "real-passwords" 'noerror)
