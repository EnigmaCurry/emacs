(defun epg-disable-agent ()
  "Make EasyPG bypass any gpg-agent"
  (interactive)
  (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent bypassed"))

(defun epg-enable-agent ()
  "Make EasyPG use a gpg-agent after having been disabled with epg-disable-agent"
  (interactive)
  (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent re-enabled"))

(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (message "Running advice")
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))


