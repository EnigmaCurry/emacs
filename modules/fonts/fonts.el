;;; Install all the icons:
(use-package
 all-the-icons
 :if (display-graphic-p)
 :init
 (let ((font-file
        (expand-file-name "~/.local/share/fonts/all-the-icons.ttf")))
   (unless (file-exists-p font-file)
     (message "Installing all-the-icons fonts...")
     (all-the-icons-install-fonts t))))
