(defun notify-desktop (title message &optional duration &optional icon)
  "Pop up a message on the desktop with an optional duration (forever otherwise)"
  (unless icon (setq icon "gnome-emacs"))
  (pymacs-exec "import pynotify")
  (pymacs-exec "pynotify.init('Emacs')")
  (if icon 
      (pymacs-exec (format "msg = pynotify.Notification('%s','%s','%s')"
                           title message icon))
    (pymacs-exec (format "msg = pynotify.Notification('%s','%s')" title message))
    ) 
  (if duration 
      (pymacs-exec (format "msg.set_timeout(%s)" duration))
    )
  (pymacs-exec "msg.show()")
  )
