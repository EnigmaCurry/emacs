;;; Download JetBrains Mono typeface
(let* ((url "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.3.0/JetBrainsMono.zip")
       (zip-file (expand-file-name "JetBrainsMono.zip" temporary-file-directory))
       (font-dir (expand-file-name "~/.local/share/fonts/JetBrainsMonoNerdFont/"))
       (default-directory temporary-file-directory))
  (unless (file-directory-p font-dir)
    (url-copy-file url zip-file t)
    (make-directory font-dir t)
    (let ((output-buffer (generate-new-buffer "*unzip-output*")))
      (unwind-protect
          (call-process "unzip" nil output-buffer nil "-j" zip-file "-d" font-dir)
        (kill-buffer output-buffer)))
    (call-process "fc-cache" nil nil nil "-fv")
    (delete-file zip-file)
    (message "JetBrains Mono Nerd Font installed successfully.")))

;;; show list of installed fonts:
;;(font-family-list)
;;; show default font:
;;(face-attribute 'default :font)
;;; Set default font faces:
;; M-x my/custom-settings    :: font-settings group:
(defgroup my/font-settings nil
  "My custom font settings"
    :group 'my/custom-settings)
(defcustom my/font-family-default "JetBrainsMono Nerd Font"
  "Default font family"
  :type 'string
  :group 'my/font-settings)
(defcustom my/font-size-default 120
  "Default font size"
  :type 'string
  :group 'my/font-settings)
(defun my/font-settings-apply ()
  "Set the default font based on `my/font-family-default` and `my/font-size-default`."
  (set-face-attribute 'default nil
                      :family my/font-family-default
                      :height my/font-size-default)
  t)
(my/font-settings-apply)
(add-hook 'after-init-hook #'my/font-settings-apply)
(advice-add 'custom-save-all :after (lambda ()
            "Re-apply custom settings after saving customizations."
            (my/font-settings-apply)))

;; Use nerd icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font"))

(use-package show-font)
