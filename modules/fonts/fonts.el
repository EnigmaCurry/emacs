;;; Download JetBrains Mono typeface
(let* ((url "https://github.com/JetBrains/JetBrainsMono/releases/download/v2.304/JetBrainsMono-2.304.zip")
       (zip-file (expand-file-name "JetBrainsMono.zip" temporary-file-directory))
       (font-dir (expand-file-name "~/.local/share/fonts/JetBrainsMono/"))
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
    (message "JetBrains Mono font installed successfully.")))

;;; show list of installed fonts:
;;(font-family-list)
;;; show default font:
;;(face-attribute 'default :font)
;;; Set default font faces:
;; M-x my/custom-settings    :: font-settings group:
(defgroup my/font-settings nil
  "My custom font settings"
    :group 'my/custom-settings)
(defcustom my/font-family-default "Noto Sans Mono"
  "Default font family"
  :type 'string
  :group 'my/font-settings)
(defcustom my/font-family-default-fixed-pitch "Noto Sans Mono"
  "Default font family for fixed-pitch faces"
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
  (set-face-attribute 'fixed-pitch nil
                      :family my/font-family-default-fixed-pitch
                      :height my/font-size-default)
  t)
(my/font-settings-apply)
(add-hook 'after-init-hook #'my/font-settings-apply)
(advice-add 'custom-save-all :after (lambda ()
            "Re-apply custom settings after saving customizations."
            (my/font-settings-apply)))

(use-package show-font)

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

(defun my/default-text-scale-reset nil
  (setq default-text-scale--complement 0)
  (set-face-attribute 'default
                      nil
                      :height my/default-text-height)
  (message "Text height reset: %d" my/default-text-height)
  )
;; Scale text sizes in all buffers :: https://github.com/purcell/default-text-scale
(use-package
  default-text-scale
  :general
  ("C-="
   'default-text-scale-increase
   "C--"
   (lambda ()
     "Reset text scale if C-u is used, otherwise decrease it."
     (interactive)
     (let ((prefix current-prefix-arg))
       ;; Intercept and clear the prefix argument before calling the function
       (setq current-prefix-arg nil)
       (if prefix
           (my/default-text-scale-reset)
         (default-text-scale-decrease)))))
  :init (setq default-text-scale-amount 5))
