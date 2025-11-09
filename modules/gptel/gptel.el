(use-package
  gptel
  :general
  ("C-c C-g" 'gptel-menu)
  (:keymaps 'gptel-mode-map "C-c C-c" 'gptel-send)
  :config (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
                                        ;(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (setq gptel-org-branching-context t)
  :init
  ;; LM-studio offers an OpenAI compatible API
  (setq
   gptel-model 'test
   gptel-backend
   (gptel-make-openai
       "openweb-ui"
     :stream t
     :protocol "https"
     :host "openwebui.example.com"
     :key "sk-1231231231341354123123123123"
     :endpoint "/api/chat/completions"
     :models '(gpt-oss:120b)))
  (defun my-gptel-export-context-markdown ()
    "Export the current GPTel context as Markdown with file-labeled code blocks."
    (interactive)
    (require 'gptel)
    (unless (boundp 'gptel-context--alist)
      (user-error "GPTel context not found"))
    ;; Build the Markdown content
    (let ((md-output ""))
      (dolist (entry (symbol-value 'gptel-context--alist))
        (let ((src (car entry))
              (data (cdr entry)))
          (cond
           ;; If context item is a live buffer (region or whole buffer content)
           ((bufferp src)
            (when (buffer-live-p src)
              ;; Determine a name for the source (file name if available, else buffer name)
              (let* ((buf src)
                     (file-name (buffer-file-name buf))
                     (src-name (if file-name
                                   (file-name-nondirectory file-name)
                                 (buffer-name buf))))
                ;; Loop through overlays (regions) or treat whole buffer
                (with-current-buffer buf
                  (if (listp data)
                      ;; Multiple regions (overlays) stored for this buffer
                      (dolist (ov data)
                        (when (overlayp ov)
                          (let ((code (buffer-substring-no-properties (overlay-start ov)
                                                                      (overlay-end ov))))
                            (setq md-output
                                  (concat md-output
                                          (format "**File:** %s\n\n" src-name)
                                          "```"
                                          ;; Optionally, add language hint based on file extension or mode
                                          (let ((mode major-mode))
                                            (cond
                                             ((and file-name (string-match "\\.\\([^.]+\\)$" src-name))
                                              (concat (match-string 1 src-name) "\n"))
                                             ((eq mode 'emacs-lisp-mode) "elisp\n")
                                             (t "\n")))
                                          code
                                          "\n```\n\n")))))
                    ;; If `data` is not a list, assume whole buffer content
                    (let ((code (buffer-substring-no-properties (point-min) (point-max))))
                      (setq md-output
                            (concat md-output
                                    (format "**File:** %s\n\n" src-name)
                                    "```\n" code "\n```\n\n"))))))))
           ;; If context item is a file path (string)
           ((stringp src)
            (let ((path src))
              (when (file-readable-p path)
                (with-temp-buffer
                  (insert-file-contents path)
                  (let* ((src-name (file-name-nondirectory path))
                         (code (buffer-substring-no-properties (point-min) (point-max)))
                         (ext (file-name-extension src-name)))
                    (setq md-output
                          (concat md-output
                                  (format "**File:** %s\n\n" src-name)
                                  (if ext (concat "```" ext "\n") "```\n")
                                  code
                                  "\n```\n\n"))))))))))
      ;; Show the result in a new buffer for the user to copy
      (with-current-buffer (get-buffer-create "*GPTel Context Markdown*")
        (erase-buffer)
        (insert md-output)
        (markdown-mode)
        (display-buffer (current-buffer))))))
