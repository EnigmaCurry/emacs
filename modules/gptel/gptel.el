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
    "lm-studio"
    :stream t
    :protocol "http"
    :host "localhost:1234"
    :models '(test)))
  (defun my-gptel-deepseek-wrap-think-block (beg end)
    "Wrap '<think>' blocks in an Org-mode drawer if not already wrapped."
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char beg)
        ;; Find all occurrences of <think> blocks
        (while (re-search-forward "^<think>" end t)
          (let ((start (line-beginning-position)))
            ;; Check if the block is already wrapped
            (unless (save-excursion
                      (forward-line -4)
                      (looking-at "^:THINKING:$"))
              ;; Insert Org-mode drawer start
              (goto-char start)
              (kill-region (point) (line-end-position))
              (insert-and-inherit ":THINKING:\n")
              (insert-and-inherit
               "#+attr_shortcode: :title Thinking ...\n")
              (insert-and-inherit "#+begin_expand\n")
              (insert-and-inherit "<pre>")
              (forward-line 4)
              ;; Find the closing tag again after insertion
              (when (re-search-forward "</think>" nil t)
                (end-of-line)
                (kill-region (line-beginning-position) (point))
                (insert-and-inherit "</pre>")
                (end-of-line)
                ;; Ensure we don't add duplicate :END:
                (unless (looking-at "\n#+end_expand")
                  (insert-and-inherit "\n#+end_expand\n")
                  (insert-and-inherit ":END:\n"))
                ;; Move back to the start of the drawer for org-cycle
                (goto-char start)
                (org-cycle)))))))
    (message "Think blocks wrapped and folded."))

  (add-hook
   'gptel-post-response-functions
   #'my-gptel-deepseek-wrap-think-block))
