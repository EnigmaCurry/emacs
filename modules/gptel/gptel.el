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
    :models '(test))))
