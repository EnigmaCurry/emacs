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
    :models '(gpt-oss:120b))))
