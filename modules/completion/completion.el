;; Allow minibuffers to stack:
  (setq-default enable-recursive-minibuffers t)
  ;; Filter command completions to only include commands
  ;; applicable to the current major mode:
  (setq-default read-extended-command-predicate
                #'command-completion-default-include-p)
  ;; Add custom prompt when asking for multiple values as (comma) separated list:
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (replace-regexp-in-string
                               "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                               crm-separator)
                              (car args))
                      (cdr args))))
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (use-package vertico
    :custom
    (vertico-scroll-margin 2)
    (vertico-count 10)
    (vertico-resize 'grow-only)
    (vertico-cycle nil)
    :init
    ;;(keymap-set vertico-map "?" #'minibuffer-completion-help)
    ;;(keymap-set vertico-map "M-TAB" #'vertico-insert)
    ;;(keymap-set vertico-map "TAB" #'minibuffer-complete)
    (vertico-mode))

  (use-package orderless
    :custom
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (orderless-style-dispatchers
    ;;   '(+orderless-consult-dispatch orderless-affix-dispatch))
    ;; (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))

  (use-package marginalia
    :hook
    ((marginalia-mode . all-the-icons-completion-marginalia-setup))
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  (use-package nerd-icons-completion
    :init
    (nerd-icons-completion-mode))

(use-package company)
