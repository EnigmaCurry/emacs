;; -*- coding: utf-8 -*-
;; GUI defaults loaded in early-init.el

;; Nice defaults
(setq confirm-kill-emacs #'yes-or-no-p)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(electric-pair-mode t)
(setq vc-follow-symlinks t)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;; Install the straight.el package manager:
;; (Emacs' own default package system is disabled in early-init.el)
;; Reference: https://github.com/raxod502/straight.el#readme
;;            https://jeffkreeftmeijer.com/emacs-straight-use-package/
(defvar bootstrap-version)
(let
  (
    (bootstrap-file
      (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
    (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent
        'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package for all dependencies :: https://github.com/jwiegley/use-package
(straight-use-package 'use-package)
;; Make use-package use straight.el by default:
(setq straight-use-package-by-default t)

;; Scale text sizes in all buffers :: https://github.com/purcell/default-text-scale
(use-package default-text-scale
  :init
  (define-key global-map (kbd "C-=") 'default-text-scale-increase)
  (define-key global-map (kbd "C--") 'default-text-scale-decrease))

;; Ivy counsel (list-completion) :: https://oremacs.com/swiper/#introduction
(use-package counsel
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t))

;; Magit (git version control system) :: https://magit.vc/
(use-package magit
  :init (define-key global-map (kbd "C-c g") 'magit-status)
  ;; open magit in a full frame always:
  (setq magit-display-buffer-function
    #'magit-display-buffer-fullframe-status-v1))

;; Avy (like ace-jump) :: https://github.com/abo-abo/avy
(use-package avy
  :init (define-key global-map (kbd "C-c s") 'avy-goto-word-1))

;; which-key (shows keyboard shortcut completions) :: https://github.com/justbur/emacs-which-key
(use-package which-key
  :config (which-key-mode))

;; Elisp autoformatter :: https://codeberg.org/ideasman42/emacs-elisp-autofmt
(use-package elisp-autofmt
  :commands (elisp-autofmt-save-hook-for-this-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-save-hook-for-this-buffer)
  :straight
  (elisp-autofmt
    :type git
    :files (:defaults "elisp-autofmt")
    :repo "https://codeberg.org/ideasman42/emacs-elisp-autofmt.git")
  :init (setq default-buffer-file-coding-system 'utf-8-unix))

;; LSP mode :: https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((svelte-mode . lsp)
    (python-mode . lsp)
    (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)


;; LSP debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
