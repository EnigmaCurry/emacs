;; -*- coding: utf-8 -*-
;; GUI defaults loaded in early-init.el

;; Nice defaults
(setq confirm-kill-emacs #'yes-or-no-p)
(setq vc-follow-symlinks t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(electric-pair-mode t)
(define-key global-map (kbd "M-o") 'browse-url-at-point)

;; Store file backups in ~/.emacs.d/backup rather than being littered everywhere:
;; Reference: https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)
;; https://www.emacswiki.org/emacs/ForceBackups
(defun force-backup-of-buffer () (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-backup-of-buffer)

;; Store automatic customisation options in ~/.emacs.d/custom.el
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

;; Company (in-buffer completion dropdown) :: https://github.com/company-mode/company-mode
(use-package company)

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
  (setq lsp-modeline-diagnostics-scope :workspace)
  :hook
  ((web-mode . lsp) (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
(use-package flycheck)

;; LSP debuggers
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Poetry (Python environment manager) :: https://github.com/galaunay/poetry.el
(use-package poetry
  :init
  (advice-add 'pyvenv-activate
    :after
    ;; lsp needs to be enabled *after* poetry activates
    (lambda (&rest r) (lsp))
    '((name . "poetry-after-workon-activate-lsp")))
  (poetry-tracking-mode))

;; Black (Python code formatter) :: https://github.com/wbolster/emacs-python-black
;; Note: this depends on black being installed in the project virtualenv as a dev dependency
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

;; Web mode :: https://github.com/fxbois/web-mode
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))

;; Tailwind CSS :: https://github.com/merrickluo/lsp-tailwindcss
(use-package lsp-tailwindcss)

;; vterm (terminal emulator) :: https://github.com/akermu/emacs-libvterm
;; Configure BASH to work with vterm: https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
(use-package vterm
  :init (define-key global-map (kbd "C-c t") 'vterm-toggle))
;; shell-pop for vterm :: https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle)

;; ace-link (follow links in info docs) :: https://github.com/abo-abo/ace-link
(use-package ace-link
  :init (ace-link-setup-default))
