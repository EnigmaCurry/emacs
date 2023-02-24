;; -*- coding: utf-8 -*-
;; EnigmaCurry's emacs config
;; inspiration : https://github.com/susam/emfy
;;               https://emacs.amodernist.com

;; GUI defaults are loaded in early-init.el before init.el

;; Nice defaults
(setq confirm-kill-emacs #'yes-or-no-p)
(setq vc-follow-symlinks t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(define-key global-map (kbd "M-o") 'browse-url-at-point)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default visible-bell t)
(column-number-mode)
(put 'narrow-to-region 'disabled nil)

;; core keybindings
(define-key global-map (kbd "C-;") 'comment-region) ; C-u prefix to uncomment

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
;; autosaves in separate directory
(make-directory "~/.emacs.d/auto-save/" t)
(setq auto-save-file-name-transforms
  '((".*" "~/.emacs.d/auto-save/" t)))

;; Store automatic customisation options in ~/.emacs.d/custom.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; global electric-pair-mode
(electric-pair-mode t)
(defvar markdown-electric-pairs '((96 . 96) (?* . ?*))
  "Electric pairs for markdown-mode.")
(defun markdown-add-electric-pairs ()
  (setq-local electric-pair-pairs
    (append electric-pair-pairs markdown-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))
(add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)

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
  (define-key global-map (kbd "C--") 'default-text-scale-decrease)
  (setq default-text-scale-amount 5))

;; Ivy counsel (list-completion) :: https://oremacs.com/swiper/#introduction
(use-package counsel
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (define-key global-map (kbd "M-y") 'counsel-yank-pop))

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
;; (use-package elisp-autofmt
;;   :commands (elisp-autofmt-save-hook-for-this-buffer)
;;   :hook (emacs-lisp-mode . elisp-autofmt-save-hook-for-this-buffer)
;;   :straight
;;   (elisp-autofmt
;;     :type git
;;     :files (:defaults "elisp-autofmt")
;;     :repo "https://codeberg.org/ideasman42/emacs-elisp-autofmt.git")
;;   :init (setq default-buffer-file-coding-system 'utf-8-unix))

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

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))
;; Activate python virtualenv BEFORE opening a python buffer and/or starting pyright server:
;; M-x pyvenv-activate     (~/.virtualenvs/XXX)
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs/")
  :config
  ;; (pyvenv-mode t)
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))
(use-package python-mode
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . flycheck-mode)
  (python-mode . company-mode)
  ;(python-mode . yas-minor-mode)
  (python-mode . python-black-on-save-mode)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  :config
  )
;; Black (Python code formatter) :: https://github.com/wbolster/emacs-python-black
;; Note: this depends on black being installed in the project virtualenv as a dev dependency
(use-package python-black
  :demand t
  :after python)

;; Web mode :: https://github.com/fxbois/web-mode
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
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

;; jump between windows (rebinds `C-x o`) :: https://github.com/abo-abo/ace-window
(use-package ace-window
  :init
  (setq aw-scope 'frame)
  (global-set-key [remap other-window] 'ace-window))

;; lispy LISP mode :: https://github.com/abo-abo/lispy
;; (use-package lispy
;;   :hook (emacs-lisp . (lambda (lispy-mode 1)))
;;   :init
;;   (defun conditionally-enable-lispy ()
;;     (when (eq this-command 'eval-expression)
;;       (lispy-mode 1)))
;;   (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy))

(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook
    'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
  (defun override-slime-del-key ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key)
      nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-del-key))
(use-package slime
  :init (setq inferior-lisp-program "sbcl"))


;; hydra (rapid fire mnemonic keybindings) :: https://github.com/abo-abo/hydra
;; (use-package hydra
;;   :init
;;   (global-set-key (kbd "C-n")
;;     (defhydra
;;       hydra-move
;;       (:body-pre (next-line))
;;       "move"
;;       ("n" next-line)
;;       ("p" previous-line)
;;       ("f" forward-char)
;;       ("b" backward-char)
;;       ("a" beginning-of-line)
;;       ("e" move-end-of-line)
;;       ("v" scroll-up-command)
;;       ;; Converting M-v to V here by analogy.
;;       ("V" scroll-down-command)
;;       ("l" recenter-top-bottom)))
;;   (defhydra
;;     hydra-zoom
;;     (global-map "<f2>")
;;     "zoom"
;;     ("=" default-text-scale-increase "in")
;;     ("-" default-text-scale-decrease "out"))
;;   (defhydra
;;     hydra-buffer-menu
;;     (:color pink :hint nil)
;;     "
;; ^Mark^             ^Unmark^           ^Actions^          ^Search
;; ^^^^^^^^-----------------------------------------------------------------
;; _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
;; _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
;; _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
;; _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
;; _~_: modified
;; "
;;     ("m" Buffer-menu-mark)
;;     ("u" Buffer-menu-unmark)
;;     ("U" Buffer-menu-backup-unmark)
;;     ("d" Buffer-menu-delete)
;;     ("D" Buffer-menu-delete-backwards)
;;     ("s" Buffer-menu-save)
;;     ("~" Buffer-menu-not-modified)
;;     ("x" Buffer-menu-execute)
;;     ("b" Buffer-menu-bury)
;;     ("g" revert-buffer)
;;     ("T" Buffer-menu-toggle-files-only)
;;     ("O" Buffer-menu-multi-occur :color blue)
;;     ("I" Buffer-menu-isearch-buffers :color blue)
;;     ("R" Buffer-menu-isearch-buffers-regexp :color blue)
;;     ("c" nil "cancel")
;;     ("v" Buffer-menu-select "select" :color blue)
;;     ("o" Buffer-menu-other-window "other-window" :color blue)
;;     ("q" quit-window "quit" :color blue))

;;   (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body))

;; Load SSH / GPG keys from keychain agent
(use-package keychain-environment
  :straight
  (keychain-environment
    :type git
    :files (:defaults "keychain-environment")
    :host github
    :repo "tarsius/keychain-environment")
  :init (keychain-refresh-environment))

;; GIMP script-fu mode
;; (use-package gimp-mode
;;   :straight
;;   (gimp-mode
;;     :type git
;;     :host github
;;     :repo "enigmacurry/gimpmode"
;;     :build nil)
;;   :init (load "~/.emacs.d/straight/repos/gimpmode/gimp-init.el")
;;   ;; Uncomment following line to globally use `gimp-selector':
;;   ;; (global-set-key "\C-cg" 'gimp-selector)
;;   ;;Now you can run the GIMP with `M-x run-gimp'.
;;   ;;Alternatively, connect to GIMP server with `M-x gimp-cl-connect'.
;;   ;;  Type `M-x gimp-help' for help.
;;   )

;; yaml mode
(use-package yaml-mode)

;; prettier JS code formatter
;; must manually install: prettier and prettier-plugin-svelte
(use-package prettier-js
  :straight
  (prettier-js
    :type git
    :host github
    :repo "prettier/prettier-emacs"
    :build nil)
  :init
  (load "~/.emacs.d/straight/repos/prettier-emacs/prettier-js.el")
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

;(use-package eglot)

;; Go
;; must manually install: gopls
(use-package go-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook #'lsp-deferred)
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (lsp-register-custom-settings
    '(("gopls.completeUnimported" t t) ("gopls.staticcheck" t t))))

;; Rust
;; must manually install rust, rust-analyzer, cargo-watch, wasm-pack, wasm-bindgen, cargo-generate
(use-package rustic
  :init (setq rustic-format-on-save t))

;; C++
;; must manually install ccls
(use-package ccls
  :init
  ;; (setq-default indent-tabs-mode t)
  ;; (setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide
  ;; (defvaralias 'c-basic-offset 'tab-width)
  :hook
  ((c-mode c++-mode objc-mode cuda-mode)
    .
    (lambda () (require 'ccls) (lsp))))

;; WebGPU .wgsl mode
(use-package wgsl-mode
  :straight
  (wgsl-mode :type git :host github :repo "acowley/wgsl-mode"))

;; (use-package edit-server
;;   :ensure t
;;   :commands edit-server-start
;;   :init
;;   (if after-init-time
;;     (edit-serer-start)
;;     (add-hook 'after-init-hook #'(lambda () (edit-server-start))))
;;   :config
;;   (setq edit-server-new-frame-alist
;;     '
;;     ((name . "Edit with Emacs FRAME")
;;       (top . 200)
;;       (left . 200)
;;       (width . 80)
;;       (height . 25)
;;       (minibuffer . t)
;;       (menu-bar-lines . t)
;;       (window-system . x))))

;; atomic-chrome
;; (use-package atomic-chrome
;;   :init
;;   (require 'atomic-chrome)
;;   (atomic-chrome-start-server))

(use-package sqlformat
  :init
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g" "-u1"))
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun my-novel-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.5)
    (setq-local show-trailing-whitespace nil)
    )
  (add-hook 'nov-mode-hook 'my-novel-setup)
  )

(use-package nov)

;; typescript
(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))
