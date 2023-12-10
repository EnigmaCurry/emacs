;; -*- coding: utf-8 -*-
;; EnigmaCurry's emacs config
;; inspiration : https://github.com/susam/emfy
;;               https://emacs.amodernist.com
;;               https://emacsrocks.com/
;;               http://whattheemacsd.com/
;; elisp links: https://github.com/chrisdone/elisp-guide#readme
;;              https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html
;;              https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html

;; GUI defaults are loaded in early-init.el before init.el

;; Debug options:
;;; Start Emacs with the `--debug-init` argument, to debug errors during startup.
;;; Enter debugger on specific logger regex (see *Messages* buffer):
;; (setq debug-on-message "Example log message to trace")
;;; M-x toggle-debug-on-error
;; (setq debug-on-error t)

;; Profile startup time (minus early-init.el time) using profile-dotemacs.el:
;;; curl -O https://raw.githubusercontent.com/emacsmirror/emacswiki.org/ed647e999fd4942d1c0bed02abe75bdf20f42baf/profile-dotemacs.el
;;; emacs -Q -l .emacs.d/early-init.el -l profile-dotemacs.el --eval '(let ((profile-dotemacs-file "~/.emacs.d/init.el") (vc-follow-symlinks t)) (profile-dotemacs))'

;; Nice defaults
(setq confirm-kill-emacs #'yes-or-no-p)
(setq vc-follow-symlinks t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default visible-bell t)
(column-number-mode)
(put 'narrow-to-region 'disabled nil)
;; need this on fedora (?) ::
(setq-default native-comp-deferred-compilation-deny-list nil)

;; Show a vertical line after 80 columns (programming modes only):
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; Show line numbers (programming modes only)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)                          

;; choose your default web browser
;(setq-default browse-url-browser-function 'eww-browse-url)
(setq-default browse-url-browser-function 'browse-url-firefox)

;; unbind arrow keys and pgup/pgdwn to prevent bad habits and keep fingers on home row.
;;; This is hardcore.
;; (mapcar #'(lambda (k) (global-unset-key (kbd k)))
;;         '("<left>" "<right>" "<up>" "<down>" "<C-right>" "<C-up>" "<C-down>"
;;           "<M-left>" "<M-right>" "<M-up>" "<M-down>" "<prior>" "<next>"))

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

;; Switch between two most recent buffers:
(fset 'quick-switch-buffer [?\C-x ?b return])

;; Retain buffer cursor positions across Emacs sessions: 
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

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

;; Doom themes
;; https://github.com/doomemacs/themes/
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

;;; Pick a theme:
  ;;(load-theme 'doom-acario-dark t)
  ;;(load-theme 'doom-ir-black t)
  ;;(load-theme 'doom-old-hope t)
  (load-theme 'doom-rouge t)
  ;;(load-theme 'doom-1337 t)
  ;;(load-theme 'doom-tokyo-night t)
  ;;(load-theme 'doom-tomorrow-night t)
  ;;(load-theme 'doom-ayu-dark t)
  ;;(load-theme 'doom-challenger-deep t)
  ;;(load-theme 'doom-homage-black t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; General keybinding manager
;; https://github.com/noctuid/general.el#readme
;; https://emacsnotes.wordpress.com/2022/10/30/use-xkb-to-setup-full-spectrum-of-modifiers-meta-alt-super-and-hyper-for-use-with-emacs/
;; Two ways to show the bindings for the current buffer:
;;; Describe *all* bindings (including default bindings): C-h b
;;; Describe only the general.el configured bindings: C-h B
(use-package general
  :config
  ;;; Custom global bindings:
  (general-define-key
   "C-h B" 'general-describe-keybindings
   "H-b" 'quick-switch-buffer
   "H-B" 'buffer-menu "A-B" 'buffer-menu "C-x B" 'buffer-menu
   "H-o" 'browse-url
   "C-;" 'comment-region                ; C-u C-; to uncomment
   )
  ;;; Default keybindings you want included in general-describe-keybindings:
  ;;; Its useful to duplicate these simply as a way of documentation:
  (general-define-key
   "M-SPC" 'cycle-spacing)
;;; Define bindings for specific builtin (non use-package) modes:
  ;; Emacs Lisp mode bindings:
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   "A-e" 'eval-defun                    ;eval top-level form
   )
   ;; Dired mode bindings:
   (general-define-key
    :keymaps 'dired-mode-map
    "C-c C-q" 'dired-toggle-read-only))

;; Scale text sizes in all buffers :: https://github.com/purcell/default-text-scale
(use-package default-text-scale
  :general
  ("C-=" 'default-text-scale-increase
   "C--" 'default-text-scale-decrease)
  :init
  (setq default-text-scale-amount 5))

;; Smart line mode
;; https://github.com/Malabarba/smart-mode-line#readme
;; (use-package smart-mode-line
;;   :config
;;   :init
;;   (use-package smart-mode-line-powerline-theme
;;     :config
;;     (setq sml/theme 'powerline)
;;     )
;;   (sml/setup)
;;   )

;; Hide selected minor modes from the modeline:
(use-package diminish
  :init
  (mapcar #'(lambda (mode)
              (add-hook mode #'(lambda ()
                (mapcar #'(lambda (m) (diminish m) )
                        '(which-key-mode eldoc-mode ivy-mode paredit-mode)))))
          '(text-mode-hook prog-mode-hook special-mode-hook)))

;; Alternative M-x interface:
;; https://github.com/DarwinAwardWinner/amx
(use-package amx
  :general
  ("M-x" 'amx
   "<menu>" 'amx)
  )

;; Ivy / counsel (list-completion) :: https://oremacs.com/swiper/#introduction
(use-package counsel
  :general
  ("M-y" 'counsel-yank-pop
   "A-b" 'ivy-switch-buffer
   )
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t))

;; hydra (rapid fire mnemonic keybindings) :: https://github.com/abo-abo/hydra
(use-package hydra)

;; yasnippet templates
;; https://github.com/joaotavora/yasnippet
;; docs: https://joaotavora.github.io/yasnippet/
;; M-x yas-describe-tables to show the loaded snippets per mode
;; Put your snippets in ~/.emacs.d/snippets
(use-package yasnippet
  :init
  ;; Install a big snippet library:
  ;; https://github.com/AndreaCrotti/yasnippet-snippets
  (use-package yasnippet-snippets)
  ;;; You could enable yas globally:
  ;; (yas-global-mode 1)
  ;;; You could enable it just for all programming modes:
  ;; (yas-reload-all)
  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;;; Better to enable yas-minor-mode per mode you want it for, via use-package.
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  )

;; Org
(use-package org
  :after hydra
  :hook (org-mode . yas-minor-mode)
  :config
  (setq org-directory "~/org")
  (setq org-insert-mode-line-in-empty-file t)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . "/usr/bin/firefox %s")
          ("\\.pdf\\'" . default)))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/org/notes.org" "Journal")
           "* %?\nEntered on %U\n  %i\n  %a")))
  :init
  ;; Hydra for commonly used org commands:
  (defhydra hydra-org (global-map "C-c o" :exit t)
    "org"
    ("l" org-store-link "store link" )
    ("i" org-insert-link "insert link")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("m" org-info "read info manual")
    ("e" org-export-dispatch "export")
    ("p" org-preview-html-mode "toggle preview mode")
    ("s" org-insert-source-code-block "insert source code block"))

  ;; https://emacs.stackexchange.com/a/70606 thanks Chris!
  (defun org-insert-source-code-block(&optional language file)
    "Insert source code block for LANGUAGE.  Optionally pull in FILE contents.
Will prompt for LANGUAGE when called interactively.
With a `\\[universal-argument]' prefix, prompts for FILE.
The `:tangle FILE` header argument will be added when pulling in file contents."
    (interactive)
    (let ((col (current-column))
          (lang (or language (read-from-minibuffer "Source block language: ") ))
          (file (if current-prefix-arg (read-file-name "Enter file name: ") nil)))
      (insert
       (format "#+begin_src %s%s" lang (if file (concat " :tangle " file) "")))
      (newline)(newline)
      (move-to-column col t)(insert "#+end_src")(newline)
      (forward-line -2)(move-to-column col t)
      (if file (insert-file-contents file))))
  )
(use-package org-preview-html
  :after org
  )

(use-package ox-hugo
  :straight
  (ox-hugo
    :type git
    :host github
    :repo "enigmacurry/ox-mdbook"
    :local-repo "~/git/vendor/enigmacurry/ox-mdbook")
  :ensure t
  :after ox)

;; Magit (git version control system) :: https://magit.vc/
(use-package magit
  :general
  ("C-c g" 'magit-status)
  :config
  ;; open magit in a full frame always:
  (setq magit-display-buffer-function
    #'magit-display-buffer-fullframe-status-v1))

;; Avy (like ace-jump) :: https://github.com/abo-abo/avy
(use-package avy
  :general
  ("C-c s" 'avy-goto-word-1))

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
  (python-mode . yas-minor-mode)
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

;; Icons https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; Web mode :: https://github.com/fxbois/web-mode
(use-package web-mode
  :hook (web-mode . yas-minor-mode)
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
  :custom
  (vterm-always-compile-module t)
  :general
  ("C-c t" 'my-vterm-toggle)
  :init
  (defun my-vterm-toggle (&optional args)
    "Customized vterm-toggle wrapper- this fixes the universal argument (C-u) to always create a new terminal"
    (interactive "P")
    (if
        (not (or (derived-mode-p 'vterm-mode)
                 (and (vterm-toggle--get-window)
                      vterm-toggle-hide-method)))
        (if (equal current-prefix-arg '(4))
            (vterm-toggle--new args)
          (vterm-toggle args))
      (vterm-toggle args))))

;; shell-pop for vterm :: https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle)

;; ace-link (follow links in info docs) :: https://github.com/abo-abo/ace-link
(use-package ace-link
  :init (ace-link-setup-default))

;; jump between windows :: https://github.com/abo-abo/ace-window
(use-package ace-window
  :init
  (setq aw-scope 'frame)
  (global-set-key [remap other-window] 'ace-window)
  :general
  ("M-o" 'ace-window "A-o" 'ace-window "°" 'ace-window
   ;"C-x o" #'(lambda()(interactive) (message "Use M-o or A-o instead!"))
   ))

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
  (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
  (defun override-paredit-slurp-keys ()
    ;; Rebind the slurp keys to so as not to shadow the regular right-word and left-word bindings:
    (define-key paredit-mode-map (kbd "C-<right>") nil)
    (define-key paredit-mode-map (kbd "C-<left>") nil)
    (define-key paredit-mode-map (kbd "H-<right>") 'paredit-forward-barf-sexp)
    (define-key paredit-mode-map (kbd "H-<left>") 'paredit-forward-slurp-sexp))
  (add-hook 'paredit-mode-hook 'override-paredit-slurp-keys)
)
(use-package slime
  :init (setq inferior-lisp-program "sbcl"))


;; Clojure / CIDER
;;; install leiningen package
(use-package cider)

;; Lisp Flavoured Erlang (LFE)
(use-package lfe-mode
  :init
  (dolist (func '(paredit-mode rainbow-delimiters-mode))
  (add-hook 'lfe-mode-hook func)))

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
  :init
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook
            (lambda ()
              (define-key rustic-mode-map (kbd "C-c M-.") 'lsp-rust-analyzer-open-external-docs)))
)

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

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun my-novel-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.5)
    (setq-local show-trailing-whitespace nil)
    )
  (add-hook 'nov-mode-hook 'my-novel-setup)
  )

;; Docker
(use-package dockerfile-mode)
;(use-package docker-tramp)

;; Memes
;; Requires fonts: yay -S ttf-ms-fonts
(use-package meme
  :straight
  (meme :type git :host github :repo "larsmagne/meme"
        :fork
        (:host github :repo "enigmacurry/emacs-meme" :branch "dev")
        :files ("*"))
  :init
  (use-package imgur
    :straight
    (imgur :type git :host github :repo "larsmagne/imgur.el")
    :ensure t)
  )

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

(use-package nix-mode)

;; Godot
(use-package gdscript-mode
  :hook (gdscript-mode . eglot-ensure)
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode"))
(use-package gdshader-mode 
  :straight
  (gdshader-mode
   :type git :host github :repo "bbbscarter/gdshader-mode"))

;; Multiple cursors
;; https://emacsrocks.com/e13.html
(use-package multiple-cursors
  :general
  ("H-SPC" 'set-rectangular-region-anchor)
  ("H-n" 'mc/mark-next-like-this)
  ("H-N" 'mc/mark-all-like-this))

;; Eww browser
(use-package eww
  :init
  (setq-default show-trailing-whitespace nil)
  )

;; Insert placeholder text
(use-package lorem-ipsum)

;; Show keys in the modeline as they are pressed
;; disabled by default, M-x keycast-mode to start
(use-package keycast)

;; Matrix
(use-package ement
  :straight
  (ement :type git :host github :repo "alphapapa/ement.el"))

;; Put your local config into ~/.emacs.d/local/*.el
;; By default, it won't be saved in version control.
;; http://whattheemacsd.com/init.el-06.html
(let ((local-include-dir (concat user-emacs-directory "local" )))
  (if (file-exists-p local-include-dir)
      (mapc 'load (directory-files local-include-dir nil "^[^#].*el$"))
    (mkdir local-include-dir)))

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
