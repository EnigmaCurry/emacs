(my/add-exec-path "~/.local/bin")
(defun my/python-install-uv-package-manager ()
  "Install uv package manager via cargo"
  (my/cargo-install '(("uv" . "https://github.com/astral-sh/uv"))))
(defun my/python-uv-execute (command)
  (my/python-install-uv-package-manager)
  (my/shell-execute command))
(defun my/python-install-ruff ()
  (unless (executable-find "ruff")
    (my/python-uv-execute "uv tool install ruff@latest")))
(use-package
  python-mode
  :general ("s-a" 'lsp-execute-code-action)
  :init (my/python-install-ruff)
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . flycheck-mode)
  (python-mode . company-mode)
  (python-mode . python-black-on-save-mode)
  :custom (python-shell-interpreter "python3")
  :config
  ;; Activate python virtualenv BEFORE opening a python buffer and/or starting pyright server:
  ;; M-x pyvenv-activate     (~/.virtualenvs/XXX)
  (use-package
    pyvenv
    :init (setenv "WORKON_HOME" "~/.virtualenvs/")
    :config
    ;; (pyvenv-mode t)
    ;; Set correct Python interpreter
    (setq pyvenv-post-activate-hooks
          (list
           (lambda ()
             (setq python-shell-interpreter
                   (concat pyvenv-virtual-env "bin/python")))))
    (setq pyvenv-post-deactivate-hooks
          (list (lambda () (setq python-shell-interpreter "python3")))))
  ;; Black (Python code formatter) :: https://github.com/wbolster/emacs-python-black
  ;; Note: this depends on black being installed in the project virtualenv as a dev dependency
  (use-package python-black :demand t :after python)
  ;; Python dev dependencies need to be installed in your project's virtualenv:
  ;; ruff
  ;; ruff-lsp
  ;; black
 ;;; Add the following to a .dir-locals.el to activate virtualenv automatically:
  ;; ((python-mode . ((eval . (let ((project-root (locate-dominating-file
  ;;                              (or (buffer-file-name) default-directory)
  ;;                                ".dir-locals.el")))
  ;;               (pyvenv-activate (expand-file-name "virtualenv" project-root)))))))
  )
