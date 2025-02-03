(defgroup my/path-settings nil
  "My custom path settings"
  :group 'my/custom-settings)
(defcustom my/git-vendor-directory "~/git/vendor"
  "My git vendor directory"
  :type 'string
  :group 'my/path-settings)
(defcustom my/git-user-directory "~/git/vendor/enigmacurry"
  "My personal git repositories directory"
  :type 'string
  :group 'my/path-settings)

(set-register ?e `(file . ,(expand-file-name "emacs.org" user-emacs-directory)))
(set-register ?g `(file . ,my/git-user-directory))
(set-register ?d `(file . ,(expand-file-name "enigmacurry/d.rymcg.tech/" my/git-vendor-directory)))
(set-register ?v `(file . ,my/git-vendor-directory))

(defun my/register-save-f1 () (interactive) (point-to-register 'my/register-f1))
(defun my/register-jump-f1 () (interactive) (jump-to-register 'my/register-f1))
(defun my/register-save-f2 () (interactive) (point-to-register 'my/register-f2))
(defun my/register-jump-f2 () (interactive) (jump-to-register 'my/register-f2))
(defun my/register-save-f3 () (interactive) (point-to-register 'my/register-f3))
(defun my/register-jump-f3 () (interactive) (jump-to-register 'my/register-f3))
(defun my/register-save-f4 () (interactive) (point-to-register 'my/register-f4))
(defun my/register-jump-f4 () (interactive) (jump-to-register 'my/register-f4))
