;;; epa-setup.el --- setup routine for the EasyPG Assistant.
;; Copyright (C) 2006,2007,2008 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG

;; This file is part of EasyPG.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(autoload 'epa-list-keys "epa")

(autoload 'epa-dired-mode-hook "epa-dired")
(add-hook 'dired-mode-hook 'epa-dired-mode-hook)

(require 'epa-file)
(epa-file-enable)

(autoload 'epa-mail-mode "epa-mail")
(add-hook 'mail-mode-hook 'epa-mail-mode)

(provide 'epa-setup)

;;; epa-setup.el ends here