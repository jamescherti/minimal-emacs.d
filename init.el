;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; This is the main initialization file for Emacs. It configures package
;; archives, ensures essential packages like `use-package` are installed, and
;; sets up further package management and customization settings.

;;; Code:

;;; Load user-pre-init.el
(minimal-emacs-load-user-init "pre-init.el")

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package gcmh
  :hook
  (emacs-startup . gcmh-mode))

;;; Load user-pre-init.el
(minimal-emacs-load-user-init "post-init.el")

(provide 'init)

;;; init.el ends here
