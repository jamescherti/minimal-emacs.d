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

;;; package: Set package archives for package installation
(progn
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package-quickstart nil)

  (when (version< emacs-version "28")
    (add-to-list 'package-archives
                 '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives
               '("stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))

  (customize-set-variable 'package-archive-priorities
                          '(("gnu"    . 99)
                            ("nongnu" . 80)
                            ("stable" . 70)
                            ("melpa"  . 0)))
  (when package-enable-at-startup
    (package-initialize)))

;;; use-package:
(progn
  ;; Always ensure packages are installed
  (setq use-package-always-ensure t)

  ;; Ensure the 'use-package' package is installed
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  ;; Load use-package for package configuration
  (eval-when-compile
    (require 'use-package)))

;;; Load user-init.el
(let ((user-init-file (expand-file-name "user-init.el"
                                        user-emacs-directory)))
  (when (file-exists-p user-init-file)
    (load user-init-file)))

(provide 'init)

;;; init.el ends here
