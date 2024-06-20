;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; This file contains early initialization settings for Emacs. It is designed
;; to optimize the startup process and configure essential settings before the
;; main initialization.

;;; Code:

;;; Load pre-early-init.el
(defvar minimal-emacs--default-user-emacs-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")
(defun minimal-emacs-load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((user-init-file
         (expand-file-name filename
                           minimal-emacs--default-user-emacs-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file))))

(minimal-emacs-load-user-init "pre-early-init.el")

;;; Variables

(defvar minimal-emacs-gc-cons-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.
(defvar minimal-emacs-default-gc-cons-threshold gc-cons-threshold
  "The default value of `gc-cons-threshold'.")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold (* 16 1024 1024))))

;;; file-name-handler-alist

;; During startup, Emacs will process every opened and loaded file through this
;; list to identify a suitable handler. However, at that point, it won't
;; require any of them.
(defvar minimal-emacs--default-file-name-handler-alist file-name-handler-alist
  "The default value of the `file-name-handler-alist' variable.")
(setq file-name-handler-alist nil)
(defun minimal-emacs--restore-file-name-handler-alist ()
  "Restore the variables that were modified by `early-init.el'."
  (setq file-name-handler-alist minimal-emacs--default-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'minimal-emacs--restore-file-name-handler-alist)

;; Byte comp
(setq load-prefer-newer t)  ; Prefer loading newer compiled files

;; Native comp
(defvar minimal-emacs-native-comp-reserved-cpus 2
  "Number of CPUs to reserve and not use for `native-compile'.")

(defun minimal-emacs-calculate-native-comp-async-jobs ()
  "Set `native-comp-async-jobs-number' based on the available CPUs."
  ;; The `num-processors' function is only available in Emacs >= 28.1
  (max 1 (- (num-processors) minimal-emacs-native-comp-reserved-cpus)))

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-async-jobs-number
          (minimal-emacs-calculate-native-comp-async-jobs)
          native-comp-deferred-compilation t
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

;;; Simple UI

;; Disable startup screens and messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)

;;; Disable unneeded UI elements
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;;; package: Set package archives for package installation
(progn
  (require 'package)
  (setq package-enable-at-startup t)
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

;;; Load post-early-init.el
(minimal-emacs-load-user-init "post-early-init.el")

(provide 'early-init)

;;; early-init.el ends here
