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
(defvar minimal-emacs-debug nil
  "Non-nil to enable debug.")
(defvar minimal-emacs--default-user-emacs-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")
(defun minimal-emacs-load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((user-init-file
         (expand-file-name filename
                           minimal-emacs--default-user-emacs-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

(minimal-emacs-load-user-init "pre-early-init.el")

;;; Variables

(defvar minimal-emacs-gc-cons-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")

;;; Misc

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.
(defvar minimal-emacs-default-gc-cons-threshold gc-cons-threshold
  "The default value of `gc-cons-threshold'.")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold (* 16 1024 1024))))

;;; Performance

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 128 1024))  ; 128kb

;; With this method, the redisplay process skips fontification (syntax
;; highlighting) while you are actively typing or performing other actions.
(setq redisplay-skip-fontification-on-input t)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

(unless (daemonp)
  (unless noninteractive
    ;; Without this, Emacs will try to resize itself to a specific column size
    (setq frame-inhibit-implied-resize t)

    ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
    ;; No second pass of case-insensitive search over auto-mode-alist.
    (setq auto-mode-case-fold nil)

    ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
    ;; dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (setq initial-buffer-choice nil
          inhibit-startup-buffer-menu t
          inhibit-x-resources t)

    ;; Disable bidirectional text scanning for a modest performance boost.
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)

    ;; Give up some bidirectional functionality for slightly faster re-display.
    (setq bidi-inhibit-bpa t)

    ;; Remove "For information about GNU Emacs..." message at startup
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; Suppress the vanilla startup screen completely. We've disabled it with
    ;; `inhibit-startup-screen', but it would still initialize anyway.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch buffer in
    ;; `fundamental-mode'
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)))

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

;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors minimal-emacs-debug
      native-comp-warning-on-missing-source minimal-emacs-debug)

(setq debug-on-error minimal-emacs-debug
      jka-compr-verbose minimal-emacs-debug)

(setq byte-compile-warnings minimal-emacs-debug)
(setq byte-compile-verbose minimal-emacs-debug)

;;; Simple UI

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;;; Disable unneeded UI elements

;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters can
;;   trigger/queue a superfluous (and expensive, depending on the window system)
;;   frame redraw at startup. The variables must be set to `nil' as well so
;;   users don't have to call the functions twice to re-enable them.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; (unless (memq window-system '(mac ns))
;;   (menu-bar-mode -1))
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;;; package: Set package archives for package installation
(progn
  (require 'package)

  ;; Since Emacs 27, package initialization occurs before `user-init-file' is
  ;; loaded, but after `early-init-file'.
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
  ;; (when package-enable-at-startup
  ;;   (package-initialize))
  )

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
