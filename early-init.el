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

;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold (* 16 1024 1024))))

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Disable startup screens and messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)

;; Disable unneeded UI elements
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

(provide 'early-init)

;;; early-init.el ends here
