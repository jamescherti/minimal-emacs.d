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
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  ;; (gcmh-high-cons-threshold (* 16 1024 1024))
  :hook
  (emacs-startup . gcmh-mode))

;;; Uniquify - Unique buffer name
(progn
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "•")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*"))

;;; Files
(progn
  ;; Do not auto-disable auto-save after deleting large chunks of text. The
  ;; purpose of auto-save is to provide a failsafe, and disabling it
  ;; contradicts this objective.
  (setq auto-save-include-big-deletions t)

  ;; Auto save options
  (setq kill-buffer-delete-auto-save-files t)

  ;; Do not save BackupFiles under the original name with a tilde `~’
  (setq backup-by-copying t) ; Backup by copying rather renaming
  (setq delete-old-versions t) ; Delete excess backup versions silently
  (setq make-backup-files nil)
  (setq version-control t))

;;; Subr
(progn
  ;; Allow for shorter responses: "y" for yes and "n" for no.
  (defalias #'yes-or-no-p 'y-or-n-p)
  ;; Never show the hello file
  (defalias #'view-hello-file #'ignore))

;;; Mule-util
(setq truncate-string-ellipsis "…")

;;; Frames and windows

(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Do not resize the frames in steps. It can leave gaps.
(setq frame-resize-pixelwise t)

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

;;; Buffer
(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Do not show an arrow at the top/bottomin the fringe and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

(setq-default word-wrap t)

;;; Smooth scrolling

;; Enables faster scrolling through unfontified regions. This may result in
;; brief periods of inaccurate syntax highlighting immediately after scrolling,
;; which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends excessive time recentering the screen when the cursor
      ;; moves more than N lines past the window edges (where N is the value of
      ;; `scroll-conservatively'). This can be particularly slow in larger
      ;; files during extensive scrolling. If `scroll-conservatively` is set
      ;; above 100, the window is never automatically recentered. The default
      ;; value (0) triggers recentering too aggressively, so I've set it to 10
      ;; to recenter the window only when scrolling significantly off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Minimize cursor lag slightly by preventing automatic adjustment of
      ;; `window-vscroll' for tall lines.
      auto-window-vscroll nil
      ;; Mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;;; Cursor
;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
;; Additionally, it can cause freezing, especially on macOS, for users with
;; customized and colored cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;;; Annoyances

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;;; Minimal UI

;; Show feedback after typing
(setq echo-keystrokes 0.02)

;; Disable GUIs because theyr are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;;; Performance

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(provide 'init)

;;; Load user-pre-init.el
(minimal-emacs-load-user-init "post-init.el")

;;; init.el ends here
