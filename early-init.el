;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1.1.1
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The minimal-emacs.d project is a customizable base that provides better Emacs
;; defaults and optimized startup, intended to serve as a solid foundation for
;; your vanilla Emacs configuration.

;;; Code:

;;; Variables

(defvar minimal-emacs-ui-features '()
  "List of user interface features to disable in minimal Emacs setup.

This variable holds a list Emacs UI features that can be enabled:
- `context-menu`: Enables the context menu in graphical environments.
- `tool-bar`: Enables the tool bar in graphical environments.
- `menu-bar`: Enables the menu bar in graphical environments.
- `dialogs`: Enables both file dialogs and dialog boxes.
- `tooltips`: Enables tooltips.

Each feature in the list corresponds to a specific UI component that can be
turned on.")

(defvar minimal-emacs-frame-title-format "%b – Emacs"
  "Template for displaying the title bar of visible and iconified frame.")

(defvar minimal-emacs-debug nil
  "Non-nil to enable debug.")

(defvar minimal-emacs-gc-cons-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")

(defvar minimal-emacs-package-initialize-and-refresh t
  "Whether to automatically initialize and refresh packages.
When set to non-nil, Emacs will automatically call `package-initialize' and
`package-refresh-contents' to set up and update the package system.")

(defvar minimal-emacs-user-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")

;;; Load pre-early-init.el

(defun minimal-emacs-load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((user-init-file
         (expand-file-name filename
                           minimal-emacs-user-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

(minimal-emacs-load-user-init "pre-early-init.el")

(setq custom-theme-directory
      (expand-file-name "themes/" minimal-emacs-user-directory))
(setq custom-file (expand-file-name "custom.el" minimal-emacs-user-directory))

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold minimal-emacs-gc-cons-threshold)))

;;; Misc

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;;; Performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; Determine the state of bundled libraries using calc-loaddefs.el.
     ;; If compressed, retain the gzip handler in `file-name-handler-alist`.
     ;; If compiled or neither, omit the gzip handler during startup for
     ;; improved startup and package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Ensure the new value persists through any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist
                                file-name-handler-alist)
    ;; Remember the old value to reset it as needed.
    (add-hook 'emacs-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite to preserve any changes made
                 ;; since startup.
                 (delete-dups (append file-name-handler-alist old-value))))
              101))

  (unless noninteractive
    (unless minimal-emacs-debug
      (unless minimal-emacs-debug
        ;; Suppress redisplay and redraw during startup to avoid delays and
        ;; prevent flashing an unstyled Emacs frame.
        ;; (setq-default inhibit-redisplay t) ; Can cause artifacts
        (setq-default inhibit-message t)

        ;; Reset the above variables to prevent Emacs from appearing frozen or
        ;; visually corrupted after startup or if a startup error occurs.
        (defun minimal-emacs--reset-inhibited-vars-h ()
          ;; (setq-default inhibit-redisplay nil) ; Can cause artifacts
          (setq-default inhibit-message nil)
          (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibited-vars-h))

        (add-hook 'post-command-hook
                  #'minimal-emacs--reset-inhibited-vars-h -100))

      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (setq mode-line-format nil)))

      (put 'mode-line-format 'initial-value
           (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)

      (defun minimal-emacs--startup-load-user-init-file (fn &rest args)
        "Advice for startup--load-user-init-file to reset mode-line-format."
        (unwind-protect
            (progn
              ;; Start up as normal
              (apply fn args))
          ;; If we don't undo inhibit-{message, redisplay} and there's an
          ;; error, we'll see nothing but a blank Emacs frame.
          (setq-default inhibit-message nil)
          (unless (default-toplevel-value 'mode-line-format)
            (setq-default mode-line-format
                          (get 'mode-line-format 'initial-value)))))

      (advice-add 'startup--load-user-init-file :around
                  #'minimal-emacs--startup-load-user-init-file))

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
          initial-scratch-message nil)

    (unless minimal-emacs-debug
      ;; Unset command line options irrelevant to the current OS. These options
      ;; are still processed by `command-line-1` but have no effect.
      (unless (eq system-type 'darwin)
        (setq command-line-ns-option-alist nil))
      (unless (memq initial-window-system '(x pgtk))
        (setq command-line-x-option-alist nil)))))

;;; Native compilation and Byte compilation

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-jit-compilation t
          native-comp-deferred-compilation t  ; Obsolete since Emacs 29.1
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors
      (or minimal-emacs-debug 'silent))
(setq native-comp-warning-on-missing-source minimal-emacs-debug)

(setq debug-on-error minimal-emacs-debug
      jka-compr-verbose minimal-emacs-debug)

(setq byte-compile-warnings minimal-emacs-debug)
(setq byte-compile-verbose minimal-emacs-debug)

;;; UI elements

(setq frame-title-format minimal-emacs-frame-title-format
      icon-title-format minimal-emacs-frame-title-format)

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because manipulating frame parameters can trigger or queue
;; a superfluous and potentially expensive frame redraw at startup, depending
;; on the window system. The variables must also be set to `nil' so users don't
;; have to call the functions twice to re-enable them.
(unless (memq 'menu-bar minimal-emacs-ui-features)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (unless (memq window-system '(mac ns))
    (setq menu-bar-mode nil)))

(unless (daemonp)
  (unless noninteractive
    (when (fboundp 'tool-bar-setup)
      ;; Temporarily override the tool-bar-setup function to prevent it from
      ;; running during the initial stages of startup
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file
          (:after (&rest _) minimal-emacs-setup-toolbar)
        (advice-remove #'tool-bar-setup #'ignore)
        (when tool-bar-mode
          (tool-bar-setup))))))
(unless (memq 'tool-bar minimal-emacs-ui-features)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (setq tool-bar-mode nil))

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(unless (memq 'tooltips minimal-emacs-ui-features)
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(unless (memq 'dialogs minimal-emacs-ui-features)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil))

;;; package.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("stable" . 70)
                                                      ("melpa"  . 0)))

;;; Load post-early-init.el
(minimal-emacs-load-user-init "post-early-init.el")

(provide 'early-init)

;;; early-init.el ends here
