;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1.3.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The minimal-emacs.d project is a lightweight and optimized Emacs base
;; (init.el and early-init.el) that gives you full control over your
;; configuration. It provides better defaults, an optimized startup, and a clean
;; foundation for building your own vanilla Emacs setup.
;;
;; Building the minimal-emacs.d init.el and early-init.el was the result of
;; extensive research and testing to fine-tune the best parameters and
;; optimizations for an Emacs configuration.
;;
;; Do not modify this file; instead, modify pre-early-init.el or
;; post-early-init.el.

;;; Code:

;;; Internal variables

;; Backup of `gc-cons-threshold' and `gc-cons-percentage' before startup.
(defvar minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
(defvar minimal-emacs--backup-gc-cons-percentage gc-cons-percentage)

;; Temporarily raise the garbage collection threshold to its maximum value.
;; It will be restored later to controlled values.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

;;; Variables

(defvar minimal-emacs-ui-features '()
  "List of user interface features to enable in minimal Emacs setup.
This variable holds a list of Emacs UI features that can be enabled:
- context-menu (Enables the context menu in graphical environments.)
- tool-bar (Enables the tool bar in graphical environments.)
- menu-bar (Enables the menu bar in graphical environments.)
- dialogs (Enables both file dialogs and dialog boxes.)
- tooltips (Enables tooltips.)")

(defvar minimal-emacs-frame-title-format "%b – Emacs"
  "Template for displaying the title bar of visible and iconified frame.")

(defvar minimal-emacs-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

(defvar minimal-emacs-optimize-startup-gc t
  "If non-nil, increase `gc-cons-threshold' during startup to reduce pauses.
After Emacs finishes loading, `gc-cons-threshold' is restored to the value
stored in `minimal-emacs-gc-cons-threshold'.")

(defvar minimal-emacs-gc-cons-threshold (* 32 1024 1024)
  "Value to which `gc-cons-threshold' is set after Emacs startup.
Ignored if `minimal-emacs-optimize-startup-gc' is nil.")

(defvar minimal-emacs-gc-cons-percentage gc-cons-percentage
  "Value to which `gc-cons-percentage' is set after Emacs startup.
Ignored if `minimal-emacs-optimize-startup-gc' is nil.")

(defvar minimal-emacs-gc-cons-threshold-restore-delay nil
  "Number of seconds to wait before restoring `gc-cons-threshold'.")

(defvar minimal-emacs-optimize-file-name-handler-alist t
  "Enable optimization of `file-name-handler-alist'.
When non-nil, this variable activates optimizations to reduce file name handler
lookups during Emacs startup.")

(defvar minimal-emacs-disable-mode-line-during-startup t
  "Disable the mode line during startup.
This reduces visual clutter and slightly enhances startup performance. The
tradeoff is that the mode line is hidden during the startup phase.")

(defvar minimal-emacs-package-initialize-and-refresh t
  "Whether to automatically initialize and refresh packages.
When set to non-nil, Emacs will automatically call `package-initialize' and
`package-refresh-contents' to set up and update the package system.")

(defvar minimal-emacs-setup-native-compilation t
  "Controls whether native compilation settings are enabled during setup.
When non-nil, the following variables are set to non-nil to enable
native compilation features:
- `native-comp-deferred-compilation'
- `native-comp-jit-compilation'
- `package-native-compile'
If nil, these variables are left at their default values and are not
modified during setup.")

(defvar minimal-emacs-inhibit-redisplay-during-startup nil
  "Suppress redisplay during startup to improve performance.
This prevents visual updates while Emacs initializes. The tradeoff is that you
won't see the progress or activities during the startup process.")

(defvar minimal-emacs-inhibit-message-during-startup nil
  "Suppress startup messages for a cleaner experience.
This slightly enhances performance. The tradeoff is that you won't be informed
of the progress or any relevant activities during startup.")

(defvar minimal-emacs-user-directory user-emacs-directory
  "Directory beneath minimal-emacs.d files are placed.
Note that this should end with a directory separator.")

;;; Load pre-early-init.el

;; Prefer loading newer compiled files
(setq load-prefer-newer t)
(setq debug-on-error minimal-emacs-debug)

(defvar minimal-emacs--success nil)
(defun minimal-emacs--check-success ()
  "Verify that the Emacs configuration has loaded successfully."
  (unless minimal-emacs--success
    (cond
     ((or (file-exists-p (expand-file-name "~/.emacs.el"))
          (file-exists-p (expand-file-name "~/.emacs")))
      (error "Emacs ignored loading 'init.el'. Please ensure that files such as ~/.emacs or ~/.emacs.el do not exist, as they may be preventing Emacs from loading the 'init.el' file"))

     (t
      (error "Configuration error. Debug by starting Emacs with: emacs --debug-init")))))
(add-hook 'emacs-startup-hook #'minimal-emacs--check-success 102)

(defvar minimal-emacs-load-compiled-init-files nil
  "If non-nil, attempt to load byte-compiled .elc for init files.
This will enable minimal-emacs to load byte-compiled or possibly native-compiled
init files for the following initialization files: pre-init.el, post-init.el,
pre-early-init.el, and post-early-init.el.")

(defun minimal-emacs--remove-el-file-suffix (filename)
  "Remove the Elisp file suffix from FILENAME and return it (.el, .el.gz...)."
  (let ((suffixes (mapcar (lambda (ext) (concat ".el" ext))
                          load-file-rep-suffixes)))
    (catch 'done
      (dolist (suffix suffixes filename)
        (when (string-suffix-p suffix filename)
          (setq filename (substring filename 0 (- (length suffix))))
          (throw 'done t))))
    filename))

(defun minimal-emacs-load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((init-file (expand-file-name filename
                                     minimal-emacs-user-directory)))
    (if (not minimal-emacs-load-compiled-init-files)
        (load init-file :no-error (not init-file-debug) :nosuffix)
      ;; Remove the file suffix (.el, .el.gz, etc.) to let the `load' function
      ;; select between .el and .elc files.
      (setq init-file (minimal-emacs--remove-el-file-suffix init-file))
      (load init-file :no-error (not init-file-debug)))))

(minimal-emacs-load-user-init "pre-early-init.el")

(setq custom-theme-directory
      (expand-file-name "themes/" minimal-emacs-user-directory))

(setq custom-file (expand-file-name "custom.el" minimal-emacs-user-directory))

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.

(setq garbage-collection-messages minimal-emacs-debug)

(defun minimal-emacs--restore-gc-values ()
  "Restore garbage collection values to minimal-emacs.d values."
  (setq gc-cons-threshold minimal-emacs-gc-cons-threshold)
  (setq gc-cons-percentage minimal-emacs-gc-cons-percentage))

(defun minimal-emacs--restore-gc ()
  "Restore garbage collection settings."
  (if (bound-and-true-p minimal-emacs-gc-cons-threshold-restore-delay)
      ;; Defer garbage collection during initialization to avoid 2 collections.
      (run-with-timer minimal-emacs-gc-cons-threshold-restore-delay nil
                      #'minimal-emacs--restore-gc-values)
    (minimal-emacs--restore-gc-values)))

(if minimal-emacs-optimize-startup-gc
    ;; `gc-cons-threshold' is managed by minimal-emacs.d
    (add-hook 'emacs-startup-hook #'minimal-emacs--restore-gc 105)
  ;; gc-cons-threshold is not managed by minimal-emacs.d.
  (when (= gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-threshold minimal-emacs--backup-gc-cons-threshold)
    (setq gc-cons-percentage minimal-emacs--backup-gc-cons-percentage)))

;;; Native compilation and Byte compilation

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (when minimal-emacs-setup-native-compilation
      ;; Activate `native-compile'
      (setq package-native-compile t))
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(setq native-comp-warning-on-missing-source minimal-emacs-debug
      native-comp-async-report-warnings-errors (or minimal-emacs-debug 'silent))

(setq jka-compr-verbose minimal-emacs-debug)
(setq byte-compile-warnings minimal-emacs-debug
      byte-compile-verbose minimal-emacs-debug)

;;; Miscellaneous

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb

(setq process-adaptive-read-buffering nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(setq warning-minimum-level (if minimal-emacs-debug :warning :error))
(setq warning-suppress-types '((lexical-binding)))

(when minimal-emacs-debug
  (setq message-log-max 16384))

;; In PGTK, this timeout introduces latency. Reducing it from the default 0.1
;; improves responsiveness of childframes and related packages.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;;; Performance: Miscellaneous options

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(when (and (not (daemonp)) (not noninteractive))
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

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
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  ;; The initial buffer is created during startup even in non-interactive
  ;; sessions, and its major mode is fully initialized. Modes like `text-mode',
  ;; `org-mode', or even the default `lisp-interaction-mode' load extra packages
  ;; and run hooks, which can slow down startup.
  ;;
  ;; Using `fundamental-mode' for the initial buffer to avoid unnecessary
  ;; startup overhead.
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  (unless minimal-emacs-debug
    ;; Unset command line options irrelevant to the current OS. These options
    ;; are still processed by `command-line-1` but have no effect.
    (unless (eq system-type 'darwin)
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

;;; Performance: File-name-handler-alist

(defvar minimal-emacs--old-file-name-handler-alist (default-toplevel-value
                                                    'file-name-handler-alist))

(defun minimal-emacs--respect-file-handlers (fn args-left)
  "Respect file handlers.
FN is the function and ARGS-LEFT is the same argument as `command-line-1'.
Emacs processes command-line files very early in startup. These files may
include special paths like TRAMP paths, so restore `file-name-handler-alist' for
this stage of initialization."
  (let ((file-name-handler-alist (if args-left
                                     minimal-emacs--old-file-name-handler-alist
                                   file-name-handler-alist)))
    (funcall fn args-left)))

(defun minimal-emacs--restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist'."
  (set-default-toplevel-value
   'file-name-handler-alist
   ;; Merge instead of overwrite to preserve any changes made since startup.
   (delete-dups (append file-name-handler-alist
                        minimal-emacs--old-file-name-handler-alist))))

(when (and minimal-emacs-optimize-file-name-handler-alist
           (not (daemonp))
           (not minimal-emacs-debug))
  ;; Determine the state of bundled libraries using calc-loaddefs.el. If
  ;; compressed, retain the gzip handler in `file-name-handler-alist`. If
  ;; compiled or neither, omit the gzip handler during startup for improved
  ;; startup and package load time.
  (set-default-toplevel-value
   'file-name-handler-alist
   (if (locate-file-internal "calc-loaddefs.el" load-path)
       nil
     (list (rassq 'jka-compr-handler
                  minimal-emacs--old-file-name-handler-alist))))

  ;; Ensure the new value persists through any current let-binding.
  (put 'file-name-handler-alist 'initial-value
       minimal-emacs--old-file-name-handler-alist)

  ;; Emacs processes command-line files very early in startup. These files may
  ;; include special paths TRAMP. Restore `file-name-handler-alist'.
  (advice-add 'command-line-1 :around #'minimal-emacs--respect-file-handlers)

  (add-hook 'emacs-startup-hook #'minimal-emacs--restore-file-name-handler-alist
            101))

;;; Performance: Inhibit redisplay

(defun minimal-emacs--reset-inhibit-redisplay ()
  "Reset inhibit redisplay."
  (setq-default inhibit-redisplay nil)
  (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibit-redisplay))

(when (and minimal-emacs-inhibit-redisplay-during-startup
           (not (daemonp))
           (not noninteractive)
           (not minimal-emacs-debug))
  ;; Suppress redisplay and redraw during startup to avoid delays and
  ;; prevent flashing an unstyled Emacs frame.
  (setq-default inhibit-redisplay t)
  (add-hook 'post-command-hook #'minimal-emacs--reset-inhibit-redisplay -100))

;;; Performance: Inhibit message

(defun minimal-emacs--reset-inhibit-message ()
  "Reset inhibit message."
  (setq-default inhibit-message nil)
  (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibit-message))

(when (and minimal-emacs-inhibit-message-during-startup
           (not (daemonp))
           (not noninteractive)
           (not minimal-emacs-debug))
  (setq-default inhibit-message t)
  (add-hook 'post-command-hook #'minimal-emacs--reset-inhibit-message -100))

;;; Performance: Disable mode-line during startup

(when (and minimal-emacs-disable-mode-line-during-startup
           (not (daemonp))
           (not noninteractive)
           (not minimal-emacs-debug))
  (put 'mode-line-format
       'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format nil))))

;;; Restore values

(defun minimal-emacs--startup-load-user-init-file (fn &rest args)
  "Advice to reset `mode-line-format'. FN and ARGS are the function and args."
  (unwind-protect
      ;; Start up as normal
      (apply fn args)
    ;; If we don't undo inhibit-{message, redisplay} and there's an error, we'll
    ;; see nothing but a blank Emacs frame.
    (when minimal-emacs-inhibit-message-during-startup
      (setq-default inhibit-message nil))
    (when minimal-emacs-inhibit-redisplay-during-startup
      (setq-default inhibit-redisplay nil))
    ;; Restore the mode-line
    (when minimal-emacs-disable-mode-line-during-startup
      (unless (default-toplevel-value 'mode-line-format)
        (setq-default mode-line-format (get 'mode-line-format
                                            'initial-value))))))

(advice-add 'startup--load-user-init-file :around
            #'minimal-emacs--startup-load-user-init-file)

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

(defun minimal-emacs--setup-toolbar (&rest _)
  "Setup the toolbar."
  (when (fboundp 'tool-bar-setup)
    (advice-remove 'tool-bar-setup #'ignore)
    (when (bound-and-true-p tool-bar-mode)
      (funcall 'tool-bar-setup))))

(when (and (not (daemonp))
           (not noninteractive))
  (when (fboundp 'tool-bar-setup)
    ;; Temporarily override the tool-bar-setup function to prevent it from
    ;; running during the initial stages of startup
    (advice-add 'tool-bar-setup :override #'ignore)

    (advice-add 'startup--load-user-init-file :after
                #'minimal-emacs--setup-toolbar)))

(unless (memq 'tool-bar minimal-emacs-ui-features)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (setq tool-bar-mode nil))

(setq default-frame-scroll-bars 'right)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)

(unless (memq 'tooltips minimal-emacs-ui-features)
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(unless (memq 'dialogs minimal-emacs-ui-features)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil))

;;; Security
(setq gnutls-verify-error t)  ; Prompts user if there are certificate issues
(setq tls-checktrust t)  ; Ensure SSL/TLS connections undergo trust verification
(setq gnutls-min-prime-bits 3072)  ; Stronger GnuTLS encryption

;;; package.el
(setq use-package-compute-statistics minimal-emacs-debug)

;; Setting use-package-expand-minimally to (t) results in a more compact output
;; that emphasizes performance over clarity.
(setq use-package-expand-minimally (not minimal-emacs-debug))

(setq package-quickstart-file
      (expand-file-name "package-quickstart.el" user-emacs-directory))
(setq use-package-minimum-reported-time (if minimal-emacs-debug 0 0.1))
(setq use-package-verbose minimal-emacs-debug)
(setq package-enable-at-startup nil)  ; Let the init.el file handle this
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 70)
                                   ("melpa-stable" . 50)))

;;; Load post-early-init.el
(minimal-emacs-load-user-init "post-early-init.el")

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
