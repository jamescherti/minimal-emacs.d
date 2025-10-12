;;; init.el --- Init -*- lexical-binding: t; -*-

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
;; Do not modify this file; instead, modify pre-init.el or post-init.el.

;;; Code:

;;; Load pre-init.el
(if (fboundp 'minimal-emacs-load-user-init)
    (minimal-emacs-load-user-init "pre-init.el")
  (error "The early-init.el file failed to loaded"))

;;; Before package

;; Ask the user whether to terminate asynchronous compilations on exit.
;; This prevents native compilation from leaving temporary files in /tmp.
(setq native-comp-async-query-on-exit t)

;; Allow for shorter responses: "y" for yes and "n" for no.
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;; Undo/redo

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;;; package.el

(when (bound-and-true-p minimal-emacs-package-initialize-and-refresh)
  ;; Initialize and refresh package contents again if needed
  (package-initialize)
  ;; Install use-package if necessary
  (unless (package-installed-p 'use-package)
    (unless (seq-empty-p package-archive-contents)
      (package-refresh-contents))
    (package-install 'use-package))

  ;; Ensure use-package is available
  (require 'use-package))

;;; Minibuffer

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; User interface

;; By default, Emacs "updates" its ui more often than it needs to
(setq which-func-update-delay 1.0)
(setq idle-update-delay which-func-update-delay)  ;; Obsolete in >= 30.1

(defalias #'view-hello-file #'ignore)  ; Never show the hello file

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;;; Show-paren

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Misc

(setq custom-buffer-done-kill t)

(setq whitespace-line-column nil)  ; Use the value of `fill-column'.

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setq truncate-string-ellipsis "…")

;; Disable truncation of printed s-expressions in the message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

(setq tramp-verbose 1)
(setq tramp-completion-reread-directory-timeout 50)
(setq remote-file-name-inhibit-cache 50)

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings
(setq imenu-max-item-length 160)

;; Disable auto-adding a new line at the bottom when scrolling.
(setq next-line-add-newlines nil)

;; This setting forces Emacs to save bookmarks immediately after each change.
;; Benefit: you never lose bookmarks if Emacs crashes.
(setq bookmark-save-flag 1)

;;; Files

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Ignoring this is acceptable since it will redirect to the buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks so that operations are conducted from the file's directory
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Prefer vertical splits over horizontal ones
(setq split-width-threshold 170
      split-height-threshold nil)

;;; Buffers

(setq uniquify-buffer-name-style 'forward)

;;; comint (general command interpreter in a window)

(setq ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096)

;;; Compilation

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

;;; Backup files

;; Avoid backups or lockfiles to prevent creating world-readable copies of files
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)  ; Backup by copying rather renaming
(setq delete-old-versions t)  ; Delete excess backup versions silently
(setq version-control t)  ; Use version numbers for backup files
(setq kept-new-versions 5)
(setq kept-old-versions 5)

;;; VC

(setq vc-git-print-log-follow t)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files
(setq vc-git-diff-switches '("--histogram"))  ; Faster algorithm for diffing.

;;; Auto save

;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-default nil)
(setq auto-save-no-message t)

;; Do not auto-disable auto-save after deleting large chunks of
;; text.
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))  ; Resolve issue #29

;;; recentf

;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'mode)
(setq recentf-exclude nil)

;;; saveplace

;; Enables Emacs to remember the last location within a file upon reopening.
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

;;; savehist

;; `savehist-mode' is an Emacs feature that preserves the minibuffer history
;; between sessions.
(setq history-length 300)
(setq savehist-save-minibuffer-history t)  ;; Default
(setq savehist-additional-variables
      '(kill-ring                        ; clipboard
        register-alist                   ; macros
        mark-ring global-mark-ring       ; marks
        search-ring regexp-search-ring)) ; searches

;;; Frames and windows

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider-mode' does not.
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

;;; Fontification

;; Disable fontification during user input to reduce lag in large buffers.
;; Also helps marginally with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;;; Scrolling

;; Enables faster scrolling. This may result in brief periods of inaccurate
;; syntax highlighting, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setq scroll-error-top-bottom t)

;; Keep screen position if scroll command moved it vertically out of the window.
(setq scroll-preserve-screen-position t)

;; Emacs recenters the window when the cursor moves past `scroll-conservatively'
;; lines beyond the window edge. A value over 101 disables recentering; the
;; default (0) is too eager. Here it is set to 20 for a balanced behavior.
(setq scroll-conservatively 20)

;; 1. Preventing automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolving the issue of random half-screen jumps during scrolling.
(setq auto-window-vscroll nil)

;; Number of lines of margin at the top and bottom of a window.
(setq scroll-margin 0)

;; Number of lines of continuity when scrolling by screenfuls.
(setq next-screen-context-lines 0)

;; Horizontal scrolling
(setq hscroll-margin 2
      hscroll-step 1)

;;; Mouse

(setq mouse-yank-at-point nil)

;; Emacs 29
(when (memq 'context-menu minimal-emacs-ui-features)
  (when (and (display-graphic-p) (fboundp 'context-menu-mode))
    (add-hook 'after-init-hook #'context-menu-mode)))

;;; Cursor

;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Do not extend the cursor to fit wide characters
(setq x-stretch-cursor nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;;; Text editing, indent, font, and formatting

;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)

;; A longer delay can be annoying as it causes a noticeable pause after each
;; deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Disable visual indicators in the fringe for buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; Configure automatic indentation to be triggered exclusively by newline and
;; DEL (backspace) characters.
(setq-default electric-indent-chars '(?\n ?\^?))

;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Enable indentation and completion using the TAB key
(setq tab-always-indent 'complete)
(setq tab-first-completion 'word-or-paren-or-punct)

;; Perf: Reduce command completion overhead.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines.
(setq comment-multi-line t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance.
(setq comment-empty-lines t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Disable the obsolete practice of end-of-line spacing from the typewriter era.
(setq sentence-end-double-space nil)

;; According to the POSIX, a line is defined as "a sequence of zero or more
;; non-newline characters followed by a terminating newline".
(setq require-final-newline t)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)

;;; Modeline

;; Makes Emacs omit the load average information from the mode line.
(setq display-time-default-load-average nil)

;;; Filetype

;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)

(setq sh-indent-after-continuation 'always)

;;; Dired and ls-lisp

(setq dired-free-space nil
      dired-dwim-target t  ; Propose a target for intelligent moving/copying
      dired-deletion-confirmer 'y-or-n-p
      dired-filter-verbose nil
      dired-recursive-deletes 'top
      dired-recursive-copies 'always
      dired-vc-rename-file t
      dired-create-destination-dirs 'ask
      ;; Suppress Dired buffer kill prompt for deleted dirs
      dired-clean-confirm-killing-deleted-buffers nil)

;; This is a higher-level predicate that wraps `dired-directory-changed-p'
;; with additional logic. This `dired-buffer-stale-p' predicate handles remote
;; files, wdired, unreadable dirs, and delegates to dired-directory-changed-p
;; for modification checks.
(setq auto-revert-remote-files nil)
(setq dired-auto-revert-buffer 'dired-buffer-stale-p)

;; dired-omit-mode
(setq dired-omit-verbose nil
      dired-omit-files (concat "\\`[.]\\'"))

(setq ls-lisp-verbosity nil)
(setq ls-lisp-dirs-first t)

;;; Ediff

;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;; Help

;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

;; Fixes #11: Prevents help command completion from triggering autoload.
;; Loading additional files for completion can slow down help commands and may
;; unintentionally execute initialization code from some libraries.
(setq help-enable-completion-autoload nil)
(setq help-enable-autoload nil)
(setq help-enable-symbol-autoload nil)
(setq help-window-select t)  ;; Focus new help windows when opened

;;; Eglot

;; A setting of nil or 0 means Eglot will not block the UI at all, allowing
;; Emacs to remain fully responsive, although LSP features will only become
;; available once the connection is established in the background.
(setq eglot-sync-connect 0)

(setq eglot-autoshutdown t)  ; Shut down server after killing last managed buffer

;; Activate Eglot in cross-referenced non-project files
(setq eglot-extend-to-xref t)

;; Eglot optimization
(if minimal-emacs-debug
    (setq eglot-events-buffer-config '(:size 2000000 :format full))
  ;; This reduces log clutter to improves performance.
  (setq jsonrpc-event-hook nil)
  ;; Reduce memory usage and avoid cluttering *EGLOT events* buffer
  (setq eglot-events-buffer-size 0)  ; Deprecated
  (setq eglot-events-buffer-config '(:size 0 :format short)))

(setq eglot-report-progress minimal-emacs-debug)  ; Prevent minibuffer spam

;;; Flymake

(setq flymake-show-diagnostics-at-end-of-line nil)

;; Disable wrapping around when navigating Flymake errors.
(setq flymake-wrap-around nil)

;;; hl-line-mode

;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
;; clutter and slightly improving `hl-line-mode' performance.
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

;;; icomplete

;; Do not delay displaying completion candidates in `fido-mode' or
;; `fido-vertical-mode'
(setq icomplete-compute-delay 0.01)

;;; flyspell

(setq flyspell-issue-welcome-flag nil)

;; Improves flyspell performance by preventing messages from being displayed for
;; each word when checking the entire buffer.
(setq flyspell-issue-message-flag nil)

;;; ispell

;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
;; when no `ispell' dictionary is set.
(setq text-mode-ispell-word-completion nil)

(setq ispell-silently-savep t)

;;; ibuffer

(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 55 55 :left :elide)
              " " (size 8 -1 :right)
              " " (mode 18 18 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

;;; xref

;; Enable completion in the minibuffer instead of the definitions buffer
(setq xref-show-definitions-function 'xref-show-definitions-completing-read
      xref-show-xrefs-function 'xref-show-definitions-completing-read)

;;; abbrev

;; Ensure `abbrev_defs` is stored in the correct location when
;; `user-emacs-directory` is modified, as it defaults to ~/.emacs.d/abbrev_defs
;; regardless of the change.
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(setq save-abbrevs 'silently)

;;; dabbrev

(setq dabbrev-upcase-means-case-search t)

(setq dabbrev-ignored-buffer-modes
      '(archive-mode image-mode docview-mode tags-table-mode
                     pdf-view-mode tags-table-mode))

(setq dabbrev-ignored-buffer-regexps
      '(;; - Buffers starting with a space (internal or temporary buffers)
        "\\` "
        ;; Tags files such as ETAGS, GTAGS, RTAGS, TAGS, e?tags, and GPATH,
        ;; including versions with numeric extensions like <123>
        "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))

;;; Remove warnings from narrow-to-region, upcase-region...

(dolist (cmd '(list-timers narrow-to-region upcase-region downcase-region
                           list-threads erase-buffer scroll-left
                           dired-find-alternate-file set-goal-column))
  (put cmd 'disabled nil))

;;; Load post init
(when (fboundp 'minimal-emacs-load-user-init)
  (minimal-emacs-load-user-init "post-init.el"))
(setq minimal-emacs--success t)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
