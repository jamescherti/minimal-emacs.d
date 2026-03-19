;;; post-init.el --- Post-Init.el  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Allow :make or :shell when using :vc with use-package
(setq package-vc-allow-build-commands t)

;; Ensure all libraries are byte-compiled and native-compiled
(use-package compile-angel
  :ensure t
  :demand t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; `use-package', you'll need to explicitly add `(require 'use-package)` at
  ;; the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded.
  (compile-angel-on-load-mode))

;; Package auto-update
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))
(setq package-install-upgrade-built-in t)

;; Sync system environment variables with emacs
(use-package exec-path-from-shell
  :hook
  (after-init . exec-path-from-shell-initialize)
  :custom
  (epg-pinentry-mode 'loopback)
  (exec-path-from-shell-variables '("PATH" "SHELL" "GOPATH"))
  :config
  (setenv "SSH_AUTH_SOCK" (string-chop-newline (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))))

;; Make emacs work with gui pinentry, note this requires setup of my emacs-tty-pinentry script
;; https://stackoverflow.com/questions/60812866/emacs-gpg-pinentry-el-for-authentication
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; Disable backup and autosave of .vcf and .gpg files
;; Alternatively, to protect only some files, like some .txt files, use a line like
;; // -*-mode:asciidoc; mode:sensitive-minor; fill-column:132-*-
(setq auto-mode-alist
      (append
       (list '("\\.\\(vcf\\|gpg\\)$" . sensitive-mode)) auto-mode-alist))

(use-package pinentry
  :init
  (pinentry-start)
  :config
  (setenv "GPG_AGENT_INFO" nil))

;; Completion configuration
(minimal-emacs-load-user-init "config/completion.el")

;; User Interface configuration
(minimal-emacs-load-user-init "config/ui.el")

;; Backup configuration
(minimal-emacs-load-user-init "config/backup.el")

;; User Experience configuration
(minimal-emacs-load-user-init "config/ux.el")

;; Navigation configuration
(minimal-emacs-load-user-init "config/navigation.el")

;; Editing configuration
(minimal-emacs-load-user-init "config/editing.el")

;; Version Control configuration
(minimal-emacs-load-user-init "config/vc.el")

;; Development configuration
(minimal-emacs-load-user-init "config/development.el")

;; Container configuration
(minimal-emacs-load-user-init "config/container.el")

;; Org configuration
(minimal-emacs-load-user-init "config/org.el")

;; Apps configuration
(minimal-emacs-load-user-init "config/apps.el")

;; Languages configuration
(let ((language-config-dir (expand-file-name "languages" minimal-emacs-user-directory)))
    (when (file-directory-p language-config-dir)
    (dolist (file (directory-files language-config-dir "\\.el$"))
      (minimal-emacs-load-user-init file))))
