;;; pre-early-init.el --- Pre-Early-Init.el  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Set debug mode on/off
(setq debug-on-error nil)

;; Set XDG Base Directory variables
;; https://wiki.archlinux.org/title/XDG_Base_Directory
(defvar xdg-home (getenv "HOME"))
(defvar xdg-state (getenv "XDG_STATE_HOME"))
(defvar xdg-data (getenv "XDG_DATA_HOME"))
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))
(defvar xdg-cache (getenv "XDG_CACHE_HOME"))
(defvar xdg-lib (getenv "XDG_LIB_HOME"))

;; Move native compilation cache directory to xdg-cache
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name (format "%s/emacs/eln-cache/" xdg-cache))))

;; Change the default emacs configuration dircetory to avoid cluttering main
(setq minimal-emacs-user-directory user-emacs-directory)
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq user-emacs-directory minimal-emacs-var-dir)
