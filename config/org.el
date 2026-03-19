;;; org.el --- Org mode customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package org
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :hook (org-mode . visual-line-mode)
  :init
  (setq org-directory (format "%s/org/" xdg-home))
  (let ((org-archive-directory (format "%s/archives/" org-directory)))
    (me/mkdir org-archive-directory)
    (setq org-archive-location (format "%s/%%s::" org-archive-directory)))
  :bind
  ((:map me/org-map
        ("c" . org-capture)
        ("a" . org-agenda))
   (:map org-mode-map
         ("M-<return>" . org-insert-heading-respect-content)
         ("C-<return>" . org-insert-item)))
  :custom
  ;; General
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated t)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-confirm-babel-evaluate nil)
  (org-return-follows-link t)
  (org-link-descriptive t)
  (org-enforce-todo-dependencies t)
  (org-log-done 'time)
  ;; Babel
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t))

;; Modernize the look of org
(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :custom
  (org-ellipsis "…")
  (org-pretty-entities t)
  (org-hide-leading-stars t)
  (org-superstar-remove-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-superstar-item-bullet-alist
   '((?* . ?•)
     (?+ . ?•)
     (?- . ?•))))

;; Toggle visibility of hidden Org mode element parts upon entering and leaving an element
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t))

;; Org agenda setup
(use-package org-agenda
  :ensure nil
  :bind (:map org-agenda-mode-map
              ("C-n" . org-agenda-next-item)
              ("C-p" . org-agenda-previous-item)
              ("g" . org-agenda-goto))
  :config
  (let ((org-agenda-directory (format "%s/agenda/" org-directory)))
    (me/mkdir org-agenda-directory)
    (org-agenda-files org-agenda-directory)))


;; Simple notes with an efficient file-naming scheme
(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:map me/note-map
        (("n" . denote)
         ("r" . denote-rename-file)
         ("l" . denote-link)
         ("b" . denote-backlinks)
         ("d" . denote-sort-dired)))
  :config
  (let ((org-notes-directory (format "%s/notes/" org-directory)))
    (me/mkdir org-notes-directory)
    (setq denote-directory org-notes-directory))
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :after denote consult
  :bind
  (:map me/note-map
        ("f" . consult-denote-find)
        ("g" . consult-denote-grep))
  :custom
  (consult-denote-grep-command 'consult-ripgrep)
  :config
  (consult-denote-mode 1))

(use-package writeroom-mode
  :bind (("<f3>" . writeroom-mode)))
