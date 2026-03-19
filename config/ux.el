;;; ux.el --- User Experience customization  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Enable Smooth Scrolling
(setq pixel-scroll-precision-interpolate-mice nil)        ; Disable interpolation (causes wired jumps)
(setq pixel-scroll-precision-mode (display-graphic-p))    ; Enable pixel-wise scrolling
(setq pixel-scroll-precision-use-momentum t)              ; Enable momentum for scrolling lagre buffers

;; Window Management
(use-package ace-window
  :autoload ace-display-buffer
  :init
  (winner-mode)
  :bind
  (("M-o" . ace-window)
   ("M-O" . me/ace-window-prefix)
   ("M-u" . me/toggle-fullscreen-window)
   ([remap split-window-right] . me/hsplit-last-window)
   ([remap split-window-below] . me/vsplit-last-window)
   (:map me/window-map
         ("b" . balance-windows)
         ("c" . recenter-top-bottom)
         ("i" . enlarge-window)
         ("j" . shrink-window-horizontally)
         ("k" . shrink-window)
         ("u" . winner-undo)
         ("r" . winner-redo)
         ("l" . enlarge-window-horizontally)
         ("s" . switch-window-then-swap-buffer)
         ("-" . text-scale-decrease)
         ("+" . text-scale-increase)
         ("=" . (lambda () (interactive) (text-scale-increase 0)))))
  :preface
  (defun me/hsplit-last-window ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))
  (defun me/vsplit-last-window ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1))
  (defun me/toggle-fullscreen-window ()
    "Toggle a buffer as fullscreen"
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (delete-other-windows))))
  (defun me/ace-window-prefix ()
    "https://karthinks.com/software/emacs-window-management-almanac/#a-window-prefix-command-for-ace-window"
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  :custom
  ;; Switch to minibuffer as well
  (aw-minibuffer-flag t)
  ;; Make Emacs ask where to place a new buffer
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-in-previous-window
      ace-display-buffer)))
  :custom-face
  (aw-leading-char-face ((t (:foreground "red" :weight bold :height 2.0)))))

;; Buffer Management
(use-package ibuffer
  :ensure nil
  :bind
  ((:map ctl-x-map
         ("B" . me/switch-to-previous-buffer))
   (:map me/buffer-map
         ("r" . me/rename-file-and-buffer)
         ("d" . me/delete-file-and-buffer)
         ("o" . me/kill-other-buffers)))
  :init (me/protected-buffers)
  :preface
  (defvar protected-buffers '("*scratch*" "*Messages*"))
  (defun me/protected-buffers ()
    "Protect some buffers from being killed."
    (dolist (buffer protected-buffers)
      (with-current-buffer buffer
        (emacs-lock-mode 'kill))))
  (defun me/switch-to-previous-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  (defun me/rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))
  (defun me/delete-file-and-buffer ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
        (if (vc-backend filename)
            (vc-delete-file filename)
          (progn
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer))))))
  (defun me/kill-other-buffers ()
    "Kill other buffers except current one and protected buffers."
    (interactive)
    (eglot-shutdown-all)
    (mapc 'kill-buffer
          (cl-remove-if
           (lambda (x)
             (or
              (eq x (current-buffer))
              (member (buffer-name x) protected-buffers)))
           (buffer-list)))
    (delete-other-windows))
  :config
  (setq switch-to-buffer-obey-display-actions t)
  (setq switch-to-buffer-in-dedicated-window 'pop))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; Project Management
(use-package project
  :ensure nil
  :bind
  (:map ctl-x-map
   ("C-p" . project-find-file)
   :map project-prefix-map
   ("l" . project-list-buffers)
   ("F" . project-forget-project)
   ("S" . me/project-save-all-buffers)
   ("s" . project-search))
  :preface
  (defun me/project-save-all-buffers (&optional proj arg)
    "Save all file-visiting buffers in project without asking."
    (interactive)
    (let* ((proj (or proj (project-current)))
           (buffers (project-buffers (proj))))
      (dolist (buf buffers)
        ;; Act on base buffer of indirect buffers, if needed.
        (with-current-buffer (or (buffer-base-buffer buf) buf)
          (when (and (buffer-file-name buf)   ; Ignore all non-file-visiting buffers.
                     (buffer-modified-p buf)) ; Ignore all unchanged buffers.
            (let ((buffer-save-without-query t))  ; Save silently.
              (save-buffer arg)))))))
  :config
  (setq project-buffers-viewer 'project-list-buffers-ibuffer)
  (setq project-kill-buffers-display-buffer-list t)
  (setq project-switch-commands
        '((project-find-file "Find file" "f")
          (project-find-dir "Find dir" "d")
          (project-dired "Dired" "D")
          (consult-ripgrep "ripgrep" "r")
          (magit-project-status "Magit" "m")))
  (setq project-vc-extra-root-markers '(".project")))


;; File Management
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-mode . dired-omit-mode)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-find-file)
        ("M-n" . dired-find-file)
        ("<backtab>" . dired-up-directory)
        ("M-p" . dired-up-directory)
        ( "."     . dired-omit-mode)
        ("C-+" . dired-create-empty-file)
        ("Q" . (lambda ()
                 (interactive)
                 (mapc (lambda (buffer)
                         (when (or (eq 'dired-mode (buffer-local-value 'major-mode buffer))
                                   (string-match-p "^\\*image-dired" (buffer-name buffer)))
                           (kill-buffer buffer)))
                       (buffer-list)))))
  (:map me/file-map
        ("d" . (lambda ()
                 (interactive)
                 (dired-jump)
                 (revert-buffer)))
        ("D" . dired)
        ("f" . find-file-at-point))
  :custom
  (dired-omit-files (rx (seq bol ".")))
  (dired-listing-switches "-goah --group-directories-first --time-style=long-iso -v")
  (dired-kill-when-opening-new-dired-buffer nil)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-create-destination-dirs 'ask)
  (dired-clean-confirm-killing-deleted-buffers nil))


;; Sidebar file exploration
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind
  (("<f1>" . dired-sidebar-toggle-sidebar)
   :map dired-sidebar-mode-map
        ("<tab>" . dired-sidebar-subtree-toggle)
        ("M-n" . dired-sidebar-subtree-toggle)
        ("<backtab>" . dired-subtree-up)
        ("M-p" . dired-subtree-up))
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-theme 'nerd-icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))
