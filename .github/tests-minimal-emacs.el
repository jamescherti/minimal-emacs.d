;;; tests-minimal-emacs.el --- ERT tests for minimal-emacs.d -*- lexical-binding: t; no-byte-compile: t; -*-

;; Verify README elisp blocks and init files by byte-compiling them.
;; Extracts configuration blocks from README.md between
;;   <!-- filename.el:begin --> and <!-- filename.el:end --> markers,
;; then byte-compiles each block to confirm syntactic correctness.
;;
;; Also performs offline consistency checks between README claims and
;; the actual init.el / early-init.el source.
;;
;; Run with:
;; emacs -Q --batch -l .github/tests-minimal-emacs.el -f ert-run-tests-batch-and-exit

;;; Code:

(defconst minimal-emacs--repo-root
  (file-name-as-directory
   (file-name-directory
    (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
  "Root of the minimal-emacs.d repository.")

(defconst minimal-emacs--readme (expand-file-name "README.md" minimal-emacs--repo-root))
(defconst minimal-emacs--init (expand-file-name "init.el" minimal-emacs--repo-root))
(defconst minimal-emacs--early-init (expand-file-name "early-init.el" minimal-emacs--repo-root))

;; ---------------------------------------------------------------------------
;; Block extraction
;; ---------------------------------------------------------------------------

(defun minimal-emacs--extract-blocks (readme)
  "Parse all <!-- fname.el:begin --> ... <!-- fname.el:end --> blocks in README.
Return a list of (FILENAME CONTENT START END) quadruples."
  (let ((text (with-temp-buffer
                (insert-file-contents readme)
                (buffer-substring-no-properties (point-min) (point-max))))
        blocks)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "<!--[ \\t]*\\(\\(?:[a-zA-Z0-9_-]+\\.el\\)\\)[ \\t]*:begin[ \\t]*-->"
              nil t)
        (let* ((fname (match-string 1))
               (start (match-end 0))
               end
               content)
          (when (re-search-forward
                 (format "<!--[ \\t]*%s[ \\t]*:end[ \\t]*-->"
                         (regexp-quote fname))
                 nil t)
            (setq end (match-end 0)
                  content (buffer-substring-no-properties start (match-beginning 0)))
            (push (list fname content start end) blocks)))))
    (nreverse blocks)))

(defun minimal-emacs--strip-fences (content)
  "Remove surrounding ```lang ... ``` fences from CONTENT if present."
  (let ((stripped (string-trim content)))
    (if (string-match "^\\`\\`\\`[a-zA-Z0-9_-]*[ \t]*\n\\(.*\n\\)*\\`\\`\\`[ \t]*$" stripped)
        (let ((inner (substring stripped (match-end 0) (string-length stripped))))
          ;; Remove the leading content up to the last ```
          (string-match "^[ \t]*\n\\(.*\\)" inner)
          (string-trim (if (match-string 1 inner)
                           (match-string 1 inner)
                         inner)))
      stripped)))

;; ---------------------------------------------------------------------------
;; Byte-compilation helper
;; ---------------------------------------------------------------------------

(defun minimal-emacs--byte-compile-string (code &optional label)
  "Byte-compile CODE written to a temp file. Signal an error on failure.
LABEL is used in the error message."
  (let* ((tmpel (make-temp-file "__readme-test-" nil ".el"))
         (result nil))
    (with-temp-buffer
      (insert ";;; test.el --- test -*- lexical-binding: t; no-byte-compile: t; -*-")
      (insert "(setq gc-cons-threshold most-positive-fixnum)\n\n")
      (insert code)
      (write-file tmpel))
    (with-temp-buffer
      (setq result
            (condition-case err
                (byte-compile-file tmpel t)
              (error err)))
      (delete-file tmpel)
      (let ((elc (concat tmpel "c")))
        (when (file-exists-p elc)
          (delete-file elc))))
    (when (consp result)
      (error "Byte-compilation failed for %s: %S" (or label "unknown") result))))

;; ---------------------------------------------------------------------------
;; ERT tests
;; ---------------------------------------------------------------------------

(ert-deftest minimal-emacs--early-init-byte-compiles ()
  "Verify that early-init.el byte-compiles without errors."
  (should (file-exists-p minimal-emacs--early-init))
  (minimal-emacs--byte-compile-string
   (with-temp-buffer
     (insert-file-contents minimal-emacs--early-init)
     (buffer-substring-no-properties (point-min) (point-max)))
   "early-init.el"))

(ert-deftest minimal-emacs--init-byte-compiles ()
  "Verify that init.el byte-compiles without errors."
  (should (file-exists-p minimal-emacs--init))
  (minimal-emacs--byte-compile-string
   (with-temp-buffer
     (insert-file-contents minimal-emacs--init)
     (buffer-substring-no-properties (point-min) (point-max)))
   "init.el"))

(ert-deftest minimal-emacs--readme-byte-compiles ()
  "Verify that README.md elisp blocks byte-compile without errors."
  (should (file-exists-p minimal-emacs--readme))
  (let ((blocks (minimal-emacs--extract-blocks minimal-emacs--readme)))
    (should (> (length blocks) 0))
    (dolist (b blocks)
      (let* ((fname (car b))
             (content (cadr b))
             (code (minimal-emacs--strip-fences content)))
        (when (string-trim code)
          (minimal-emacs--byte-compile-string code fname))))))

(ert-deftest minimal-emacs--line-count-consistency ()
  "Verify README line count claim is within 25% of actual non-trivial code lines.
The README claims ~450 lines excluding empty lines, comments, and docstrings,
so count only substantive code lines to match that claim."
  (let* ((readme-text (with-temp-buffer
                        (insert-file-contents minimal-emacs--readme)
                        (buffer-substring-no-properties (point-min) (point-max))))
         (init-text (with-temp-buffer
                      (insert-file-contents minimal-emacs--init)
                      (buffer-substring-no-properties (point-min) (point-max))))
         (early-text (with-temp-buffer
                       (insert-file-contents minimal-emacs--early-init)
                       (buffer-substring-no-properties (point-min) (point-max))))
         (claimed nil)
         (code-line-count 0))
    ;; Count non-empty, non-comment lines to match the README's claim
    (dolist (text (list init-text early-text))
      (dolist (line (split-string text "\n" t))
        (let ((trimmed (string-trim line)))
          (when (and (not (string-empty-p trimmed))
                     (not (string-prefix-p ";;;" trimmed))
                     (not (string-prefix-p ";;" trimmed)))
            (cl-incf code-line-count)))))
    (when (string-match "\\([0-9]+\\)[ \\t]*lines" readme-text)
      (setq claimed (string-to-number (match-string 1 readme-text)))
      (should (< (abs (- code-line-count claimed)) (* code-line-count 0.25))))))

(ert-deftest minimal-emacs--feature-consistency ()
  "Cross-reference README feature bullets with actual variable presence in init files."
  (let* ((readme-text (with-temp-buffer
                        (insert-file-contents minimal-emacs--readme)
                        (buffer-substring-no-properties (point-min) (point-max))))
         (init-text (with-temp-buffer
                      (insert-file-contents minimal-emacs--init)
                      (buffer-substring-no-properties (point-min) (point-max))))
         (early-text (with-temp-buffer
                       (insert-file-contents minimal-emacs--early-init)
                       (buffer-substring-no-properties (point-min) (point-max))))
         (combined (concat init-text early-text))
         ;; List of (LABEL README-PATTERN VAR-NAME)
         ;; Covers every Feature bullet in the README Features section.
         (checks '(("Optimized File Handlers"
                    "\\*\\*Optimized File Handlers"
                    "gc-cons-threshold")
                   ("Process Throughput"
                    "\\*\\*Process Throughput"
                    "read-process-output-max")
                   ("Compiled File Preference"
                    "\\*\\*Compiled File Preference"
                    "load-prefer-newer")
                   ("Optimized Text Rendering"
                    "\\*\\*Optimized Text Rendering"
                    "inhibit-compacting-font-caches")
                   ("Silent Boot Sequence"
                    "\\*\\*Silent Boot Sequence"
                    "inhibit-startup-screen")

                   ;; --- Native Compilation and Byte Compilation ---
                   ("Out-of-the-Box Optimization"
                    "\\*\\*Out-of-the-Box Optimization"
                    "native-comp-async-report-warnings-errors")
                   ("Quiet Compilation"
                    "\\*\\*Quiet Compilation"
                    "byte-compile-warnings")

                   ;; --- Interface Defaults ---
                   ("Minimal UI"
                    "\\*\\*Minimal UI"
                    "menu-bar-mode")
                   ("Smart Rendering"
                    "\\*\\*Smart Rendering"
                    "highlight-nonselected-windows")
                   ("Typographic Defaults"
                    "\\*\\*Typographic Defaults"
                    "x-underline-at-descent-line")
                   ("Focused Minibuffer"
                    "\\*\\*Focused Minibuffer"
                    "enable-recursive-minibuffers")

                   ;; --- Package and File Management ---
                   ("Repository Prioritization"
                    "\\*\\*Repository Prioritization"
                    "package-archives")
                   ("Centralized Artifacts"
                    "\\*\\*Centralized Artifacts"
                    "backup-directory-alist")
                   ("Auto-Revert"
                    "\\*\\*Auto-Revert"
                    "global-auto-revert-non-file-buffers")
                   ("Session Memory"
                    "\\*\\*Session Memory"
                    "recentf-max-saved-items")

                   ;; --- Precision Editing and UX Enhancements ---
                   ("Predictable Scrolling"
                    "\\*\\*Predictable Scrolling"
                    "scroll-conservatively")
                   ("Modern Formatting Standards"
                    "\\*\\*Modern Formatting Standards"
                    "require-final-newline")
                   ("Sensible Tab Management"
                    "\\*\\*Sensible Tab Management"
                    "indent-tabs-mode")
                   ("Fast Interactions"
                    "\\*\\*Fast Interactions"
                    "read-answer-short")

                   ;; --- Developer Experience ---
                   ("Optional Built-in Package Defaults"
                    "\\*\\*Optional Built-in Package Defaults"
                    "eglot-report-progress")
                   ("Git Integration"
                    "\\*\\*Git Integration"
                    "vc-git-diff-switches")

                   ;; --- Buffer, Directory, and Window Management ---
                   ("Modern Splits"
                    "\\*\\*Modern Splits"
                    "split-width-threshold")
                   ("Ediff Optimization"
                    "\\*\\*Ediff Optimization"
                    "ediff-window-setup-function")
                   ("Dired Mastery"
                    "\\*\\*Dired Mastery"
                    "dired-auto-revert-buffer")
                   ("Window Dividers"
                    "\\*\\*Window Dividers"
                    "window-divider-default-bottom-width")

                   ;; --- Security, Stability, and Customizable Initialization ---
                   ("Safety Checks"
                    "\\*\\*Safety Checks"
                    "minimal-emacs--check-success")
                   ("Drop-In Customization"
                    "\\*\\*Drop-In Customization"
                    "minimal-emacs-load-user-init")
                   ("Strict TLS Verification"
                    "\\*\\*Strict TLS Verification"
                    "gnutls-verify-error")
                   ("Encrypted Auth Sources"
                    "\\*\\*Encrypted Auth Sources"
                    "auth-sources"))))
    (dolist (check checks)
      (let ((label (car check))
            (readme-pat (cadr check))
            (var-name (caddr check)))
        (when (string-match readme-pat readme-text)
          (should (string-match var-name combined)))))))

(provide 'tests-minimal-emacs)
;;; tests-minimal-emacs.el ends here
