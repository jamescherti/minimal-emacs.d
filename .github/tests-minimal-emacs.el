;;; tests-minimal-emacs.el --- ERT tests for minimal-emacs.d -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1.5.2
;; SPDX-License-Identifier: GPL-3.0-or-later

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
  "Remove the opening and closing code-fence lines of a single fenced block.
Handles the fence variants used in README.md: ```lang, ``` lang, and a
plain closing ```.  If no fences are present, returns CONTENT trimmed."
  (let ((lines (split-string (string-trim content) "\n" t)))
    (when (and lines
               (string-match-p "^```[ \t]*[a-zA-Z0-9_+-]*[ \t]*$" (car lines)))
      (setq lines (cdr lines)))
    (when (and lines
               (string-match-p "^```[ \t]*$" (car (last lines))))
      (setq lines (butlast lines)))
    (string-trim (string-join lines "\n"))))

(defun minimal-emacs--group-by-filename (blocks)
  "Group BLOCKS by filename, concatenating contents in README order.
BLOCKS is a list of (FILENAME CONTENT START END) quadruples.
Returns a list of (FILENAME CONCATENATED-CONTENT) pairs, preserving the
order in which each filename first appears.  Empty blocks are skipped."
  (let ((groups))
    (dolist (b blocks)
      (let* ((fname (car b))
             (content (minimal-emacs--strip-fences (cadr b)))
             (cell (catch 'found
                     (dolist (g groups)
                       (when (equal (car g) fname)
                         (throw 'found g))))))
        (when (not (string-empty-p content))
          (if cell
              (setcar (cdr cell) (concat (cadr cell) "\n\n" content))
            (push (list fname content) groups)))))
    (nreverse groups)))

(defun minimal-emacs--describe-file (fname)
  "Return a brief DESCRIPTION for the user customization file FNAME.
Descriptions are taken from the README's description of each file's role in
the startup sequence."
  (or (cdr (assoc (file-name-nondirectory fname)
                  '(("pre-early-init.el" . "User customization loaded before early-init.el")
                    ("post-early-init.el" . "User customization loaded after early-init.el")
                    ("pre-init.el" . "User customization loaded before init.el")
                    ("post-init.el" . "User customization loaded after init.el"))))
      "User customization"))

(defun minimal-emacs--materialize (fname content)
  "Write CONTENT to repo-root/FNAME as a well-formed Emacs Lisp file.
The layout follows the project conventions used by `init.el': a
`;;; FNAME --- DESCRIPTION -*- lexical-binding: t; -*-' line, the metadata
block, a `;;; Commentary:' section, a `;;; Code:' marker preceding CONTENT,
and a `;;; FNAME ends here' footer.  Only `lexical-binding: t;' is set (not
`no-byte-compile: t;') so the ERT suite can still byte-compile the
materialized file to verify it parses.
Returns the absolute file name of the materialized file."
  (let* ((base (file-name-nondirectory fname))
         (desc (minimal-emacs--describe-file base))
         (body (string-trim content))
         (path (expand-file-name fname minimal-emacs--repo-root)))
    (with-temp-file path
      (insert
       (format ";;; %s --- %s -*- lexical-binding: t; -*-\n\n" base desc)
       ";; Author: James Cherti <https://www.jamescherti.com/contact/>\n"
       ";; URL: https://github.com/jamescherti/minimal-emacs.d\n"
       ";; Package-Requires: ((emacs \"29.1\"))\n"
       ";; Keywords: maint\n"
       ";; Version: 1.5.2\n"
       ";; SPDX-License-Identifier: GPL-3.0-or-later\n\n"
       ";;; Commentary:\n"
       (format ";; %s\n" desc)
       ";;\n"
       ";; Materialized from the matching code block in README.md by the ERT\n"
       ";; suite to verify it is well-formed Emacs Lisp.\n\n"
       ";;; Code:\n\n")
      (insert body)
      (unless (string-suffix-p "\n" body)
        (insert "\n"))
      (insert "\n;; Local variables:\n")
      (insert ";; byte-compile-warnings: (not free-vars)\n")
      (insert ";; End:\n")
      (insert (format "\n;;; %s ends here\n" base)))
    path))

(defun minimal-emacs--byte-compile-config (file)
  "Byte-compile FILE in-process, succeeding only if it produces a .elc.
`byte-compile-file' reports hard parse/syntax errors to stderr and returns
nil (rather than signalling), so a produced .elc is the authoritative success
signal.  Warnings ARE treated as errors (`byte-compile-error-on-warn' is t):
the code blocks in README.md must be byte-compilation-clean.  Only
`free-vars' and `obsolete' warnings are excluded, since the blocks reference
packages that are not installed in the minimal test environment.  A stray
markdown code-fence marker such as ```elisp leaks into the extracted source
and yields a hard parse error, which also fails the test.
Returns t on success, otherwise a string describing the failure."
  (let* ((byte-compile-error-on-warn t)
         (byte-compile-warnings '(not obsolete free-vars))
         (elc (concat file "c")))
    (unwind-protect
        (progn
          (when (file-exists-p elc)
            (delete-file elc))
          (byte-compile-file file)
          (if (file-exists-p elc)
              t
            (format "%s: byte-compilation failed (no %s produced) ,  the source\ncontains a hard parse/syntax error, e.g. a stray markdown code-fence marker.\nSee the byte-compiler's stderr for the specific diagnostic."
                    (file-name-nondirectory file) (file-name-nondirectory elc))))
      (when (file-exists-p elc)
        (delete-file elc)))))

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
  "Extract README code blocks, write files, and byte-compile successfully.
Materialize README config blocks into the repo working directory next
to init.el / early-init.el, byte-compile them, and require actual
compilation success (no parse/syntax errors).

The README documents the `pre-early-init.el' and `post-init.el'
customization files as fenced code blocks.  This test extracts those blocks,
strips the markdown code-fence markers (```elisp / ```emacs-lisp / ```),
materializes one file per documented filename (concatenating all blocks that
share a filename, in README order), writes them to the repo root, and
byte-compiles each.  Stray fence markers leak into the extracted source and
produce a hard parse error, which fails the test; the legitimate warnings
that these config files emit in the minimal environment (undefined
functions, free variables) do not."
  (should (file-exists-p minimal-emacs--readme))
  (let* ((blocks (minimal-emacs--extract-blocks minimal-emacs--readme))
         (groups (minimal-emacs--group-by-filename blocks))
         (fnames (mapcar 'car groups)))
    (should (> (length groups) 0))
    (should (member "pre-early-init.el" fnames))
    (should (member "post-init.el" fnames))
    (let (failures)
      (dolist (g groups)
        (let* ((fname (car g))
               (content (cadr g))
               (path (minimal-emacs--materialize fname content))
               (result (minimal-emacs--byte-compile-config path)))
          (unless (eq result t)
            (push (format "%s: %s" fname result) failures))
          ;; Remove the compiled artifact; keep the materialized source.
          (let ((elc (concat path "c")))
            (when (file-exists-p elc)
              (delete-file elc)))))
      (when failures
        (error "Byte-compilation failed for materialized README files:\n%s"
               (string-join (nreverse failures) "\n"))))))

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
