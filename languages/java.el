;;; java.el --- Java customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package java-ts-mode
  :ensure nil
  :mode (("\\.java\\'" . java-ts-mode))
  :hook
  (java-ts-mode . eglot-ensure)
  :hook
  (java-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "r") #'me/java-run)
                    (define-key me/run-map (kbd "t") #'me/java-test)
                    (define-key me/run-map (kbd "c") #'me/java-check)
                    (define-key me/run-map (kbd "f") #'me/java-format)
                    (define-key me/run-map (kbd "b") #'me/java-build)
                    (define-key me/run-map (kbd "C") #'me/java-clean)))
  :preface
  (defun me/java-project-root ()
    "Find the project root by looking for pom.xml or build.gradle."
    (or (locate-dominating-file default-directory "pom.xml")
        (locate-dominating-file default-directory "build.gradle")
        (locate-dominating-file default-directory "build.gradle.kts")
        default-directory))

  (defun me/java-build-tool ()
    "Detect which build tool the project uses."
    (let ((root (me/java-project-root)))
      (cond
       ((file-exists-p (expand-file-name "pom.xml" root)) "mvn")
       ((or (file-exists-p (expand-file-name "build.gradle" root))
            (file-exists-p (expand-file-name "build.gradle.kts" root))) "gradle")
       (t nil))))

  (defun me/java-run ()
    "Run the main class of the current Java project."
    (interactive)
    (let* ((root (me/java-project-root))
           (tool (me/java-build-tool))
           (default-directory root))
      (cond
       ((string= tool "mvn")
        (compile "mvn compile exec:java"))
       ((string= tool "gradle")
        (compile "./gradlew run"))
       (t
        ;; Fallback: compile and run current file directly
        (let* ((class-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
               (compile-cmd (format "javac %s && java %s"
                                   (shell-quote-argument buffer-file-name)
                                   class-name)))
          (compile compile-cmd))))))

  (defun me/java-test ()
    "Run tests for the current Java project."
    (interactive)
    (let* ((root (me/java-project-root))
           (tool (me/java-build-tool))
           (default-directory root))
      (cond
       ((string= tool "mvn")
        (compile "mvn test"))
       ((string= tool "gradle")
        (compile "./gradlew test"))
       (t
        (message "No build tool detected. Please use Maven or Gradle.")))))

  (defun me/java-build ()
    "Build the current Java project."
    (interactive)
    (let* ((root (me/java-project-root))
           (tool (me/java-build-tool))
           (default-directory root))
      (cond
       ((string= tool "mvn")
        (compile "mvn compile"))
       ((string= tool "gradle")
        (compile "./gradlew build"))
       (t
        (compile (format "javac %s" (shell-quote-argument buffer-file-name)))))))

  (defun me/java-clean ()
    "Clean build artifacts."
    (interactive)
    (let* ((root (me/java-project-root))
           (tool (me/java-build-tool))
           (default-directory root))
      (cond
       ((string= tool "mvn")
        (compile "mvn clean"))
       ((string= tool "gradle")
        (compile "./gradlew clean"))
       (t
        (message "No build tool detected.")))))

  (defun me/java-format ()
    "Format current buffer using google-java-format."
    (interactive)
    (if (executable-find "google-java-format")
        (progn
          (shell-command-to-string
           (format "google-java-format --aosp --replace %s"
                   (shell-quote-argument buffer-file-name)))
          (me/revert-buffer-no-confirm)
          (message "Formatted %s" (file-name-nondirectory buffer-file-name)))
      (message "google-java-format not found. Install it first.")))

  (defun me/java-check ()
    "Check current file/project for style violations using checkstyle."
    (interactive)
    (let* ((root (me/java-project-root))
           (tool (me/java-build-tool))
           (default-directory root))
      (if (string= tool "mvn")
          (compile "mvn checkstyle:check")
        ;; Fallback: just compile to check for errors
        (compile (format "javac %s" (shell-quote-argument buffer-file-name))))))

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(java-ts-mode . ("jdtls")))))
