# Minimal ~/.emacs.d - Vanilla Emacs Configuration with Better Defaults and Optimized Startup
![License](https://img.shields.io/github/license/jamescherti/be-quiet.el)
![](https://raw.githubusercontent.com/jamescherti/minimal-emacs.d/main/.images/made-for-gnu-emacs.svg)

The `minimal-emacs.d` project is a customizable Emacs base that provides **better Emacs defaults and optimized startup**, intended to serve as a solid foundation for a vanilla Emacs configuration.

Creating `minimal-emacs.d` (`init.el` and `early-init.el`) involved extensive research and testing to find the best parameters and optimizations for an Emacs init file. The concept behind `minimal-emacs.d` is to provide a clean, bloat-free, optimized base.

By default, only essential features are enabled, providing a minimal base that is optimized. From there, users are encouraged to consult this README.md to customize and extend the configuration based on their specific needs.

The author is using **[minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d)** as his `early-init.el` and `init.el`. He is using 146 packages and his Emacs configuration starts in 0.22 seconds:
![](https://www.jamescherti.com/wp-content/uploads/minimal-emacs-startup-time.png)

A [user commented on Reddit](https://www.reddit.com/r/emacs/comments/1feaf37/comment/lmw7ijd/) that after switching to `minimal-emacs.d`, their configuration execution time decreased from 3 seconds to just 1 second by simply replacing their `init.el` and `early-init.el` files with those from the project. Please [send me](https://www.jamescherti.com/contact/) your feedback and I'll add it to this README.md file.

(The optimizations in `minimal-emacs.d` significantly contribute to speeding up Emacs startup. Additional factors include deferring package loading when not necessary on startup by using `:defer t` with `use-package`, and byte-compiling and native-compiling all `.el` files. The author also regularly uses `M-x list-timers` and `M-x describe-mode` for each file type to ensure only essential modes and timers are active, which helps optimize Emacs' performance.)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Minimal ~/.emacs.d - Vanilla Emacs Configuration with Better Defaults and Optimized Startup](#minimal-emacsd---vanilla-emacs-configuration-with-better-defaults-and-optimized-startup)
    - [Install minimal-emacs.d](#install-minimal-emacsd)
        - [Installation into ~/.emacs.d](#installation-into-emacsd)
        - [Alternative: Installation in `~/.minimal-emacs.d`](#alternative-installation-in-minimal-emacsd)
    - [Update minimal-emacs.d](#update-minimal-emacsd)
    - [Customizations](#customizations)
        - [How to customize early-init.el and init.el?](#how-to-customize-early-initel-and-initel)
        - [How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?](#how-to-enable-the-menu-bar-the-tool-bar-dialogs-the-contextual-menu-and-tooltips)
        - [Reducing clutter in `~/.emacs.d` by redirecting files to `~/emacs.d/var/`](#reducing-clutter-in-emacsd-by-redirecting-files-to-emacsdvar)
        - [How to activate recentf, savehist, saveplace, and auto-revert?](#how-to-activate-recentf-savehist-saveplace-and-auto-revert)
        - [Optimization: Native Compilation](#optimization-native-compilation)
        - [How to configure vterm](#how-to-configure-vterm)
        - [How to configure Vertico, Consult, and Embark](#how-to-configure-vertico-consult-and-embark)
        - [How to configure Vim keybindings using Evil?](#how-to-configure-vim-keybindings-using-evil)
        - [Configuring LSP Servers with Eglot (built-in)](#configuring-lsp-servers-with-eglot-built-in)
        - [Code completion with corfu](#code-completion-with-corfu)
        - [How to configure straight.el?](#how-to-configure-straightel)
        - [Which other packages can be interesting to add?](#which-other-packages-can-be-interesting-to-add)
    - [Frequently asked questions](#frequently-asked-questions)
        - [How to increase gc-cons-threshold?](#how-to-increase-gc-cons-threshold)
        - [How to change the outline-mode or outline-minor-mode Ellipsis (...) to (▼)?](#how-to-change-the-outline-mode-or-outline-minor-mode-ellipsis--to-)
        - [How to make minimal-emacs.d use an environment variable to change ~/.emacs.d to another directory?](#how-to-make-minimal-emacsd-use-an-environment-variable-to-change-emacsd-to-another-directory)
        - [Are post-early-init.el and pre-init.el the same file in terms of the logic?](#are-post-early-initel-and-pre-initel-the-same-file-in-terms-of-the-logic)
        - [Why the reflexive disabling of the menu bar?](#why-the-reflexive-disabling-of-the-menu-bar)
        - [Why did the author develop minimal-emacs.d?](#why-did-the-author-develop-minimal-emacsd)
    - [Features](#features)
    - [Author and license](#author-and-license)
    - [Links](#links)

<!-- markdown-toc end -->

## Install minimal-emacs.d

### Installation into ~/.emacs.d

Execute the following command to clone this repository into `~/.emacs.d`:
```
git clone https://github.com/jamescherti/minimal-emacs.d ~/.emacs.d
```

### Alternative: Installation in `~/.minimal-emacs.d`

To install `minimal-emacs.d` in a non-default directory, use the `--init-directory` Emacs option to specify your desired configuration path. For example, to install `minimal-emacs.d` in `~/.minimal-emacs.d/`, follow these steps:

1. Clone the repository into `~/.minimal-emacs.d/` using:
   ```
   git clone https://github.com/jamescherti/minimal-emacs.d ~/.minimal-emacs.d
   ```

2. Start Emacs with the new configuration directory:
   ```
   emacs --init-directory ~/.minimal-emacs.d/
   ```

## Update minimal-emacs.d

To keep your Emacs configuration up to date, you can pull the latest changes from the repository. Run the following command in your terminal:

```
git -C ~/.emacs.d pull
```

## Customizations

### How to customize early-init.el and init.el?
The `init.el` and `early-init.el` files should never be modified directly because they are intended to be managed by Git during an update.

The minimal-emacs.d init files support additional customization files that are loaded at different stages of the Emacs startup process. These files allow you to further customize the initialization sequence:

- `~/.emacs.d/pre-init.el`: This file is loaded before `init.el`. Use it to set up variables or configurations that need to be available early in the initialization process but after `early-init.el`.

- `~/.emacs.d/post-init.el`: This file is loaded after `init.el`. It is useful for additional configurations or package setups that depend on the configurations in `init.el`.

- `~/.emacs.d/pre-early-init.el`: This file is loaded before `early-init.el`. Use it for configurations that need to be set even earlier in the startup sequence, typically affecting the initial setup of the Emacs environment.

- `~/.emacs.d/post-early-init.el`: This file is loaded after `early-init.el` but before `init.el`. It is useful for setting up configurations that depend on the early initialization but need to be set before the main initialization begins.

Always begin your `pre-init.el`, `post-init.el`, `post-early-init.el`, and `pre-early-init.el` files with the following header to prevent them from being byte-compiled and to activate lexical binding:
```elisp
;;; FILENAME.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
```

(Replace `FILENAME.el` with the actual name and DESCRIPTION with a brief description of its purpose.)

### How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?

**Note:** Enabling the tool-bar, menu-bar, and similar UI elements may slightly increase your startup time.

To customize your Emacs setup to include various user interface elements, you can use the following settings in your ``~/.emacs.d/pre-early-init.el``:

``` emacs-lisp
(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))
```

These settings control the visibility of dialogs, context menus, toolbars, menu bars, and tooltips.

### Reducing clutter in `~/.emacs.d` by redirecting files to `~/emacs.d/var/`

Emacs, by default, stores various configuration files, caches, backups, and other data in the `~/.emacs.d` directory. Over time, this directory can become cluttered with numerous files, making it difficult to manage and maintain.

One common solution to this issue is the installation of the no-littering package, which reduces clutter in the `~/.emacs.d` directory.

However, an alternative lightweight approach is to simply change the default `~/.emacs.d` directory to `~/.emacs.d/var/`, which will contain all the files that Emacs typically stores in the base directory. This can be accomplished by adding the following code to `~/.emacs.d/pre-early-init.el`:
```
;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)
```

To prevent Emacs from saving customization information to a custom file, set `custom-file` to `null-device`:
```
(setq custom-file null-device)
```

### How to activate recentf, savehist, saveplace, and auto-revert?

The recentf, savehist, saveplace, and auto-revert built-in packages are already configured by `minimal-emacs.d`. All you need to do is activate them by adding the following to `~/.emacs.d/post-init.el`:
``` emacs-lisp
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'recentf-mode)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)
```

### Optimization: Native Compilation

Check if native compilation is enabled by evaluating `(native-comp-available-p)` in Emacs. If the result is non-nil, it indicates that native compilation is active.

Native compilation can greatly enhance performance by translating Emacs Lisp code into native machine code, leading to faster execution and improved responsiveness.

### How to configure vterm

The `emacs-libvterm` package is a terminal emulator integrated into GNU Emacs. Built on libvterm, a C library, it offers superior performance compared to Elisp-based alternatives. This compiled code approach enables `emacs-libvterm` to handle large outputs efficiently, providing a fast and feature-complete terminal experience within Emacs.

To configure `emacs-vterm`, add the following to `~/.emacs.d/post-init.el`:
``` emacs-lisp
(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))
```

(Note that the `emacs-vterm` Emacs package requires compilation of its C components, which includes the gcc compiler and the `libvterm` library. On Debian or Ubuntu systems, the necessary packages can be installed with: `sudo apt-get install build-essential libvterm-dev libtool-bin cmake`)

### How to configure Vertico, Consult, and Embark

Vertico, Consult, and Embark collectively enhance Emacs' completion and navigation capabilities. Vertico provides a vertical completion interface, making it easier to navigate and select from completion candidates (e.g., when `M-x` is pressed). Consult offers a suite of commands for efficient searching, previewing, and interacting with buffers, file contents, and more, improving various tasks. Embark integrates with these tools to provide context-sensitive actions and quick access to commands based on the current selection, further improving user efficiency and workflow within Emacs. Together, they create a cohesive and powerful environment for managing completions and interactions.

Add the following to `~/.emacs.d/post-init.el` to set up Vertico, Consult, and Embark:
``` emacs-lisp
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))
```

### How to configure Vim keybindings using Evil?

Configuring Vim keybindings in Emacs can greatly enhance your editing efficiency if you are accustomed to Vim's modal editing style. Add the following to `~/.emacs.d/post-init.el` to set up Evil mode:

``` emacs-lisp
;; evil-want-keybinding must be declared before Evil and Evil Collection
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

```

You can also use the [vim-tab-bar](https://github.com/jamescherti/vim-tab-bar.el) Emacs package to `~/.emacs.d/post-init.el` to give the built-in Emacs tab-bar a style similar to Vim's tabbed browsing interface:
``` emacs-lisp
(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :hook (after-init . vim-tab-bar-mode))
```

You can also add `vdiff`, a package that provides Vimdiff-like functionality to Emacs:
``` emacs-lisp
(use-package vdiff
  :ensure t
  :defer t
  :commands (vdiff-buffers
             vdiff-buffers3
             vdiff-quit
             vdiff-files
             vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))
```

The `evil-visualstar` package allows using `*` or `#` search from the visual selection:
``` emacs-lisp
(use-package evil-visualstar
  :after evil
  :ensure t
  :defer t
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))
```

The `evil-surround` package simplifies handling surrounding characters, such as parentheses, brackets, quotes, etc. It provides key bindings to easily add, change, or delete these surrounding characters in pairs. For instance, you can surround the currently selected text with double quotes in visual state using `S"` or `gS"`:
``` emacs-lisp
(use-package evil-surround
  :after evil
  :ensure t
  :defer t
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))
```

You can also add the following code to enable commenting and uncommenting by pressing `gcc` in normal mode and `gc` in visual mode (thanks you to the Reddit user u/mistakenuser for this contribution, which replaces the evil-commentary package):
``` emacs-lisp
(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))
```

### Configuring LSP Servers with Eglot (built-in)

To set up Language Server Protocol (LSP) servers using Eglot, you can configure it in your Emacs setup as follows. This configuration ensures minimal disruption from Eglot’s progress reporting and optimizes performance by disabling unnecessary logging.

To configure `eglot`, add the following to `~/.emacs.d/post-init.el`:
``` emacs-lisp
(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)

  :custom
  (eglot-report-progress nil)  ; Prevent minibuffer spam

  :config
  ;; Optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil))
```

Here is an example of how to configure Eglot to enable or disable certain options for the `pylsp` server in Python development. (Note that a third-party tool, [python-lsp-server](https://github.com/python-lsp/python-lsp-server), must be installed):

``` emacs-lisp
(setq-default eglot-workspace-configuration
              `(:pylsp (:plugins
                        (;; Fix imports and syntax using `eglot-format-buffer`
                         :isort (:enabled t)
                         :autopep8 (:enabled t)

                         ;; Syntax checkers (works with Flymake)
                         :pylint (:enabled t)
                         :pycodestyle (:enabled t)
                         :flake8 (:enabled t)
                         :pyflakes (:enabled t)
                         :pydocstyle (:enabled t)
                         :mccabe (:enabled t)

                         :yapf (:enabled :json-false)
                         :rope_autoimport (:enabled :json-false)))))

(add-hook 'python-mode-hook #'eglot)
(add-hook 'python-ts-mode-hook #'eglot)
```

### Code completion with corfu

Corfu enhances in-buffer completion by displaying a compact popup with current candidates, positioned either below or above the point. Candidates can be selected by navigating up or down.

Cape, or Completion At Point Extensions, extends the capabilities of in-buffer completion. It integrates with Corfu or the default completion UI, by providing additional backends through completion-at-point-functions.

To configure `corfu` and `cape`, add the following to `~/.emacs.d/post-init.el`:
``` emacs-lisp
(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
```

### How to configure straight.el?

[Add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to `~/.emacs.d/pre-init.el`:
``` emacs-lisp
;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
```

### Which other packages can be interesting to add?

Add the following to `~/.emacs.d/post-init.el`:
``` emacs-lisp
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(pixel-scroll-precision-mode)

(display-time-mode)
```

## Frequently asked questions

### How to increase gc-cons-threshold?

Add the following to `~/.emacs.d/pre-early-init.el` to ensure that `minimal-emacs.d` restores the specified amount after startup:
``` emacs-lisp
(setq minimal-emacs-gc-cons-threshold (* 64 1024 1024))
```

### How to change the outline-mode or outline-minor-mode Ellipsis (...) to (▼)?

If you want to to change the outline-mode or outline-minor-mode Ellipsis (...) to (▼), use the code snippet in this article: [Changing the Ellipsis (“…”) in outline-mode and outline-minor-mode](https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/).

### How to make minimal-emacs.d use an environment variable to change ~/.emacs.d to another directory?

Add the following to the top of the `~/.emacs.d/pre-early-init.el` file to make `minimal-emacs.d` use the `MINIMAL_EMACS_USER_DIRECTORY` environment variable to change `~/.emacs.d` to another directory:
```emacs-lisp
;; Place this at the very beginning of pre-early-init.el
(let ((previous-minimal-emacs-user-directory (expand-file-name
                                              minimal-emacs-user-directory))
      (env-dir (getenv "MINIMAL_EMACS_USER_DIRECTORY")))
  (setq minimal-emacs-user-directory (if env-dir
                                         (expand-file-name env-dir)
                                       (expand-file-name user-emacs-directory)))
  (unless (string= minimal-emacs-user-directory
                   previous-minimal-emacs-user-directory)
    ;; Load pre-early-init.el from the new directory
    (minimal-emacs-load-user-init "pre-early-init.el")))
```

### Are post-early-init.el and pre-init.el the same file in terms of the logic?

During the execution of `early-init.el` (and `pre-early-init.el` and  `post-early-init.el`), Emacs has not yet loaded the graphical user interface (GUI). This file is used for configurations that need to be applied before the GUI is initialized, such as settings that affect the early stages of the Emacs startup process.

Thus, `post-early-init.el` and `pre-init.el` serve different purposes and are not the same.

### Why the reflexive disabling of the menu bar?

The menu bar is disabled by default in `minimal-emacs.d` to provide a minimal, distraction-free environment, which many experienced users prefer.

The menu bar can be re-enabled by adding the following configuration to `~/.emacs.d/pre-early-init.el`:
```
(setq minimal-emacs-ui-features '(menu-bar))
```

Other UI features can also be enabled by adding the following to `~/.emacs.d/pre-early-init.el`:
```
(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))
```

### Why did the author develop minimal-emacs.d?

The author began working on it after realizing that no existing starter kit offered a truly minimal setup with the flexibility for users to choose exactly what to include in their configuration.

## Features

1. **Performance Improvements:**
   - Increases the amount read from processes in a single chunk.
   - Prefers loading newer compiled files.
   - Reduces startup screen and message noise, including removing the "For information about GNU Emacs..." message.
   - Configures Emacs to start with a scratch buffer in `fundamental-mode` to shave seconds off startup time.
   - Delays garbage collection during startup to improve performance and resets it to a more reasonable value once Emacs has started.
   - Customizes `file-name-handler-alist` for improved startup time and package load time (Special thanks to the Doom Emacs developers for the `file-name-handler-alist` optimizations; This function have been inspired by their project and will contribute to improving vanilla Emacs configurations.)
   - Reduces rendering workload by not rendering cursors or regions in non-focused windows.
   - Disables warnings from the legacy advice API and suppresses warnings about aliased variables.
   - Avoids unnecessary excessive UI updates.
   - Disables font compacting to avoid high memory usage.
   - Defer tool bar setup
   - Unset command line options irrelevant to the current OS

2. **Native Compilation and Byte Compilation:**
   - Configures native compilation and byte compilation settings
   - Suppresses compiler warnings and errors during native compilation.

4. **UI Element Management:**
   - Disables the startup screen and messages, including menu bar, tool bar, and scroll bars.
   - Configures Emacs to avoid resizing frames and minimizes unnecessary UI updates.

5. **Package Management:**
   - Configures package archives and priorities for MELPA, ELPA, and other repositories.

6. **Customizable Initialization Files:**
   - Supports additional configuration files (`pre-init.el`, `post-init.el`, `pre-early-init.el`, and `post-early-init.el`) to allow further customization at different stages of the startup process.

7. **File Management:**
   - Manages auto-save and backup files, including backup options and version control settings.

8. **User Experience Enhancements:**
   - Configures user interface settings such as cursor behavior, scrolling, and response to prompts.
   - Disables beeping and blinking to avoid distractions.

9. **Buffer and Window Configuration:**
   - Sets default fringe widths and buffer boundaries.
   - Configures smooth scrolling and cursor behavior for a more seamless editing experience.

10. **Miscellaneous**
    - Configure recentf, savehist, and auto-save
    - Configure Ediff to use a single frame and split windows horizontally

## Author and license

The `minimal-emacs.d` project has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program (in the .LICENSE file).

## Links

- [minimal-emacs.d @GitHub](https://github.com/jamescherti/minimal-emacs.d)

Other Emacs packages by the same author:
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
