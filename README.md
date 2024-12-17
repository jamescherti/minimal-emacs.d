# Minimal ~/.emacs.d - Vanilla Emacs Configuration with Better Defaults and Optimized Startup
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![](https://raw.githubusercontent.com/jamescherti/minimal-emacs.d/main/.images/made-for-gnu-emacs.svg)

The **minimal-emacs.d** project is a customizable Emacs base that provides **better Emacs defaults and optimized startup**, intended to serve as a solid foundation for a vanilla Emacs configuration.

Creating *minimal-emacs.d* `init.el` and `early-init.el` involved extensive research and testing to find the best parameters and optimizations for an Emacs init file. The concept behind *minimal-emacs.d* is to provide a clean, bloat-free, fast base.

By default, only essential features are enabled, providing a minimal base that is optimized. From there, users are encouraged to consult this README.md to customize and extend the configuration based on their specific needs. (Read: [Never modify init.el and early-init.el. Modify these instead...](#never-modify-initel-and-early-initel-modify-these-instead))

The author is using **[minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d)** as his `early-init.el` and `init.el`. He is using 146 packages and his Emacs configuration starts in 0.22 seconds:
![](https://www.jamescherti.com/wp-content/uploads/minimal-emacs-startup-time.png)

(The optimizations in *minimal-emacs.d* significantly contribute to speeding up Emacs startup. Additional factors include deferring package loading when not necessary on startup by using `:defer t` with `use-package`, and using [compile-angel](https://github.com/jamescherti/compile-angel.el) to ensure all `.el` files are byte-compiled and native-compiled. The author also regularly uses `M-x list-timers` and `M-x describe-mode` for each file type to ensure only essential modes and timers are active, which helps optimize Emacs' performance)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [Minimal ~/.emacs.d - Vanilla Emacs Configuration with Better Defaults and Optimized Startup](#minimal-emacsd---vanilla-emacs-configuration-with-better-defaults-and-optimized-startup)
  - [Install minimal-emacs.d](#install-minimal-emacsd)
    - [Install minimal-emacs.d into `~/.emacs.d`](#install-minimal-emacsd-into-emacsd)
    - [Alternative: Install minimal-emacs.d into `~/.minimal-emacs.d`](#alternative-install-minimal-emacsd-into-minimal-emacsd)
  - [Update minimal-emacs.d](#update-minimal-emacsd)
  - [Customizations](#customizations)
    - [Never modify init.el and early-init.el. Modify these instead...](#never-modify-initel-and-early-initel-modify-these-instead)
    - [How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?](#how-to-enable-the-menu-bar-the-tool-bar-dialogs-the-contextual-menu-and-tooltips)
    - [Compile-Angel - Speed up Emacs by Automatically Byte-compiling and Native-compiling all .el files](#compile-angel---speed-up-emacs-by-automatically-byte-compiling-and-native-compiling-all-el-files)
    - [Reducing clutter in `~/.emacs.d` by redirecting files to `~/emacs.d/var/`](#reducing-clutter-in-emacsd-by-redirecting-files-to-emacsdvar)
    - [How to activate recentf, savehist, saveplace, and auto-revert?](#how-to-activate-recentf-savehist-saveplace-and-auto-revert)
    - [Optimization: Native Compilation](#optimization-native-compilation)
    - [How to configure vterm](#how-to-configure-vterm)
    - [How to configure Vertico, Consult, and Embark](#how-to-configure-vertico-consult-and-embark)
    - [How to configure Vim keybindings using Evil?](#how-to-configure-vim-keybindings-using-evil)
    - [Configuring LSP Servers with Eglot (built-in)](#configuring-lsp-servers-with-eglot-built-in)
    - [Code completion with corfu](#code-completion-with-corfu)
    - [How to configure straight.el?](#how-to-configure-straightel)
    - [How to configure elpaca (package manager)](#how-to-configure-elpaca-package-manager)
    - [Which other customizations can be interesting to add?](#which-other-customizations-can-be-interesting-to-add)
  - [Frequently asked questions](#frequently-asked-questions)
    - [How to load a local lisp file for machine-specific configurations?](#how-to-load-a-local-lisp-file-for-machine-specific-configurations)
    - [How to load Emacs customizations?](#how-to-load-emacs-customizations)
    - [How to increase gc-cons-threshold?](#how-to-increase-gc-cons-threshold)
    - [How to change the outline-mode or outline-minor-mode Ellipsis (...) to (▼)?](#how-to-change-the-outline-mode-or-outline-minor-mode-ellipsis--to-)
    - [How to make minimal-emacs.d use an environment variable to change ~/.emacs.d to another directory?](#how-to-make-minimal-emacsd-use-an-environment-variable-to-change-emacsd-to-another-directory)
    - [Are post-early-init.el and pre-init.el the same file in terms of the logic?](#are-post-early-initel-and-pre-initel-the-same-file-in-terms-of-the-logic)
    - [Why the reflexive disabling of the menu bar?](#why-the-reflexive-disabling-of-the-menu-bar)
    - [Why did the author develop minimal-emacs.d?](#why-did-the-author-develop-minimal-emacsd)
    - [How to keep minimal-emacs.d pre-\*.el and post-\*.el files in a separate directory?](#how-to-keep-minimal-emacsd-pre-el-and-post-el-files-in-a-separate-directory)
    - [How to make *minimal-emacs.d* install packages in the early-init phase instead of the init phase?](#how-to-make-minimal-emacsd-install-packages-in-the-early-init-phase-instead-of-the-init-phase)
    - [Are there any comments from users?](#are-there-any-comments-from-users)
  - [Features](#features)
  - [Author and license](#author-and-license)
  - [Links](#links)

<!-- markdown-toc end -->

## Install minimal-emacs.d

- **Important:** Ensure that the *~/.emacs* and *~/.emacs.el* files do not exist. These files takes precedence over *init.el*, causing Emacs to ignore the *init.el* file that is in the *~/.emacs.d/* directory or any other directory specified with the *--init-directory* Emacs command-line argument. This behavior is due to the way Emacs searches for initialization files ([more information](https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html#Find-Init)). **Simply delete the *~/.emacs* and *~/.emacs.el* files avoid this issue.**
- Prerequisite: git

### Install minimal-emacs.d into `~/.emacs.d`

Execute the following command install this repository into `~/.emacs.d`:
```
git clone https://github.com/jamescherti/minimal-emacs.d ~/.emacs.d
```

### Alternative: Install minimal-emacs.d into `~/.minimal-emacs.d`

To install *minimal-emacs.d* in a non-default directory, use the `--init-directory` Emacs option to specify your desired configuration path. For example, to install *minimal-emacs.d* in `~/.minimal-emacs.d/`, follow these steps:

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

### Never modify init.el and early-init.el. Modify these instead...
**The `init.el` and `early-init.el` files should never be modified directly** because they are intended to be managed by Git during an update.

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

### Compile-Angel - Speed up Emacs by Automatically Byte-compiling and Native-compiling all .el files

The [compile-angel.el](https://github.com/jamescherti/compile-angel.el) package automatically byte-compiles and native-compiles Emacs Lisp libraries. It offers:
- `(compile-angel-on-load-mode)`: A global mode that compiles .el files before they are loaded.
- `(compile-angel-on-save-local-mode)`: A local mode that compiles .el files whenever the user saves them.

The *compile-angel* modes **speed up Emacs by ensuring all libraries are byte-compiled and native-compiled**. Byte-compilation reduces the overhead of loading Emacs Lisp code at runtime, while native compilation optimizes performance by generating machine code specific to your system.

To install compile-angel, add the following code **at the very beginning of your ~/.emacs.d/post-init.el init.el file, before all other packages**:
```emacs-lisp
(use-package compile-angel
  :ensure t
  :demand t
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))
```

It is also highly recommended to set the following variables **at the very beginning of your post-init.el**:

``` emacs-lisp
;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1
```

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

The recentf, savehist, saveplace, and auto-revert built-in packages are already configured by *minimal-emacs.d*. All you need to do is activate them by adding the following to `~/.emacs.d/post-init.el`:
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
;; Tip: You can remove the `vertico-mode' use-package and replace it
;;      with the built-in `fido-vertical-mode'.
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
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
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

Evil-snipe provides 2-character motions for quickly jumping around text compared to Evil's built-in f/F/t/T motions, incrementally highlighting candidate targets as you type. By default, snipe only binds s (forward) and S (backward) to evil-snipe-s and evil-snipe-S, respectively. In operator mode, snipe is bound to z/Z and x/X (exclusive):
``` emacs-lisp
(use-package evil-snipe
  :defer t
  :commands evil-snipe-mode
  :hook (after-init . evil-snipe-mode))
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

### How to configure elpaca (package manager)

Add to `~/.emacs.d/pre-early-init.el`:
```elisp
;; By default, minimal-emacs-package-initialize-and-refresh is set to t, which
;; makes minimal-emacs.d call the built-in package manager. Since Elpaca will
;; replace the package manager, there is no need to call it.
(setq minimal-emacs-package-initialize-and-refresh nil)
```

(According to arthsmn, a *minimal-emacs.d* user, the change above also improves startup time. [In this user's case](https://github.com/jamescherti/minimal-emacs.d/pull/22), the startup time decreased from 1.06 seconds to 0.56 seconds.)

And [add the elpaca bootstrap code](https://github.com/progfolio/elpaca?tab=readme-ov-file#installer) to `~/.emacs.d/pre-init.el`:
```elisp
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Optional: Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
```

### Which other customizations can be interesting to add?

1. Read the following article from the same author: [Essential Emacs Packages for Efficient Software Development and Text Editing](https://www.jamescherti.com/essential-emacs-packages/)

2. You can also add the following to `~/.emacs.d/post-init.el`:
```emacs-lisp
;; Hide warnings and display only errors
(setq warning-minimum-level :error)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq pixel-scroll-precision-use-momentum nil)
(pixel-scroll-precision-mode 1)

(display-time-mode)
(show-paren-mode +1)  ; Paren match highlighting
(winner-mode 1)
(delete-selection-mode 1)  ; Replace selected text with typed text
(pixel-scroll-precision-mode 1)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)
```

It is also recommended to read the following articles:
- [Automating Table of Contents Update for Markdown Documents (e.g., README.md)](https://www.jamescherti.com/emacs-markdown-table-of-contents-update-before-save/)
- [Maintaining proper indentation in indentation-sensitive programming languages](https://www.jamescherti.com/elisp-code-and-emacs-packages-for-maintaining-proper-indentation-in-indentation-sensitive-languages-such-as-python-or-yaml/)

## Frequently asked questions

### How to load a local lisp file for machine-specific configurations?

Add the following line to the end of your `post-init.el` file:
```lisp
(minimal-emacs-load-user-init "local.el")
```

This allows `local.el` to load, enabling custom configurations specific to the machine.

(Ensure that `local.el` is in the same directory as `post-init.el`.)

### How to load Emacs customizations?

To load customizations saved by Emacs (`M-x customize`), add the following code snippet to the `post-init.el` file. This ensures that the custom file, typically set to a separate file for user preferences, is loaded without errors or messages during startup:

```elisp
(load custom-file 'noerror 'nomessage)
```

### How to increase gc-cons-threshold?

Add the following to `~/.emacs.d/pre-early-init.el` to ensure that *minimal-emacs.d* restores the specified amount after startup:
```emacs-lisp
(setq minimal-emacs-gc-cons-threshold (* 64 1024 1024))
```

### How to change the outline-mode or outline-minor-mode Ellipsis (...) to (▼)?

If you want to to change the outline-mode or outline-minor-mode Ellipsis (...) to (▼), use the code snippet in this article: [Changing the Ellipsis (“…”) in outline-mode and outline-minor-mode](https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/).

### How to make minimal-emacs.d use an environment variable to change ~/.emacs.d to another directory?

Add the following to the top of the `~/.emacs.d/pre-early-init.el` file to make *minimal-emacs.d* use the `MINIMAL_EMACS_USER_DIRECTORY` environment variable to change `~/.emacs.d` to another directory:
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

The menu bar is disabled by default in *minimal-emacs.d* to provide a minimal, distraction-free environment, which many experienced users prefer.

The menu bar can be re-enabled by adding the following configuration to `~/.emacs.d/pre-early-init.el`:
```elisp
(setq minimal-emacs-ui-features '(menu-bar))
```

Other UI features can also be enabled by adding the following to `~/.emacs.d/pre-early-init.el`:
```elisp
(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))
```

### Why did the author develop minimal-emacs.d?

The author began working on it after realizing that no existing starter kit offered a truly minimal setup with the flexibility for users to choose exactly what to include in their configuration.

### How to keep minimal-emacs.d pre-\*.el and post-\*.el files in a separate directory?

To ensure the *minimal-emacs.d* configuration loads `post-early-init.el`, `pre-init.el`, and `post-init.el` from a different directory, such as `~/.config/minimal-emacs.d/`, modify the `minimal-emacs-user-directory` variable by adding the following to your `~/.emacs.d/pre-early-init.el` file:
```elisp
(setq minimal-emacs-user-directory "~/.config/minimal-emacs.d/")
```

This will ensure that the *minimal-emacs.d* configuration loads `post-early-init.el`, `pre-init.el`, and `post-init.el` from `~/.config/minimal-emacs.d/`.

Keep in mind that if you change the `minimal-emacs-user-directory`, *minimal-emacs.d* will attempt to load the rest of the configuration from that directory (e.g., `~/.config/minimal-emacs/post-early-init.el`, `~/.config/minimal-emacs/pre-init.el` and `~/.config/minimal-emacs/post-init.el`, etc.).

### How to make *minimal-emacs.d* install packages in the early-init phase instead of the init phase?

To install and load packages during the early-init phase, add the following to `post-early-init.el`:

```elisp
(setq minimal-emacs-package-initialize-and-refresh nil)

;; If you want to ignore the warning:
;; "Warning (package): Unnecessary call to package-initialize in init file."
;; Uncomment the following setq:
;; (setq warning-suppress-types '((package)))

;; Initialize packages in the early-init phase instead of init
(progn
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (eval-when-compile
    (require 'use-package)))

;; TODO: Add your use-package code here
```

A drawback of using the early-init phase instead of init is that if a package fails (e.g, due to a network issue), no output will be displayed in the Emacs GUI. You will need to open a terminal to view Emacs's stdout for error messages.

### Are there any comments from users?

- [Leading_Ad6415 commented on Reddit](https://www.reddit.com/r/emacs/comments/1feaf37/comment/lmw7ijd/) that after switching to *minimal-emacs.d*, their configuration execution time decreased from 3 seconds to just 1 second by simply replacing their `init.el` and `early-init.el` files with those from the project.
- [Another user commented on Reddit](https://www.reddit.com/r/emacs/comments/1feaf37/comment/lrsfd64/), highlighting how a minimal-emacs.d significantly enhanced their Emacs performance. They reported substantial startup time reductions on both their main machine (from ~2.25 to ~0.95 seconds) and an older laptop (from ~2.95 to ~1.27 seconds) while also experiencing a generally snappier performance within Emacs. The user expressed gratitude for the project, calling it fantastic.
- [Cyneox commented on Reddit](https://www.reddit.com/r/emacs/comments/1gh687a/comment/lwdv18t/), expressing gratitude for the resource and sharing their experience. They mentioned it was their fourth attempt to set up a vanilla configuration and highlighted that they had been using the repository as a foundation for their customizations over the past few days. They appreciated the absence of unexplained behavior and the clear instructions on where to place files. The user reported successful testing on both Linux and macOS, noting that everything functioned smoothly, including in the terminal.

Please [send me](https://www.jamescherti.com/contact/) your feedback and I'll add it to this README.md file.

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

The *minimal-emacs.d* project has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program (in the .LICENSE file).

## Links

- [minimal-emacs.d @GitHub](https://github.com/jamescherti/minimal-emacs.d)

Other Emacs packages by the same author:
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
