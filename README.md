# Minimal ~/.emacs.d - Efficient Emacs startup and configuration

Optimizing Emacs for speed and efficiency involves fine-tuning its startup process, managing essential settings, and handling package installations, etc.

This repository hosts a minimal Emacs configuration with `early-init.el` and `init.el` files. It is designed to serve as a base for your vanilla Emacs configuration, offering a robust foundation for a better vanilla Emacs experience.

The author is using [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) as his `early-init.el` and `init.el`. He is using 152 packages and his Emacs configuration starts in 0.44 seconds:
![](https://raw.githubusercontent.com/jamescherti/minimal-emacs.d/main/.images/emacs-startup.png)

(The optimizations in minimal-emacs.d contribute significantly to speeding up Emacs startup. Other factors that helped: He defers loading packages when it is not necessary to load them on startup by using `:defer t` with `use-package`. He also utilizes byte compilation and native compilation. )

## Table of contents
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
- [Installation](#installation)
- [Features](#features)
- [Update](#update)
- [Customizations](#customizations)
- [Frequently asked questions](#frequently-asked-questions)
    - [Are post-early-init.el and pre-init.el the same file in terms of the logic?](#question-are-post-early-initel-and-pre-initel-the-same-file-in-terms-of-the-logic)
    - [How to configure Vim keybindings using Evil?](#how-to-configure-vim-keybindings-using-evil)
    - [How to configure straight.el?](#how-to-configure-straightel)
- [How long does the author's Emacs configuration take to start?](#how-long-does-the-authors-emacs-configuration-take-to-start)
- [License](#license)
- [Links](#links)
<!-- markdown-toc end -->

## Installation

Execute the following command to clone this repository into `~/.emacs.d`:
```
git clone https://github.com/jamescherti/minimal-emacs.d ~/.emacs.d
```

## Features

1. **Performance Improvements:**
   - Increases the amount read from processes in a single chunk.
   - Customizes `file-name-handler-alist` for improved startup time and package load time.
   - Reduces rendering workload by not rendering cursors or regions in non-focused windows.
   - Disables warnings from the legacy advice API and suppresses warnings about aliased variables.
   - Avoids unnecessary excessive UI updates.
   - Disables font compacting to avoid high memory usage.
   - Prefers loading newer compiled files.
   - Reduces startup screen and message noise, including removing the "For information about GNU Emacs..." message.
   - Configures Emacs to start with a scratch buffer in `fundamental-mode` to shave seconds off startup time.
   - Delays garbage collection during startup to improve performance and resets it to a more reasonable value once Emacs has started.

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

## Update

To keep your Emacs configuration up to date, you can pull the latest changes from the repository. Run the following command in your terminal:

```
git -C ~/.emacs.d pull
```

## Customizations

The `init.el` and `early-init.el` files should never be modified directly because they are intended to be managed by Git during an update.

The minimal-emacs.d init files support additional customization files that are loaded at different stages of the Emacs startup process. These files allow you to further customize the initialization sequence:

- `~/.emacs.d/pre-init.el`: This file is loaded before `init.el`. Use it to set up variables or configurations that need to be available early in the initialization process but after `early-init.el`.

- `~/.emacs.d/post-init.el`: This file is loaded after `init.el`. It is useful for additional configurations or package setups that depend on the configurations in `init.el`.

- `~/.emacs.d/pre-early-init.el`: This file is loaded before `early-init.el`. Use it for configurations that need to be set even earlier in the startup sequence, typically affecting the initial setup of the Emacs environment.

- `~/.emacs.d/post-early-init.el`: This file is loaded after `early-init.el` but before `init.el`. It is useful for setting up configurations that depend on the early initialization but need to be set before the main initialization begins.

## Frequently asked questions

### Are post-early-init.el and pre-init.el the same file in terms of the logic?

During the execution of `early-init.el` (and `pre-early-init.el` and  `post-early-init.el`), Emacs has not yet loaded the graphical user interface (GUI). This file is used for configurations that need to be applied before the GUI is initialized, such as settings that affect the early stages of the Emacs startup process.

Thus, `post-early-init.el` and `pre-init.el` serve different purposes and are not the same.

### How to configure Vim keybindings using Evil?

Configuring Vim keybindings in Emacs can greatly enhance your editing efficiency if you are accustomed to Vim's modal editing style. Add the following to `~/.emacs.d/post-init.el` to set up Evil mode:

``` emacs-lisp
(use-package evil
  :ensure t
  :custom
  (evil-undo-system 'undo-fu)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

```

You can also use the [vim-tab-bar](https://github.com/jamescherti/vim-tab-bar.el) Emacs package to `~/.emacs.d/post-init.el` to give the built-in Emacs tab-bar a style similar to Vim's tabbed browsing interface:
``` emacs-lisp
(use-package vim-tab-bar
  :ensure t
  :config
  (vim-tab-bar-mode 1))
```

### Automatically compile Emacs Lisp libraries

The auto-compile package automates the byte-compilation of Emacs Lisp files, ensuring that your code runs more efficiently by converting it to byte-code. This process reduces the load time and execution time of your Emacs configuration and other Lisp files, leading to faster performance. Additionally, auto-compile helps maintain an up-to-date and optimized configuration by recompiling files automatically when they are saved, eliminating the need for manual compilation and minimizing potential issues caused by outdated byte-code.

To activate auto-compile, add the following to the beginning of `~/.emacs.d/post-init.el`, before all other `use-package` statements:
``` emacs-lisp
(use-package auto-compile
  :ensure t
  :custom
  (auto-compile-check-parens nil)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))
```

### How to activate gcmh-mode?

The Garbage Collector Magic Hack (gcmh-mode) optimizes Emacs' garbage collection process, reducing the frequency of garbage collection during normal operations and only performing it during idle times. This results in smoother performance and fewer interruptions, especially during intensive tasks or when working with large files.

To activate gcmh-mode, add the following to the beginning of `~/.emacs.d/post-init.el`, before all other `use-package` statements:
``` emacs-lisp
(use-package gcmh
  :ensure t
  :hook (emacs-init . gcmh-mode))
```

### How to configure straight.el?

[Add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to `~/.emacs.d/pre-init.el`:
``` emacs-lisp
;; Users of Emacs versions >= 27 will want to set
;; package-enable-at-startup to nil
(setq package-enable-at-startup nil)

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

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [minimal-emacs.d @GitHub](https://github.com/jamescherti/minimal-emacs.d)
