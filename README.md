# Minimal ~/.emacs.d

This repository hosts a minimal Emacs configuration with `early-init.el` and `init.el` files. It is designed to serve as a base for your vanilla Emacs configuration.

These files are designed to optimize startup time, configure essential settings, and manage package installations efficiently.

## Features

- Optimized garbage collection for faster startup.
- Suppression of unnecessary warnings and verbose outputs during byte compilation.
- Configuration of package archives (MELPA and ELPA).
- Disabling of unnecessary UI elements (menu bar, tool bar, scroll bar).
- Automatic installation of essential packages using use-package.
- Support for additional user-specific configurations.

## Installation

```
git clone https://github.com/jamescherti/minimal-emacs.d ~/.emacs.d
```


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

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [minimal-emacs.d @GitHub](https://github.com/jamescherti/minimal-emacs.d)
