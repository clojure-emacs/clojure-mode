# Clojure Mode

Provides Emacs font-lock, indentation, and navigation for the
[Clojure language](http://clojure.org).

## Installation

It's easiest to install and keep Clojure Mode updated using
[package.el](http://bit.ly/pkg-el23). 

```lisp
;; add to ~/.emacs.d/init.el if you aren't already using Marmalade.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

* <kbd>M-x eval-buffer</kbd>
* <kbd>M-x package-refresh-contents</kbd>
* <kbd>M-x package-install clojure-mode</kbd>

If you use a version of Emacs prior to 24 that doesn't include
package.el, you can get it from http://bit.ly/pkg-el23. If you have an
older package.el installed from tromey.com, you should upgrade in
order to support installation from multiple sources.

Of course, it's possible to install from source if you don't mind
missing out on automated updates, byte-compilation, and autoloads.
Download `clojure-mode.el` and place it in the `~/.emacs.d/`
directory, creating it if it doesn't exist. Then add this to the file
`~/.emacs.d/init.el`:

```lisp
(add-to-list 'load-path "~/.emacs.d/")
(require 'clojure-mode)
```

## Paredit

Using clojure-mode with paredit is highly recommended. It is also
available using package.el from the above archive.

Use paredit as you normally would with any other mode; for instance:

```lisp
;; (require 'paredit) if you didn't install via package.el
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
```

## Basic REPL

Use <kbd>M-x run-lisp</kbd> to open a simple REPL subprocess using
[Leiningen](http://github.com/technomancy/leiningen). Once that has
opened, you can use <kbd>C-c C-r</kbd> to evaluate the region or
<kbd>C-c C-l</kbd> to load the whole file.

If you don't use Leiningen, you can set `inferior-lisp-program` to
a different REPL command.

## SLIME

You can also use [Leiningen](http://github.com/technomancy/leiningen)
to start an enhanced REPL via SLIME:

    $ lein plugin install swank-clojure 1.3.3
    
    M-x clojure-jack-in # from inside a project

## License

Copyright © 2007-2011 Jeffrey Chu, Lennart Staflin, Phil Hagelberg

Distributed under the GNU General Public License; see C-h t to view.
