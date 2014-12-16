[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![travis][badge-travis]][travis]

# Clojure Mode

Provides Emacs font-lock, indentation, and navigation for the
[Clojure programming language](http://clojure.org).

More thorough walkthroughs are available at
[clojure-doc.org](http://clojure-doc.org/articles/tutorials/emacs.html)
and [Clojure for the Brave and the True](http://www.braveclojure.com/basic-emacs/).

## Installation

Available on all major `package.el` community maintained repos -  [MELPA Stable][],
[MELPA][] and [Marmalade][] repos.

MELPA Stable and Marmalade are recommended as they have the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

You can install `clojure-mode` using the following command:

<kbd>M-x package-install [RET] clojure-mode [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

### Extra font-locking

Prior to version 3.0 `clojure-mode` bundled **unreliable**
font-locking for some built-in vars.  In 3.0 this was extracted from
`clojure-mode` and moved to a separate package -
[clojure-mode-extra-font-locking][].

## Configuration

To see a list of available configuration options do `M-x customize-group RET clojure`.

### Indentation options

The default indentation rules in `clojure-mode` are derived from the
[community Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide).

Characterizing them is difficult to do in summary; this is one
attempt:

1. Bodies of parenthesized forms are indented such that arguments are aligned to
  the start column of the first argument, _except_ for a class of forms
  identified by the symbol in function position, the bodies of which are
  indented two spaces, regardless of the position of their first argument (this
  is called "defun" indentation, for historical reasons):
  1. Known special forms (e.g. `loop`, `try`, etc)
  2. Nearly all "core" macros that ship as part of Clojure itself
  3. Userland macros (and any other form?) that are locally registered via
  `put-clojure-indent`, `define-clojure-indent` (helpers for adding mappings to
  `clojure-indent-function`).
2. The bodies of certain more complicated macros and special forms
  (e.g. `letfn`, `deftype`, `extend-protocol`, etc) are indented using a
  contextual backtracking indentation method, controlled by
  `clojure-backtracking-indent`.
3. The bodies of other forms (e.g. vector, map, and set literals) are indented
  such that each new line within the form is set just inside of the opening
  delimiter of the form.

Please see the docstrings of the Emacs Lisp functions/vars noted above for
information about customizing this indentation behaviour.

## Related packages

* [clojure-mode-extra-font-locking][] provides additional font-locking
for built-in methods and macros.  The font-locking is pretty
imprecise, because it doesn't take namespaces into account and it
won't font-lock a functions at all possible positions in a sexp, but
if you don't mind its imperfections you can easily enable it:

```el
(require 'clojure-mode-extra-font-locking)
```

The code in `clojure-mode-font-locking` used to be bundled with
`clojure-mode` before version 3.0.

* [clj-refactor][] provides simple refactoring support.

* Enabling `CamelCase` support for editing commands(like
`forward-word`, `backward-word`, etc) in `clojure-mode` is quite
useful since we often have to deal with Java class and method
names. The built-in Emacs minor mode `subword-mode` provides such
functionality:

```el
(add-hook 'clojure-mode-hook 'subword-mode)
```

* The use of [paredit][] when editing Clojure (or any other Lisp) code
is highly recommended. It helps ensure the structure of your forms is
not compromised and offers a number of operations that work on code
structure at a higher level than just characters and words. To enable
it for Clojure buffers:

```el
(add-hook 'clojure-mode-hook 'paredit-mode)
```

* [smartparens][] is an excellent
  (newer) alternative to paredit. Many Clojure hackers have adopted it
  recently and you might want to give it a try as well. To enable
  `smartparens` use the following code:

```el
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
```

* [RainbowDelimiters][] is a
  minor mode which highlights parentheses, brackets, and braces
  according to their depth. Each successive level is highlighted in a
  different color. This makes it easy to spot matching delimiters,
  orient yourself in the code, and tell which statements are at a
  given depth. Assuming you've already installed `RainbowDelimiters` you can
  enable it like this:

```el
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
```

## REPL Interaction

A number of options exist for connecting to a running Clojure process
and evaluating code interactively.

### Basic REPL

Install [inf-clojure][] for basic interaction with a REPL process.

### CIDER

You can also use [Leiningen][] to start an
enhanced REPL via [CIDER][].

## Changelog

An extensive changelog is available [here](CHANGELOG.md).

## License

Copyright © 2007-2014 Jeffrey Chu, Lennart Staflin, Phil Hagelberg, Bozhidar Batsov
and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/clojure-mode-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/clojure-mode-badge.svg
[melpa-package]: http://melpa.org/#/clojure-mode
[melpa-stable-package]: http://stable.melpa.org/#/clojure-mode
[marmalade]: https://marmalade-repo.org
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[badge-travis]: https://travis-ci.org/clojure-emacs/clojure-mode.svg?branch=master
[travis]: https://travis-ci.org/clojure-emacs/clojure-mode
[CIDER]: https://github.com/clojure-emacs/cider
[inf-clojure]: https://github.com/clojure-emacs/inf-clojure
[Leiningen]: http://leiningen.org
[contributors]: https://github.com/clojure-emacs/clojure-mode/contributors
[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org
[clojure-mode-extra-font-locking]: https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode-extra-font-locking.el
[clj-refactor]: https://github.com/clojure-emacs/clj-refactor.el
[paredit]: http://mumble.net/~campbell/emacs/paredit.html
[smartparens]: https://github.com/Fuco1/smartparens
[RainbowDelimiters]: https://github.com/Fanael/rainbow-delimiters
