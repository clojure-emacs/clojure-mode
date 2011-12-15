# Clojure Mode

Provides Emacs font-lock, indentation, and navigation for the
[Clojure language](http://clojure.org).

## Installation

If you use [package.el](http://bit.ly/pkg-el23), you can install with
`M-x package-install clojure-mode`. Otherwise you can do a manual
install by downloading `clojure-mode.el` and placing it in the
`~/.emacs.d/` directory, creating it if it doesn't exist. Then add
this to the file `~/.emacs.d/init.el`:

```lisp
(add-to-list 'load-path "~/.emacs.d/")
(require 'clojure-mode)
```

### Setting up package.el

If you use package.el but haven't added
[Marmalade](http://marmalade-repo.org), the community package source,
yet, add this to `~/.emacs.d/init.el`:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

Then do this to load the package listing:

* <kbd>M-x eval-buffer</kbd>
* <kbd>M-x package-refresh-contents</kbd>

If you use a version of Emacs prior to 24 that doesn't include
package.el, you can get it from http://bit.ly/pkg-el23. If you have an
older package.el installed from tromey.com, you should upgrade in
order to support installation from multiple sources.

## Clojure Test Mode

This source repository also includes `clojure-test-mode.el`, which
provides support for running Clojure tests (using the clojure.test
framework) via SLIME and seeing feedback in the test buffer about
which tests failed or errored. The installation instructions above
should work for clojure-test-mode as well.

Once you have a SLIME session active (see below), you can run the
tests in the current buffer with `C-c C-,`. Failing tests and errors
will be highlighted using overlays. To clear the overlays, use `C-c k`.

You can jump between implementation and test files with `C-c t` if
your project is laid out in a way that clojure-test-mode expects. Your
project root should have a src/ directory containing files that
correspond to their namespace. It should also have a test/ directory
containing files that correspond to their namespace, and the test
namespaces should mirror the implementation namespaces with the
addition of "test" as the second-to-last segment of the namespace.

So `my.project.frob` would be found in `src/my/project/frob.clj` and
its tests would be in `test/my/project/test/frob.clj` in the
`my.project.test.frob` namespace.

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
