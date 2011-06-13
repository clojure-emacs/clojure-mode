# Clojure Mode

Provides Emacs font-lock, indentation, and navigation for the
[Clojure language](http://clojure.org).

## Installation

It's easiest to install and keep Clojure Mode updated using
[package.el](http://bit.ly/pkg-el23). 

    ;; add to ~/.emacs.d/init.el if you aren't already using Marmalade.
    (require 'package)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (package-initialize)

    M-x eval-buffer

    M-x package-install clojure-mode

If you use a version of Emacs prior to 24 that doesn't include
package.el, you can get it from http://bit.ly/pkg-el23. If you have an
older package.el installed from tromey.com, you should upgrade in
order to support installation from multiple sources.

Of course, it's possible to just place it on your load-path and
require it as well if you don't mind missing out on
byte-compilation and autoloads.

## Paredit

Using clojure-mode with paredit is highly recommended. It is also
available using package.el from the above archive.

Use paredit as you normally would with any other mode; for instance:

    ;; (require 'paredit) if you didn't install via package.el
    (defun turn-on-paredit () (paredit-mode 1))
    (add-hook 'clojure-mode-hook 'turn-on-paredit)

## SLIME

You can use [Leiningen](http://github.com/technomancy/leiningen) for
better interaction with subprocesses via SLIME.

    $ wget https://github.com/technomancy/leiningen/raw/stable/bin/lein
    [place the "lein" script on your $PATH and make it executable]
    $ lein plugin install swank-clojure 1.3.1
    
    M-x clojure-jack-in # from inside a project

## License

Copyright © 2007-2011 Jeffrey Chu, Lennart Staflin, Phil Hagelberg

Distributed under the GNU General Public License; see C-h t to view.
