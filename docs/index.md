Getting Started With Emacs
==========================

[GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) provides
excellent support for Clojure programming and is widely used within
the Clojure community.




Installation
============

For installing Emacs and generally finding your way around Emacs on
your platform, proceed now to one of the platform-specific docs:

  * [GNU/Linux setup](gnu-linux-setup.md)
  * [Mac OS X setup](os-x-setup.md)
  * [MS Windows setup](ms-windows-setup.md)



Configuration Overview
======================

Once you've got Emacs installed, typical configuration for use
with Clojure includes:

  * enabling the use of a remote Emacs package repository
  * installing clojure-mode (provides support for syntax highlighting
    and proper indentation for Clojure files (.clj files))
  * optionally enabling paredit mode (advanced support for working with
    expressions in parentheses/brackets)

and then deciding if you want to use an interactive interface to the
Clojure repl from inside of Emacs. If you want this, the two routes to
choose from are:

  * use the "inferior-lisp" mode, or
  * use SLIME and Swank

See below for discussion on each of these options.



Package Repository
------------------

Emacs packages extend the editor in various ways, and (as of version
24) Emacs can install them from a remote repository.  All the packages
we'll need here can be found in the [Melpa](http://melpa.milkbox.net/)
repository. Tell Emacs about melpa by adding the following to your
~/.emacs file:

~~~{.scheme}
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
~~~



Clojure Mode
------------

Emacs uses so-called "major modes" (as in, "mode of operation") to
provide filetype-specific editor support. For example, you edit a Java
file using the Emacs java mode, and Clojure using a clojure
mode. Emacs doesn't yet come with a Clojure mode by default, so we
must install it.

Install clojure-mode while running Emacs. Hit "M-x package-install RET
clojure-mode". After doing this, Emacs should automatically recognize
.clj files as Clojure files when you open them.

See [clojure mode](clojure-mode.md) for more info on using it.




Paredit Mode
------------

todo




REPL Interaction
================

If you like, you can simply interact with the Clojure repl that
[Leiningen](http://leiningen.org/index.html) provides for you (via
`lein repl`) --- totally separate from Emacs. However, Emacs can
provide access to the repl from within an Emacs buffer if you prefer:

  * The simplest way to interact with the repl from within Emacs is to
    [use inferior-mode](inferior-mode.md).

  * For more functionality, [use slime & swank](slime-swank.md).




Detailed Emacs Documentation
============================

GNU Emacs provides excellent built-in documention which is available
within the editor (see "C-h") and also
[online](http://www.gnu.org/software/emacs/manual/html_node/emacs/index.html).
