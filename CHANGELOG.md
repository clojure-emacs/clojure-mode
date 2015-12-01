# Changelog

## master (unreleased)

### New features

* Don't font-lock the `:` in keywords.
* Indent and font-lock forms that start with `let-`, `while-` or `when-` like their counterparts.

### Bugs fixed

* Namespaces can now use the full palette of legal symbol characters.

## 5.0.1 (15/11/2015)

### Bugs fixed

* Don't treat the symbol `default-(something)` as def* macro.
* `cider-find-ns` now returns the closest `ns` instead of the first one.
* [#344](https://github.com/clojure-emacs/clojure-mode/issues/344): Fixed the indentation of `extend-type`.

## 5.0.0 (30/10/2015)

### New features

* [#302](https://github.com/clojure-emacs/clojure-mode/pull/302): Add new sexp navigation commands. `clojure-forward-logical-sexp` and `clojure-backward-logical-sexp` consider `^hints` and `#reader.macros` to be part of the sexp that follows them.
* [#303](https://github.com/clojure-emacs/clojure-mode/issues/303): Handle `boot` projects in `clojure-expected-ns`.
* Added dedicated modes for ClojureScript, ClojureC and ClojureX. All of them are derived from `clojure-mode`.
* Added support for Gradle projects.
* Vastly improved indentation engine.
* Added support for reader conditionals.
* Improved font-locking of namespaced symbols.

### Bugs fixed

* [#310](https://github.com/clojure-emacs/clojure-mode/issues/310) and [#311](https://github.com/clojure-emacs/clojure-mode/issues/311) Fix `clojure-expected-ns` in multi-source projects.
* [#307](https://github.com/clojure-emacs/clojure-mode/issues/307): Don't highlight `handle` and `handler-case` as keywords.
* Fix font-locking for def with special chars such as: `defn*`, `defspecial!`.
* Numerous indentation issues.

## 4.1.0 (20/06/2015)

### Changes

* Add `.cljc` to `auto-mode-alist`.
* [#281](https://github.com/clojure-emacs/clojure-mode/pull/281): Add support for namespace-prefixed definition forms.
* Remove `clojure-mark-string`.
* [#283](https://github.com/clojure-emacs/clojure-mode/pull/283): You can now specify different indentation settings for ns-prefixed symbols.
* [#285](https://github.com/clojure-emacs/clojure-mode/issues/285): Require Emacs 24.3+.

### Bugs fixed

* Prevent error when calling `indent-for-tab-command` at the start of
the buffer at end of line.
* [#274](https://github.com/clojure-emacs/clojure-mode/issues/274): Correct font-locking of certain punctuation character literals.
* Fix font-locking of namespace-prefixed dynamic vars (e.g. `some.ns/*var*`).
* [#284](https://github.com/clojure-emacs/clojure-mode/issues/284): Fix the indentation of the `are` macro.

## 4.0.1 (19/12/2014)

### Bugs fixed

* Indent properly `as->`.
* Revert the indentation settings for `->`, `->>`, `some->` and `some->>`.

## 4.0.0 (12/12/2014)

### Changes

* Removed `inferior-lisp` integration in favor of `inf-clojure`.
* Indented the body of `cond` with 2 spaces.
* Removed special indentation settings for `defstruct`, `struct-map` and `assoc`.
* Added special indentation settings for `->`, `->>`, `cond->`, `cond->>`, `some->` and `some->>`.

## 3.0.1 (24/11/2014)

### Bugs fixed

* Numerous font-lock bug fixes.
* [#260](https://github.com/clojure-emacs/clojure-mode/pull/260): Don't treat `@` as a word character.
* [#239](https://github.com/clojure-emacs/clojure-mode/issues/239): Indent properly multi-arity definitions.

## 3.0.0 (2/9/2014)

### New features

* Added font-locking for namespaces and namespace aliases.
* Added font-locking for character literals.
* Added font-locking for constants.
* Added font-locking for dynamic vars.
* Added font-locking for `cljx`.
* Various docstring filling improvements.
* Introduced additional faces for keyword literals, character literals and
interop method invocations.
* Added support for `prettify-symbols-mode`.

### Changes

* Emacs 24.1 is required.
* Removed deprecated `clojure-font-lock-comment-sexp`.
* Renamed `clojure-mode-font-lock-setup` to `clojure-font-lock-setup`.
* Some font-locking was extracted to a separate package. ([clojure-mode-extra-font-locking](https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode-extra-font-locking.el)).

### Bugs fixed

* Properly font-lock docstrings regardless of the presence of metadata or type hints.
