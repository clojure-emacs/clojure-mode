;;; clojure-mode-external-interaction-test.el --- Clojure Mode interactions with external packages test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020 Bozhidar Batsov <bozhidar@batsov.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'clojure-mode)
(require 'buttercup)
(require 'paredit)

(describe "Interactions with Paredit"
  ;; reuse existing when-refactoring-it macro
  (describe "it should insert a space"
    (when-refactoring-it "before lists"
      "foo"
      "foo ()"
      (paredit-mode)
      (paredit-open-round))
    (when-refactoring-it "before vectors"
      "foo"
      "foo []"
      (paredit-mode)
      (paredit-open-square))
    (when-refactoring-it "before maps"
      "foo"
      "foo {}"
      (paredit-mode)
      (paredit-open-curly))
    (when-refactoring-it "before strings"
      "foo"
      "foo \"\""
      (paredit-mode)
      (paredit-doublequote))
    (when-refactoring-it "after gensym"
      "foo#"
      "foo# ()"
      (paredit-mode)
      (paredit-open-round))
    (when-refactoring-it "after symbols ending with '"
      "foo'"
      "foo' ()"
      (paredit-mode)
      (paredit-open-round)))
  (describe "should not insert a space"
    (when-refactoring-it "for anonymous fn syntax"
      "foo #"
      "foo #()"
      (paredit-mode)
      (paredit-open-round))
    (when-refactoring-it "for hash sets"
      "foo #"
      "foo #{}"
      (paredit-mode)
      (paredit-open-curly))
    (when-refactoring-it "for regexes"
      "foo #"
      "foo #\"\""
      (paredit-mode)
      (paredit-doublequote))
    (when-refactoring-it "for quoted collections"
      "foo '"
      "foo '()"
      (paredit-mode)
      (paredit-open-round))
    (when-refactoring-it "for reader conditionals"
      "foo #?"
      "foo #?()"
      (paredit-mode)
      (paredit-open-round))))


(provide 'clojure-mode-external-interaction-test)


;;; clojure-mode-external-interaction-test.el ends here
