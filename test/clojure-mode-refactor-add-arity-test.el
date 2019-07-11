;;; clojure-mode-refactor-add-arity.el --- Clojure Mode: refactor add arity  -*- lexical-binding: t; -*-

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


;;; Commentary:

;; Tests for clojure-add-arity

;;; Code:

(require 'clojure-mode)
(require 'buttercup)

(defmacro when-refactoring-with-point-it (description before after &rest body)
  "Return a buttercup spec.

Like when-refactor-it but also checks whether point is moved to the expected
position.

BEFORE is the buffer string before refactoring, where a pipe (|) represents
point.

AFTER is the expected buffer string after refactoring, where a pipe (|)
represents the expected position of point.

DESCRIPTION is a string with the description of the spec."
  `(it ,description
    (let* ((after ,after)
           (expected-cursor-pos (1+ (s-index-of "|" after)))
           (expected-state (delete ?| after)))
      (with-clojure-buffer ,before
        (goto-char (point-min))
        (search-forward "|")
        (delete-char -1)
        ,@body
        (expect (buffer-string) :to-equal expected-state)
        (expect (point) :to-equal expected-cursor-pos)))))

(describe "clojure-add-arity"

  (when-refactoring-with-point-it "should add an arity to a single-arity defn with args on same line"
    "(defn foo [arg]
  body|)"

    "(defn foo
  ([|])
  ([arg]
   body))"

    (clojure-add-arity))

  (when-refactoring-with-point-it "should add an arity to a single-arity defn with args on next line"
    "(defn foo
  [arg]
  bo|dy)"

    "(defn foo
  ([|])
  ([arg]
   body))"

    (clojure-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity defn with a docstring"
    "(defn foo
  \"some docst|ring\"
  [arg]
  body)"

    "(defn foo
  \"some docstring\"
  ([|])
  ([arg]
   body))"

    (clojure-add-arity))

  (when-refactoring-with-point-it "should handle a single-arity defn with metadata"
    "(defn fo|o
  ^{:bla \"meta\"}
  [arg]
  body)"

    "(defn foo
  ^{:bla \"meta\"}
  ([|])
  ([arg]
   body))"

    (clojure-add-arity))

  (when-refactoring-with-point-it "should add an arity to a multi-arity defn"
    "(defn foo
  ([arg1])
  ([ar|g1 arg2]
   body))"

    "(defn foo
  ([|])
  ([arg1])
  ([arg1 arg2]
   body))"

    (clojure-add-arity))

  (when-refactoring-with-point-it "should handle a multi-arity defn with a docstring"
    "(defn foo
  \"some docstring\"
  ([])
  ([arg|]
   body))"

    "(defn foo
  \"some docstring\"
  ([|])
  ([])
  ([arg]
   body))"

    (clojure-add-arity))

  (when-refactoring-with-point-it "should handle a multi-arity defn with metadata"
    "(defn foo
  \"some docstring\"
  ^{:bla \"meta\"}
  ([])
  |([arg]
   body))"

    "(defn foo
  \"some docstring\"
  ^{:bla \"meta\"}
  ([|])
  ([])
  ([arg]
   body))"

    (clojure-add-arity)))

(provide 'clojure-mode-refactor-add-arity-test)

;;; clojure-mode-refactor-add-arity-test.el ends here
