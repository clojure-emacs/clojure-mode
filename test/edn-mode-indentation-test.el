;;; edn-mode-indentation-test.el --- EDN Mode: indentation tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov <bozhidar@batsov.dev>

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for EDN mode indentation, verifying that data forms are indented
;; uniformly rather than with code-style special indentation.

;;; Code:

(require 'clojure-mode)
(require 'cl-lib)
(require 'buttercup)

(defmacro with-edn-buffer (text &rest body)
  "Create a temporary buffer, insert TEXT, switch to `edn-mode' and evaluate BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (erase-buffer)
     (insert ,text)
     (edn-mode)
     ,@body))

(defmacro when-indenting-edn-it (description &rest forms)
  "Return a buttercup spec.

Check that all FORMS correspond to properly indented sexps in `edn-mode'.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  `(it ,description
     (progn
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (edn-mode)
                      (insert "\n" ,form)
                      (indent-region (point-min) (point-max))
                      (expect (buffer-string) :to-equal ,(concat "\n" form))))
                 forms))))

(describe "edn-mode indentation"

  (describe "lists use aligned indentation (not function-call style)"

    (when-indenting-edn-it "should align list elements uniformly"
      "(foo\n bar\n baz)")

    (when-indenting-edn-it "should align nested lists uniformly"
      "(foo\n (bar\n  baz))"))

  (describe "forms that normally get special indentation are treated as plain data"

    (when-indenting-edn-it "should align let arguments instead of using body indentation"
      "(let [x 1]\n x)")

    (when-indenting-edn-it "should align if arguments instead of using body indentation"
      "(if true\n 1\n 2)")

    (when-indenting-edn-it "should align cond arguments instead of using body indentation"
      "(cond a\n b)")

    (when-indenting-edn-it "should align do arguments instead of using body indentation"
      "(do a\n b)")

    (when-indenting-edn-it "should align keyword lists as data (issue #610)"
      "(:key1 :value1\n :key2 :value2)"))

  (describe "maps, vectors, and sets indent normally"

    (when-indenting-edn-it "should indent map values"
      "{:a 1\n :b 2}")

    (when-indenting-edn-it "should indent vectors"
      "[1\n 2\n 3]")

    (when-indenting-edn-it "should indent sets"
      "#{:a\n  :b\n  :c}")

    (when-indenting-edn-it "should indent nested data structures"
      "{:deps\n {org.clojure/clojure\n  {:mvn/version \"1.11.1\"}}}"))

  (describe "mode configuration"

    (it "should use always-align indent style"
      (with-edn-buffer ""
        (expect clojure-indent-style :to-equal 'always-align)))

    (it "should have indent specs disabled"
      (with-edn-buffer ""
        (expect clojure-enable-indent-specs :to-equal nil)))))

(provide 'edn-mode-indentation-test)

;;; edn-mode-indentation-test.el ends here
