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

(describe "edn-mode indentation"

  (describe "lists use aligned indentation (not function-call style)"

    (it "should align list elements uniformly"
      (with-edn-buffer "\n(foo\nbar\nbaz)"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(foo\n bar\n baz)")))

    (it "should align nested lists uniformly"
      (with-edn-buffer "\n(foo\n(bar\nbaz))"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(foo\n (bar\n  baz))"))))

  (describe "forms that normally get special indentation are treated as plain data"

    (it "should align let arguments instead of using body indentation"
      (with-edn-buffer "\n(let [x 1]\nx)"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(let [x 1]\n x)")))

    (it "should align if arguments instead of using body indentation"
      (with-edn-buffer "\n(if true\n1\n2)"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(if true\n 1\n 2)")))

    (it "should align cond arguments instead of using body indentation"
      (with-edn-buffer "\n(cond a\nb)"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(cond a\n b)")))

    (it "should align do arguments instead of using body indentation"
      (with-edn-buffer "\n(do a\nb)"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(do a\n b)")))

    (it "should align keyword lists as data (issue #610)"
      (with-edn-buffer "\n(:key1 :value1\n:key2 :value2)"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(:key1 :value1\n :key2 :value2)"))))

  (describe "maps, vectors, and sets indent normally"

    (it "should indent map values"
      (with-edn-buffer "\n{:a 1\n:b 2}"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n{:a 1\n :b 2}")))

    (it "should indent vectors"
      (with-edn-buffer "\n[1\n2\n3]"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n[1\n 2\n 3]")))

    (it "should indent sets"
      (with-edn-buffer "\n#{:a\n:b\n:c}"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n#{:a\n  :b\n  :c}")))

    (it "should indent nested data structures"
      (with-edn-buffer "\n{:deps\n{org.clojure/clojure\n{:mvn/version \"1.11.1\"}}}"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n{:deps\n {org.clojure/clojure\n  {:mvn/version \"1.11.1\"}}}"))))

  (describe "mode configuration"

    (it "should use always-align indent style"
      (with-edn-buffer ""
        (expect clojure-indent-style :to-equal 'always-align)))

    (it "should have indent specs disabled"
      (with-edn-buffer ""
        (expect clojure-enable-indent-specs :to-equal nil)))))

(provide 'edn-mode-indentation-test)

;;; edn-mode-indentation-test.el ends here
