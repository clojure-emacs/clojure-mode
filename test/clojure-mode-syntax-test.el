;;; clojure-mode-syntax-test.el --- Clojure Mode: syntax related tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Bozhidar Batsov <bozhidar@batsov.com>

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

;; The unit test suite of Clojure Mode

;;; Code:

(require 'clojure-mode)
(require 'ert)

(defun non-func (form-a form-b)
  (with-temp-buffer
    (clojure-mode)
    (insert form-a)
    (save-excursion (insert form-b))
    (clojure--not-function-form-p)))

(ert-deftest non-function-form ()
  (dolist (form '(("#?@ " "(c d)")
                  ("#?@" "(c d)")
                  ("#? " "(c d)")
                  ("#?" "(c d)")
                  ("" "[asda]")
                  ("" "{a b}")
                  ("#" "{a b}")
                  ("" "(~)")))
    (should (apply #'non-func form)))
  (dolist (form '("(c d)"
                  "(.c d)"
                  "(:c d)"
                  "(c/a d)"
                  "(.c/a d)"
                  "(:c/a d)"
                  "(c/a)"
                  "(:c/a)"
                  "(.c/a)"))
    (should-not (non-func "" form))
    (should-not (non-func "^hint" form))
    (should-not (non-func "#macro" form))
    (should-not (non-func "^hint " form))
    (should-not (non-func "#macro " form))))

(ert-deftest clojure-syntax-prefixed-symbols ()
  (dolist (form '(("#?@aaa" . "aaa")
                  ("#?aaa"  . "?aaa")
                  ("#aaa"   . "aaa")
                  ("'aaa"   . "aaa")))
    (with-temp-buffer
      (clojure-mode)
      (insert (car form))
      (equal (symbol-name (symbol-at-point)) (cdr form)))))


(ert-deftest clojure-syntax-skip-prefixes ()
  (dolist (form '("#?@aaa" "#?aaa" "#aaa" "'aaa"))
    (with-temp-buffer
      (clojure-mode)
      (insert form)
      (backward-word)
      (backward-prefix-chars)
      (should (bobp)))))

(def-refactor-test test-paragraph-fill-within-comments
    "
;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt
;; ut labore et dolore magna aliqua."
    "
;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
;; tempor incididunt ut labore et dolore magna aliqua."
  (goto-char (point-min))
  (let ((fill-column 80))
    (fill-paragraph)))

(def-refactor-test test-paragraph-fill-within-inner-comments
    "
(let [a 1]
  ;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt
  ;; ut labore et dolore
  ;; magna aliqua.
  )"
    "
(let [a 1]
  ;; Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  ;; tempor incididunt ut labore et dolore magna aliqua.
  )"
  (goto-char (point-min))
  (forward-line 2)
  (let ((fill-column 80))
    (fill-paragraph)))

(when (fboundp 'font-lock-ensure)
  (def-refactor-test test-paragraph-fill-not-altering-surrounding-code
      "(def my-example-variable
  \"It has a very long docstring. So long, in fact, that it wraps onto multiple lines! This is to demonstrate what happens when the docstring wraps over three lines.\"
  nil)"
      "(def my-example-variable
  \"It has a very long docstring. So long, in fact, that it wraps onto multiple
  lines! This is to demonstrate what happens when the docstring wraps over three
  lines.\"
  nil)"
    (font-lock-ensure)
    (goto-char 40)
    (let ((clojure-docstring-fill-column 80)
          (fill-column 80))
      (fill-paragraph)))

  (ert-deftest test-clojure-in-docstring-p ()
    (with-temp-buffer
      (insert  "(def my-example-variable
  \"Doc here and `doc-here`\"
  nil)")
      (clojure-mode)
      (font-lock-ensure)
      (goto-char 32)
      (should (clojure-in-docstring-p))
      (goto-char 46)
      (should (clojure-in-docstring-p)))))

(provide 'clojure-mode-syntax-test)
