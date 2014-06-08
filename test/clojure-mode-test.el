;;; clojure-mode-test.el --- Clojure Mode: Unit test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Bozhidar Batsov <bozhidar@batsov.com>

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


;;;; Utilities

(defmacro clojure-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENTS."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (clojure-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defun clojure-get-face-at-range (start end)
  (let ((start-face (get-text-property start 'face))
        (all-faces (cl-loop for i from start to end collect (get-text-property i 'face))))
    (if (cl-every (lambda (face) (eq face start-face)) all-faces)
        start-face
      'various-faces)))

(defun clojure-test-face-at (start end &optional content)
  "Get the face between START and END in CONTENT.

If CONTENT is not given, return the face at the specified range in the current
buffer."
  (if content
      (clojure-test-with-temp-buffer content
        (clojure-get-face-at-range start end))
    (clojure-get-face-at-range start end)))

(defconst clojure-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.

Each symbol in this vector corresponding to the syntax code of
its index.")

(defun clojure-test-syntax-at (pos)
  "Get the syntax at POS.

Get the syntax class symbol at POS, or nil if there is no syntax a
POS."
  (let ((code (syntax-class (syntax-after pos))))
    (aref clojure-test-syntax-classes code)))


;;;; Font locking

(ert-deftest clojure-mode-syntax-table/fontify-clojure-keyword ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 2 11 "{:something 20}") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/type ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 9 "SomeClass") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/namespaced-symbol ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "clo.core/something"
    (should (eq (clojure-test-face-at 9 9) nil))
    (should (eq (clojure-test-face-at 1 8) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 10 18) nil))))

(ert-deftest clojure-mode-syntax-table/lambda-params ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "#(+ % %2 %3)"
    (should (eq (clojure-test-face-at 5 5) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 7 8) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 10 11) 'font-lock-variable-name-face))))

(provide 'clojure-mode-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; clojure-mode-test.el ends here
