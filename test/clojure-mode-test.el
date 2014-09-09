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
(require 'cl-lib)
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
  (should (eq (clojure-test-face-at 2 11 "{:something 20}") 'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/type ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 9 "SomeClass") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/type-hint ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "#^SomeClass"
    (should (eq (clojure-test-face-at 3 11) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 1 2) nil))))

(ert-deftest clojure-mode-syntax-table/constructor ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 2 11 "(SomeClass.)") 'font-lock-type-face))
  (clojure-test-with-temp-buffer "(ns/SomeClass.)"
    (should (eq (clojure-test-face-at 2 3) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 5 14) 'font-lock-type-face))))

(ert-deftest clojure-mode-syntax-table/namespace ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 5 "one.p") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 1 11 "one.p.top13") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 2 12 "^one.p.top13") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/namespaced-symbol ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "clo.core/something"
    (should (eq (clojure-test-face-at 9 9) nil))
    (should (eq (clojure-test-face-at 1 8) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 10 18) nil))))

(ert-deftest clojure-mode-syntax-table/static-method ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "Class/methodName"
    (should (eq (clojure-test-face-at 6 6) nil))
    (should (eq (clojure-test-face-at 1 5) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 7 16) 'clojure-interop-method-face))))

(ert-deftest clojure-mode-syntax-table/interop-method ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 11 ".someMethod") 'clojure-interop-method-face))
  (should (eq (clojure-test-face-at 1 10 "someMethod") 'clojure-interop-method-face))
  (should (eq (clojure-test-face-at 1 11 "topHttpTest") 'clojure-interop-method-face))
  (should (eq (clojure-test-face-at 1 4 "getX") 'clojure-interop-method-face)))

(ert-deftest clojure-mode-syntax-table/constant ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 5 "CONST") 'font-lock-constant-face))
  (should (eq (clojure-test-face-at 1 10 "CONST_NAME") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/class-constant ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "Class/CONST_NAME"
    (should (eq (clojure-test-face-at 6 6) nil))
    (should (eq (clojure-test-face-at 1 5) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 7 16) 'font-lock-constant-face))))

(ert-deftest clojure-mode-syntax-table/namespaced-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(clo/defbar foo nil)"
    (should (eq (clojure-test-face-at 2 4) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 5 5) nil))
    (should (eq (clojure-test-face-at 6 11) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 13 15) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/variable-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(def foo 10)"
    (should (eq (clojure-test-face-at 2 4) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 6 8) 'font-lock-variable-name-face))))

(ert-deftest clojure-mode-syntax-table/type-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(deftype Foo)"
    (should (eq (clojure-test-face-at 2 8) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 10 12) 'font-lock-type-face))))

(ert-deftest clojure-mode-syntax-table/function-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(defn foo [x] x)"
    (should (eq (clojure-test-face-at 2 5) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 7 9) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/lambda-params ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "#(+ % %2 %3)"
    (should (eq (clojure-test-face-at 5 5) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 7 8) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 10 11) 'font-lock-variable-name-face))))

(ert-deftest clojure-mode-syntax-table/nil ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 6 "(= nil x)") 'font-lock-constant-face))
  (should-not (eq (clojure-test-face-at 3 5 "(fnil x)") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/true ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 7 "(= true x)") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/false ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 8 "(= false x)") 'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/keyword-meta ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "^:meta-data"
    (should (eq (clojure-test-face-at 1 1) nil))
    (should (eq (clojure-test-face-at 2 11) 'clojure-keyword-face))))

(ert-deftest clojure-mode-syntax-table/characters ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 2 "\\a") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 8 "\\newline") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\1") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 6 "\\u0032") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\+") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\.") 'clojure-character-face)))

(ert-deftest clojure-mode-syntax-table/cljx ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 5 "#+clj x") 'font-lock-preprocessor-face))
  (should (eq (clojure-test-face-at 1 6 "#+cljs x") 'font-lock-preprocessor-face)))

(ert-deftest clojure-mode-syntax-table/refer-ns ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 3 "foo/var") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 2 4 "@foo/var") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/dynamic-var ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 10 "*some-var*") 'font-lock-variable-name-face))
  (should (eq (clojure-test-face-at 2 11 "@*some-var*") 'font-lock-variable-name-face)))

(ert-deftest clojure-mode-syntax-table/ns-macro ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 5 8 "(ns name)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 13 "(ns name.name)") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/comments ()
  :tags '(fontification syntax-table)
  ;; comment delimiter
  (should (eq (clojure-test-face-at 1 1 "; comment") 'font-lock-comment-delimiter-face))
  (should (eq (clojure-test-face-at 1 2 ";; comment") 'font-lock-comment-delimiter-face))
  (should (eq (clojure-test-face-at 2 2 "(; comment\n)") 'font-lock-comment-delimiter-face))
  (should (eq (clojure-test-face-at 7 8 "(let [;; comment\n one 1] one)") 'font-lock-comment-delimiter-face))

  ;; comments
  (should (eq (clojure-test-face-at 3 9 "; comment") 'font-lock-comment-face))
  (should (eq (clojure-test-face-at 4 10 ";; comment") 'font-lock-comment-face))
  (should (eq (clojure-test-face-at 4 11 "(; comment\n)") 'font-lock-comment-face))
  (should (eq (clojure-test-face-at 10 17 "(let [;; comment\n one 1] one)") 'font-lock-comment-face)))

(provide 'clojure-mode-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; clojure-mode-test.el ends here
