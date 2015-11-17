;;; clojure-mode-indentation-test.el --- Clojure Mode: indentation tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Bozhidar Batsov <bozhidar@batsov.com>

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
(require 's)

(ert-deftest dont-hang-on-eob ()
  (with-temp-buffer
    (insert "(let [a b]")
    (clojure-mode)
    (goto-char (point-max))
    (should
     (with-timeout (2)
       (newline-and-indent)
       t))))

(defmacro check-indentation (description before after &optional var-bindings)
  "Declare an ert test for indentation behaviour.
The test will check that the swift indentation command changes the buffer
from one state to another.  It will also test that point is moved to an
expected position.

DESCRIPTION is a symbol describing the test.

BEFORE is the buffer string before indenting, where a pipe (|) represents
point.

AFTER is the expected buffer string after indenting, where a pipe (|)
represents the expected position of point.

VAR-BINDINGS is an optional let-bindings list.  It can be used to set the
values of customisable variables."
  (declare (indent 1))
  (let ((fname (intern (format "indentation/%s" description))))
    `(ert-deftest ,fname ()
       (let* ((after ,after)
              (expected-cursor-pos (1+ (s-index-of "|" after)))
              (expected-state (delete ?| after))
              ,@var-bindings)
         (with-temp-buffer
           (insert ,before)
           (goto-char (point-min))
           (search-forward "|")
           (delete-char -1)
           (clojure-mode)
           (indent-according-to-mode)

           (should (equal expected-state (buffer-string)))
           (should (equal expected-cursor-pos (point))))))))

;; Provide font locking for easier test editing.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group "check-indentation") eow)
    (1 font-lock-keyword-face))
   (,(rx "("
         (group "check-indentation") (+ space)
         (group bow (+ (not space)) eow)
         )
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))


;;; Tests


(check-indentation no-indentation-at-top-level
  "|x"
  "|x")

(check-indentation cond-indentation
  "
(cond
|x)"
  "
(cond
  |x)")

(check-indentation threading-with-expression-on-first-line
  "
(->> expr
 |ala)"
  "
(->> expr
     |ala)")

(check-indentation threading-with-expression-on-second-line
  "
(->>
|expr)"
  "
(->>
 |expr)")

(check-indentation doc-strings-without-indent-specified
  "
(defn some-fn
|\"some doc string\")"
  "
(defn some-fn
  |\"some doc string\")")

(check-indentation doc-strings-with-correct-indent-specified
  "
(defn some-fn
  |\"some doc string\")"
  "
(defn some-fn
  |\"some doc string\")")

(check-indentation doc-strings-with-additional-indent-specified
  "
(defn some-fn
  |\"some doc string
    - some note\")"
  "
(defn some-fn
  |\"some doc string
    - some note\")")

;; we can specify different indentation for symbol with some ns prefix
(put-clojure-indent 'bala 0)
(put-clojure-indent 'ala/bala 1)

(check-indentation symbol-without-ns
  "
(bala
|one)"
  "
(bala
  |one)")

(check-indentation symbol-with-ns
  "
(ala/bala top
|one)"
  "
(ala/bala top
  |one)")

(check-indentation form-with-metadata
  "
(ns ^:doc app.core
|(:gen-class))"
"
(ns ^:doc app.core
  |(:gen-class))")

(check-indentation multiline-sexps
  "
[[
  2] a
|x]"
"
[[
  2] a
 |x]")

(check-indentation reader-conditionals
  "
#?(:clj :foo
|:cljs :bar)"
  "
#?(:clj :foo
   |:cljs :bar)")

(check-indentation backtracking-with-aliases
  "
(clojure.core/letfn [(twice [x]
|(* x 2))]
  :a)"
  "
(clojure.core/letfn [(twice [x]
                       |(* x 2))]
  :a)")

(check-indentation fixed-normal-indent
  "(cond
  (or 1
      2) 3
|:else 4)"
  "(cond
  (or 1
      2) 3
  |:else 4)")

(check-indentation fixed-normal-indent-2
  "(fact {:spec-type
       :charnock-column-id} #{\"charnock\"}
|{:spec-type
       :charnock-column-id} #{\"current_charnock\"})"
  "(fact {:spec-type
       :charnock-column-id} #{\"charnock\"}
      |{:spec-type
       :charnock-column-id} #{\"current_charnock\"})")


;;; Backtracking indent
(defmacro def-full-indent-test (name &rest forms)
  "Verify that all FORMs correspond to a properly indented sexps."
  (declare (indent 1))
  `(ert-deftest ,(intern (format "test-backtracking-%s" name)) ()
     (progn
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,(replace-regexp-in-string "\n +" "\n " form))
                      (indent-region (point-min) (point-max))
                      (should (equal (buffer-string)
                                     ,(concat "\n" form)))))
                 forms))))

(def-full-indent-test closing-paren
  "(ns ca
  (:gen-class)
  )")

(def-full-indent-test default-is-not-a-define
  "(default a
         b
         b)"
  "(some.namespace/default a
                        b
                        b)")

(def-full-indent-test extend-type-allow-multiarity
  "(extend-type Banana
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))"
  "(extend-protocol Banana
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")

(def-full-indent-test non-symbol-at-start
  "{\"1\" 2
 *3 4}")

(def-full-indent-test non-symbol-at-start-2
  "(\"1\" 2
 *3 4)")

(def-full-indent-test defrecord
  "(defrecord TheNameOfTheRecord
    [a pretty long argument list]
  SomeType
  (assoc [_ x]
    (.assoc pretty x 10)))")

(def-full-indent-test defrecord-2
  "(defrecord TheNameOfTheRecord [a pretty long argument list]
  SomeType (assoc [_ x]
             (.assoc pretty x 10)))")

(def-full-indent-test letfn
  "(letfn [(f [x]
          (* x 2))
        (f [x]
          (* x 2))]
  (a b
     c) (d)
  e)")

(def-full-indent-test reify
  "(reify Object
  (x [_]
    1))"
  "(reify
  om/IRender
  (render [this]
    (let [indent-test :fail]
      ...))
  om/IRender
  (render [this]
    (let [indent-test :fail]
      ...)))")

(def-full-indent-test reader-conditionals
  "#?@ (:clj []
     :cljs [])")

(def-full-indent-test empty-close-paren
  "(let [x]
  )"

  "(ns ok
  )"

  "(ns ^{:zen :dikar}
    ok
  )")

(def-full-indent-test unfinished-sexps
  "(letfn [(tw [x]
          dd")

(def-full-indent-test symbols-ending-in-crap
  "(msg? ExceptionInfo
      10)"
  "(thrown-with-msg? ExceptionInfo
                  #\"Storage must be initialized before use\"
                  (f))"
  "(msg' 1
      10)")


(defun indent-cond (indent-point state)
  (goto-char (elt state 1))
  (let ((pos -1)
        (base-col (current-column)))
    (forward-char 1)
    ;; `forward-sexp' will error if indent-point is after
    ;; the last sexp in the current sexp.
    (condition-case nil
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (clojure-forward-logical-sexp 1)
          (cl-incf pos))
      ;; If indent-point is _after_ the last sexp in the
      ;; current sexp, we detect that by catching the
      ;; `scan-error'. In that case, we should return the
      ;; indentation as if there were an extra sexp at point.
      (scan-error (cl-incf pos)))
    (+ base-col (if (cl-evenp pos) 0 2))))
(put-clojure-indent 'test-cond #'indent-cond)

(defun indent-cond-0 (_indent-point _state) 0)
(put-clojure-indent 'test-cond-0 #'indent-cond-0)

(def-full-indent-test function-spec
  "(when me
  (test-cond
    x
  1
    2
  3))"
  "(when me
  (test-cond-0
x
1
2
3))")

;;; Alignment
(defmacro def-full-align-test (name value &rest forms)
  "Verify that all FORMs correspond to a properly indented sexps."
  (declare (indent 2))
  `(ert-deftest ,(intern (format "test-align-%s-%s" name value)) ()
     (let ((clojure-align-forms ,value))
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,(replace-regexp-in-string " +" " " form))
                      (indent-region (point-min) (point-max))
                      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                                     ,(concat "\n" form)))))
                 forms))))

(def-full-align-test basic t
  "{:this-is-a-form b
 c               d}"
  "{:this-is b
 c        d}"
  "{:this b
 c     d}"
  "{:a b
 c  d}"

  "(let [this-is-a-form b
      c              d])"
  "(let [this-is b
      c       d])"
  "(let [this b
      c    d])"
  "(let [a b
      c d])")

(def-full-align-test basic 10
  "{:this-is-a-form b
 c d}"
  "{:this-is b
 c        d}"
  "{:this b
 c     d}"
  "{:a b
 c  d}"

  "(let [this-is-a-form b
      c d])"
  "(let [this-is b
      c       d])"
  "(let [this b
      c    d])"
  "(let [a b
      c d])")

(def-full-align-test basic nil
  "{:this-is-a-form b
 c d}"
  "{:this-is b
 c d}"
  "{:this b
 c d}"
  "{:a b
 c d}"

  "(let [this-is-a-form b
      c d])"
  "(let [this-is b
      c d])"
  "(let [this b
      c d])"
  "(let [a b
      c d])")


;;; Misc

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

(provide 'clojure-mode-indentation-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; clojure-mode-indentation-test.el ends here
