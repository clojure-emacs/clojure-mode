;;; clojure-mode-font-lock-test.el --- Clojure Mode: Font lock test suite
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Bozhidar Batsov <bozhidar@batsov.com>

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
        (all-faces (cl-loop for i from start to end collect (get-text-property
                                                             i 'face))))
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

(ert-deftest clojure-mode-syntax-table/stuff-in-backticks ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 1 2 "\"`#'s/trim`\"")
                 font-lock-string-face))
  (should (equal (clojure-test-face-at 3 10 "\"`#'s/trim`\"")
                 '(font-lock-constant-face font-lock-string-face)))
  (should (equal (clojure-test-face-at 11 12 "\"`#'s/trim`\"")
                 font-lock-string-face))
  (should (equal (clojure-test-face-at 1 1 ";`#'s/trim`")
                 font-lock-comment-delimiter-face))
  (should (equal (clojure-test-face-at 2 2 ";`#'s/trim`")
                 font-lock-comment-face))
  (should (equal (clojure-test-face-at 3 10 ";`#'s/trim`")
                 '(font-lock-constant-face font-lock-comment-face)))
  (should (equal (clojure-test-face-at 11 11 ";`#'s/trim`")
                 font-lock-comment-face)))

(ert-deftest clojure-mode-syntax-table/stuff-in-backticks ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 1 2 "\"a\\bc\\n\"")
                 font-lock-string-face))
  (should (equal (clojure-test-face-at 3 4 "\"a\\bc\\n\"")
                 '(bold font-lock-string-face)))
  (should (equal (clojure-test-face-at 5 5 "\"a\\bc\\n\"")
                 font-lock-string-face))
  (should (equal (clojure-test-face-at 6 7 "\"a\\bc\\n\"")
                 '(bold font-lock-string-face)))
  (should (equal (clojure-test-face-at 4 5 "#\"a\\bc\\n\"")
                 '(bold font-lock-string-face))))

(ert-deftest clojure-mode-syntax-table/fontify-let-when-while-type-forms ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 2 11 "(when-alist [x 1]\n  ())")
                 'font-lock-keyword-face))
  (should (equal (clojure-test-face-at 2 12 "(while-alist [x 1]\n  ())")
                 'font-lock-keyword-face))
  (should (equal (clojure-test-face-at 2 10 "(let-alist [x 1]\n  ())")
                 'font-lock-keyword-face)))

(ert-deftest clojure-mode-syntax-table/comment-macros ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at
                  1 2   "#_")
                  nil))
  (should (equal (clojure-test-face-at
                  1 2  "#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)")
                  nil))
  (should (equal (clojure-test-face-at
                  5 41 "#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)")
                 'font-lock-comment-face)))

(ert-deftest clojure-mode-syntax-table/namespace ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 5 12 "(ns .validns)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 12 "(ns =validns)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 21 "(ns .ValidNs=<>?+|?*.)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 28 "(ns ValidNs<>?+|?*.b*ar.ba*z)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 18 "(ns other.valid.ns)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 11 "(ns oneword)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 11 "(ns foo.bar)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 11 "(ns Foo.bar)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 11 "(ns Foo.Bar)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 11 "(ns foo.Bar)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 11 "(ns Foo-bar)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 11 "(ns Foo-Bar)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 11 "(ns foo-Bar)") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 9  "(ns one.X)") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/oneword ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 2 8   " oneword") nil))
  (should (eq (clojure-test-face-at 2 8   "@oneword") nil))
  (should (eq (clojure-test-face-at 2 8   "#oneword") nil))
  (should (eq (clojure-test-face-at 2 8   ".oneword") nil))
  (should (eq (clojure-test-face-at 3 9   "#^oneword")
              'font-lock-type-face)) ;; type-hint
  (should (eq (clojure-test-face-at 2 8   "(oneword)") nil))

  (should (eq (clojure-test-face-at 2 8   "(oneword/oneword)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(oneword/oneword)") nil))
  (should (eq (clojure-test-face-at 11 16 "(oneword/oneword)") nil))

  (should (eq (clojure-test-face-at 2 8   "(oneword/seg.mnt)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(oneword/seg.mnt)") nil))
  (should (eq (clojure-test-face-at 11 16 "(oneword/seg.mnt)") nil))

  (should (eq (clojure-test-face-at 2 8   "(oneword/mxdCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(oneword/mxdCase)") nil))
  (should (eq (clojure-test-face-at 11 16 "(oneword/mxdCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(oneword/CmlCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(oneword/CmlCase)") nil))
  (should (eq (clojure-test-face-at 11 16 "(oneword/CmlCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(oneword/ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(oneword/ve/yCom|pLex.stu-ff)")
              nil))
  (should (eq (clojure-test-face-at 11 28 "(oneword/ve/yCom|pLex.stu-ff)")
              nil))

  (should (eq (clojure-test-face-at 2 8   "(oneword/.ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(oneword/.ve/yCom|pLex.stu-ff)")
              nil))
  (should (eq (clojure-test-face-at 12 29 "(oneword/.ve/yCom|pLex.stu-ff)")
              nil)))

(ert-deftest clojure-mode-syntax-table/segment ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 2 8   " seg.mnt") nil))
  (should (eq (clojure-test-face-at 2 8   "@seg.mnt") nil))
  (should (eq (clojure-test-face-at 2 8   "#seg.mnt") nil))
  (should (eq (clojure-test-face-at 2 8   ".seg.mnt") nil))
  (should (eq (clojure-test-face-at 3 9   "#^seg.mnt")
              'font-lock-type-face)) ;; type-hint
  (should (eq (clojure-test-face-at 2 8   "(seg.mnt)") nil))
  (should (eq (clojure-test-face-at 2 8   "(seg.mnt/oneword)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(seg.mnt/oneword)") nil))
  (should (eq (clojure-test-face-at 11 16 "(seg.mnt/oneword)") nil))

  (should (eq (clojure-test-face-at 2 8   "(seg.mnt/seg.mnt)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(seg.mnt/seg.mnt)") nil))
  (should (eq (clojure-test-face-at 11 16 "(seg.mnt/seg.mnt)") nil))

  (should (eq (clojure-test-face-at 2 8   "(seg.mnt/mxdCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(seg.mnt/mxdCase)") nil))
  (should (eq (clojure-test-face-at 11 16 "(seg.mnt/mxdCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(seg.mnt/CmlCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(seg.mnt/CmlCase)") nil))
  (should (eq (clojure-test-face-at 11 16 "(seg.mnt/CmlCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(seg.mnt/ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(seg.mnt/ve/yCom|pLex.stu-ff)")
              nil))
  (should (eq (clojure-test-face-at 11 28 "(seg.mnt/ve/yCom|pLex.stu-ff)")
              nil))

  (should (eq (clojure-test-face-at 2 8   "(seg.mnt/.ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(seg.mnt/.ve/yCom|pLex.stu-ff)")
              nil))
  (should (eq (clojure-test-face-at 12 29 "(seg.mnt/.ve/yCom|pLex.stu-ff)")
              nil)))

(ert-deftest clojure-mode-syntax-table/camelcase ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 2 8   " CmlCase") nil))
  (should (eq (clojure-test-face-at 2 8   "@CmlCase") nil))
  (should (eq (clojure-test-face-at 2 8   "#CmlCase") nil))
  (should (eq (clojure-test-face-at 2 8   ".CmlCase") nil))
  (should (eq (clojure-test-face-at 3 9   "#^CmlCase")
              'font-lock-type-face)) ;; type-hint
  (should (eq (clojure-test-face-at 2 8   "(CmlCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(CmlCase/oneword)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(CmlCase/oneword)") nil))
  (should (eq (clojure-test-face-at 11 16 "(CmlCase/oneword)") nil))

  (should (eq (clojure-test-face-at 2 8   "(CmlCase/seg.mnt)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(CmlCase/seg.mnt)") nil))
  (should (eq (clojure-test-face-at 11 16 "(CmlCase/seg.mnt)") nil))

  (should (eq (clojure-test-face-at 2 8   "(CmlCase/mxdCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(CmlCase/mxdCase)") nil))
  (should (eq (clojure-test-face-at 11 16 "(CmlCase/mxdCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(CmlCase/CmlCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(CmlCase/CmlCase)") nil))
  (should (eq (clojure-test-face-at 11 16 "(CmlCase/CmlCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(CmlCase/ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(CmlCase/ve/yCom|pLex.stu-ff)")
              nil))
  (should (eq (clojure-test-face-at 11 28 "(CmlCase/ve/yCom|pLex.stu-ff)")
              nil))

  (should (eq (clojure-test-face-at 2 8   "(CmlCase/.ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(CmlCase/.ve/yCom|pLex.stu-ff)")
              nil))
  (should (eq (clojure-test-face-at 12 29 "(CmlCase/.ve/yCom|pLex.stu-ff)")
              nil)))

(ert-deftest clojure-mode-syntax-table/mixedcase ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 2 8   " mxdCase") nil))
  (should (eq (clojure-test-face-at 2 8   "@mxdCase") nil))
  (should (eq (clojure-test-face-at 2 8   "#mxdCase") nil))
  (should (eq (clojure-test-face-at 2 8   ".mxdCase") nil))
  (should (eq (clojure-test-face-at 3 9   "#^mxdCase")
              'font-lock-type-face)) ;; type-hint
  (should (eq (clojure-test-face-at 2 8   "(mxdCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(mxdCase/oneword)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(mxdCase/oneword)") nil))
  (should (eq (clojure-test-face-at 11 16 "(mxdCase/oneword)") nil))

  (should (eq (clojure-test-face-at 2 8   "(mxdCase/seg.mnt)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(mxdCase/seg.mnt)") nil))
  (should (eq (clojure-test-face-at 11 16 "(mxdCase/seg.mnt)") nil))

  (should (eq (clojure-test-face-at 2 8   "(mxdCase/mxdCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(mxdCase/mxdCase)") nil))
  (should (eq (clojure-test-face-at 11 16 "(mxdCase/mxdCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(mxdCase/CmlCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(mxdCase/CmlCase)") nil))
  (should (eq (clojure-test-face-at 11 16 "(mxdCase/CmlCase)") nil))

  (should (eq (clojure-test-face-at 2 8   "(mxdCase/ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(mxdCase/ve/yCom|pLex.stu-ff)")
              nil))
  (should (eq (clojure-test-face-at 11 28 "(mxdCase/ve/yCom|pLex.stu-ff)")
              nil))

  (should (eq (clojure-test-face-at 2 8   "(mxdCase/.ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 10  "(mxdCase/.ve/yCom|pLex.stu-ff)")
              nil))
  (should (eq (clojure-test-face-at 12 29 "(mxdCase/.ve/yCom|pLex.stu-ff)")
              nil)))

(ert-deftest clojure-mode-syntax-table/verycomplex ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 3 4     "  ve/yCom|pLex.stu-ff")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 21    "  ve/yCom|pLex.stu-ff") nil))

  (should (eq (clojure-test-face-at 2 2     " @ve/yCom|pLex.stu-ff") nil))
  (should (eq (clojure-test-face-at 3 4     " @ve/yCom|pLex.stu-ff")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 21    " @ve/yCom|pLex.stu-ff") nil))

  (should (eq (clojure-test-face-at 2 4     " #ve/yCom|pLex.stu-ff")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 21    " #ve/yCom|pLex.stu-ff") nil))

  (should (eq (clojure-test-face-at 2 4     " .ve/yCom|pLex.stu-ff")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 21    " .ve/yCom|pLex.stu-ff") nil))

  ;; type-hint
  (should (eq (clojure-test-face-at 1 2     "#^ve/yCom|pLex.stu-ff") 'default))
  (should (eq (clojure-test-face-at 3 4     "#^ve/yCom|pLex.stu-ff")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 21    "#^ve/yCom|pLex.stu-ff") 'default))

  (should (eq (clojure-test-face-at 3 4     " (ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 21    " (ve/yCom|pLex.stu-ff)") nil))

  (should (eq (clojure-test-face-at 3 4     " (ve/yCom|pLex.stu-ff/oneword)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 29    " (ve/yCom|pLex.stu-ff/oneword)")
              nil))

  (should (eq (clojure-test-face-at 3 4     " (ve/yCom|pLex.stu-ff/seg.mnt)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 29    " (ve/yCom|pLex.stu-ff/seg.mnt)")
              nil))

  (should (eq (clojure-test-face-at 3 4     " (ve/yCom|pLex.stu-ff/mxdCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 29    " (ve/yCom|pLex.stu-ff/mxdCase)")
              nil))

  (should (eq (clojure-test-face-at 3 4     " (ve/yCom|pLex.stu-ff/CmlCase)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 29    " (ve/yCom|pLex.stu-ff/CmlCase)")
              nil))

  (should (eq (clojure-test-face-at
               3 4  " (ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at
               5 41 " (ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff)")
              nil))

  (should (eq (clojure-test-face-at
               3 4 " (ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff)")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at
               5 42 " (ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff)")
              nil)))

(ert-deftest clojure-mode-syntax-table/kw-oneword ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 3 9   " :oneword") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 9   "{:oneword 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 10  "{:#oneword 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 10  "{:.oneword 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:oneword/oneword 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:oneword/oneword 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:oneword/oneword 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3  9  "{:oneword/seg.mnt 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:oneword/seg.mnt 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:oneword/seg.mnt 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:oneword/CmlCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:oneword/CmlCase 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:oneword/CmlCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:oneword/mxdCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:oneword/mxdCase 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:oneword/mxdCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:oneword/ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:oneword/ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 11 29 "{:oneword/ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:oneword/.ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:oneword/.ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 11 30 "{:oneword/.ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/kw-namespaced ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 5 "::foo") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 1 9 ":_::_:foo") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 1 8 ":_:_:foo") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 1 9 ":foo/:bar") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 1 7 "::_:foo") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 1 9 "::_:_:foo") 'clojure-keyword-face))

  (should (eq (clojure-test-face-at 1 1   ":_:_:foo/_") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 2 8   ":_:_:foo/_") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 9 9   ":_:_:foo/_") 'default))
  (should (eq (clojure-test-face-at 10 10 ":_:_:foo/_") 'clojure-keyword-face))

  (should (eq (clojure-test-face-at 10 12 ":_:_:foo/bar")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 10 16 ":_:_:foo/bar/eee")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 10 17 ":_:_:foo/bar_:foo")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 10 19 ":_:_:foo/bar_:_:foo")
              'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/kw-segment ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 3 9   " :seg.mnt") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 9   "{:seg.mnt 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 10  "{:#seg.mnt 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 10  "{:.seg.mnt 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:seg.mnt/oneword 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:seg.mnt/oneword 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:seg.mnt/oneword 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:seg.mnt/seg.mnt 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:seg.mnt/seg.mnt 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:seg.mnt/seg.mnt 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:seg.mnt/CmlCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:seg.mnt/CmlCase 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:seg.mnt/CmlCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:seg.mnt/mxdCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:seg.mnt/mxdCase 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:seg.mnt/mxdCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:seg.mnt/ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:seg.mnt/ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 11 29 "{:seg.mnt/ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:seg.mnt/.ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:seg.mnt/.ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 11 30 "{:seg.mnt/.ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/kw-camelcase ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 3 9   " :CmlCase") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 9   "{:CmlCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 10  "{:#CmlCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 10  "{:.CmlCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 9   "{:CmlCase/oneword 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:CmlCase/oneword 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:CmlCase/oneword 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3  9  "{:CmlCase/seg.mnt 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:CmlCase/seg.mnt 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:CmlCase/seg.mnt 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 9   "{:CmlCase/CmlCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:CmlCase/CmlCase 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:CmlCase/CmlCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 9   "{:CmlCase/mxdCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:CmlCase/mxdCase 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:CmlCase/mxdCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 9   "{:CmlCase/ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:CmlCase/ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 11 29 "{:CmlCase/ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:CmlCase/.ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:CmlCase/.ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 11 30 "{:CmlCase/.ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/kw-mixedcase ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 3 9   " :mxdCase") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 9   "{:mxdCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 10  "{:#mxdCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 10  "{:.mxdCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:mxdCase/oneword 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:mxdCase/oneword 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:mxdCase/oneword 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3  9  "{:mxdCase/seg.mnt 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:mxdCase/seg.mnt 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:mxdCase/seg.mnt 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:mxdCase/CmlCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:mxdCase/CmlCase 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:mxdCase/CmlCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:mxdCase/mxdCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:mxdCase/mxdCase 0}") 'default))
  (should (eq (clojure-test-face-at 11 17 "{:mxdCase/mxdCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:mxdCase/ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:mxdCase/ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 11 29 "{:mxdCase/ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 3 9   "{:mxdCase/.ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 10 10 "{:mxdCase/.ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 11 30 "{:mxdCase/.ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/kw-verycomplex ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 3 4    " :ve/yCom|pLex.stu-ff")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 5    " :ve/yCom|pLex.stu-ff")
              'default))
  (should (eq (clojure-test-face-at 6 21   " :ve/yCom|pLex.stu-ff")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 2 2   "{:ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 4   "{:ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 5   "{:ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 6 21  "{:ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 2 2   "{:#ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 5   "{:#ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 6 6   "{:#ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 7 22  "{:#ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 2 2   "{:.ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 5   "{:.ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 6 6   "{:.ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at 7 22  "{:.ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 2 2   "{:ve/yCom|pLex.stu-ff/oneword 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 4   "{:ve/yCom|pLex.stu-ff/oneword 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 5   "{:ve/yCom|pLex.stu-ff/oneword 0}")
              'default))
  (should (eq (clojure-test-face-at 6 29  "{:ve/yCom|pLex.stu-ff/oneword 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 2 2   "{:ve/yCom|pLex.stu-ff/seg.mnt 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 4   "{:ve/yCom|pLex.stu-ff/seg.mnt 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 5   "{:ve/yCom|pLex.stu-ff/seg.mnt 0}")
              'default))
  (should (eq (clojure-test-face-at 6 29  "{:ve/yCom|pLex.stu-ff/seg.mnt 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 2 2   "{:ve/yCom|pLex.stu-ff/ClmCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 4   "{:ve/yCom|pLex.stu-ff/ClmCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 5   "{:ve/yCom|pLex.stu-ff/ClmCase 0}")
              'default))
  (should (eq (clojure-test-face-at 6 29  "{:ve/yCom|pLex.stu-ff/ClmCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at 2 2   "{:ve/yCom|pLex.stu-ff/mxdCase 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at 3 4   "{:ve/yCom|pLex.stu-ff/mxdCase 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at 5 5   "{:ve/yCom|pLex.stu-ff/mxdCase 0}")
              'default))
  (should (eq (clojure-test-face-at 6 29  "{:ve/yCom|pLex.stu-ff/mxdCase 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at
               2 2  "{:ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at
               3 4  "{:ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at
               5 5  "{:ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at
               6 41 "{:ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))

  (should (eq (clojure-test-face-at
               2 2   "{:ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face))
  (should (eq (clojure-test-face-at
               3 4   "{:ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff 0}")
              'font-lock-type-face))
  (should (eq (clojure-test-face-at
               5 5   "{:ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff 0}")
              'default))
  (should (eq (clojure-test-face-at
               6 42  "{:ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff 0}")
              'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/namespaced-def ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(_c4/defconstrainedfn bar [] nil)"
    (should (eq (clojure-test-face-at 2 4) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 5 5) nil))
    (should (eq (clojure-test-face-at 6 18) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 23 25) 'font-lock-function-name-face)))
  (clojure-test-with-temp-buffer "(clo/defbar foo nil)"
    (should (eq (clojure-test-face-at 2 4) 'font-lock-type-face))
    (should (eq (clojure-test-face-at 5 5) nil))
    (should (eq (clojure-test-face-at 6 11) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 13 15) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/variable-def ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 2 4 "(def foo 10)")
              'font-lock-keyword-face))
  (should (eq (clojure-test-face-at 6 8 "(def foo 10)")
              'font-lock-variable-name-face)))

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

(ert-deftest clojure-mode-syntax-table/custom-def-with-special-chars1 ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(defn* foo [x] x)"
    (should (eq (clojure-test-face-at 2 6) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 8 10) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/custom-def-with-special-chars2 ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(defsomething! foo [x] x)"
    (should (eq (clojure-test-face-at 2 14) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 16 18) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/custom-def-with-special-chars3 ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "(def-something foo [x] x)"
    (should (eq (clojure-test-face-at 2 14) 'font-lock-keyword-face))
    (should (eq (clojure-test-face-at 16 18) 'font-lock-function-name-face))))

(ert-deftest clojure-mode-syntax-table/fn ()
  :tags '(fontification syntax-table)
  ;; try to byte-recompile the clojure-mode.el when the face of 'fn' is 't'
  (should (eq (clojure-test-face-at 2 3 "(fn foo [x] x)")
              'font-lock-keyword-face))
  (should (eq (clojure-test-face-at 5 7 "(fn foo [x] x)")
              'font-lock-function-name-face)))

(ert-deftest clojure-mode-syntax-table/lambda-params ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "#(+ % %2 %3 %&)"
    (should (eq (clojure-test-face-at 5 5) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 7 8) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 10 11) 'font-lock-variable-name-face))
    (should (eq (clojure-test-face-at 13 14) 'font-lock-variable-name-face))))

(ert-deftest clojure-mode-syntax-table/nil ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 6 "(= nil x)") 'font-lock-constant-face))
  (should-not (eq (clojure-test-face-at 3 5 "(fnil x)")
                  'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/true ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 7 "(= true x)")
              'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/false ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 4 8 "(= false x)")
              'font-lock-constant-face)))

(ert-deftest clojure-mode-syntax-table/keyword-meta ()
  :tags '(fontification syntax-table)
  (clojure-test-with-temp-buffer "^:meta-data"
    (should (eq (clojure-test-face-at 1 1) nil))
    (should (equal (clojure-test-face-at 2 11) 'clojure-keyword-face))))

(ert-deftest clojure-mode-syntax-table/keyword-allowed-chars ()
  :tags '(fontification syntax-table)
  (should (equal (clojure-test-face-at 1 8 ":aaa#bbb") 'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/keyword-disallowed-chars ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 5 ":aaa@bbb") 'various-faces))
  (should (equal (clojure-test-face-at 1 4 ":aaa@bbb") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 1 5 ":aaa~bbb") 'various-faces))
  (should (equal (clojure-test-face-at 1 4 ":aaa~bbb") 'clojure-keyword-face))
  (should (eq (clojure-test-face-at 1 5 ":aaa@bbb") 'various-faces))
  (should (equal (clojure-test-face-at 1 4 ":aaa@bbb") 'clojure-keyword-face)))

(ert-deftest clojure-mode-syntax-table/characters ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 2 "\\a") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 8 "\\newline") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\1") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 6 "\\u0032") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\+") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\.") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\,") 'clojure-character-face))
  (should (eq (clojure-test-face-at 1 2 "\\;") 'clojure-character-face)))

(ert-deftest clojure-mode-syntax-table/refer-ns ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 3 "foo/var") 'font-lock-type-face))
  (should (eq (clojure-test-face-at 2 4 "@foo/var") 'font-lock-type-face)))

(ert-deftest clojure-mode-syntax-table/dynamic-var ()
  :tags '(fontification syntax-table)
  (should (eq (clojure-test-face-at 1 10 "*some-var*")
              'font-lock-variable-name-face))
  (should (eq (clojure-test-face-at 2 11 "@*some-var*")
              'font-lock-variable-name-face))
  (should (eq (clojure-test-face-at 9 13 "some.ns/*var*")
              'font-lock-variable-name-face)))

(provide 'clojure-mode-font-lock-test)

;;; clojure-mode-font-lock-test.el ends here
