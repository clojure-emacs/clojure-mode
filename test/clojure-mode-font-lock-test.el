;;; clojure-mode-font-lock-test.el --- Clojure Mode: Font lock test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Bozhidar Batsov <bozhidar@batsov.dev>

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

;; The unit test suite of Clojure Mode

;;; Code:

(require 'clojure-mode)
(require 'cl-lib)
(require 'buttercup)
(require 'test-helper "test/utils/test-helper")


;;;; Utilities

(defmacro with-fontified-clojure-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-clojure-buffer ,content
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun clojure-test--uniform-face (start end)
  "Return the face from START to END if uniform, else `various-faces'.
Assumes the current buffer is already fontified."
  (let ((start-face (get-text-property start 'face))
        (all-faces (cl-loop for i from start to end
                            collect (get-text-property i 'face))))
    (if (cl-every (lambda (face) (equal face start-face)) all-faces)
        start-face
      'various-faces)))

(defun clojure-get-face-at (start end content)
  "Get the face between START and END in CONTENT."
  (with-fontified-clojure-buffer content
    (clojure-test--uniform-face start end)))

(defun expect-face-at (content start end face)
  "Expect face in CONTENT between START and END to be equal to FACE."
  (expect (clojure-get-face-at start end content) :to-equal face))

(defun expect-face-of (content substring face &optional nth)
  "Expect FACE on the NTH occurrence of SUBSTRING in fontified CONTENT.
NTH defaults to 1."
  (with-fontified-clojure-buffer content
    (goto-char (point-min))
    (dotimes (_ (or nth 1))
      (search-forward substring))
    (let* ((end (1- (point)))
           (start (- (point) (length substring))))
      (expect (clojure-test--uniform-face start end) :to-equal face))))

(defun clojure-test--check-faces (content face-specs)
  "Fontify CONTENT and check all FACE-SPECS.
Each spec is either (START END FACE) for positional checks or
\(SUBSTRING FACE) for substring-based checks.

Substring specs are matched sequentially through the buffer so
that repeated substrings resolve naturally in document order
without any special annotation."
  (with-fontified-clojure-buffer content
    (dolist (spec face-specs)
      (pcase spec
        (`(,(and (pred stringp) substr) ,face)
         (let ((found (search-forward substr nil t)))
           (expect found :not :to-be nil)
           (when found
             (let* ((end (1- (point)))
                    (start (- (point) (length substr))))
               (expect (clojure-test--uniform-face start end)
                       :to-equal face)))))
        (`(,(and (pred numberp) start) ,end ,face)
         (expect (clojure-test--uniform-face start end) :to-equal face))))))

(defconst clojure-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.

Each symbol in this vector corresponding to the syntax code of
its index.")

(defmacro when-fontifying-it (description &rest tests)
  "Return a buttercup spec.

TESTS are lists of the form (content face-spec*) where each face-spec is either
\(start end expected-face) for positional checks or (substring expected-face)
for substring-based checks.  Substring specs are matched sequentially
through the buffer so repeated substrings resolve in document order.

DESCRIPTION is the description of the spec."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (clojure-test--check-faces (car test) (cdr test)))))

;;;; Font locking

(describe "clojure-mode-syntax-table"

  (when-fontifying-it "should handle stuff in backticks"
    ("\"`#'s/trim`\""
     (1 2 font-lock-string-face)
     (3 10 (font-lock-constant-face font-lock-string-face))
     (11 12 font-lock-string-face))

    (";`#'s/trim`"
     (1 1 font-lock-comment-delimiter-face)
     (2 2 font-lock-comment-face)
     (3 10 (font-lock-constant-face font-lock-comment-face))
     (11 11 font-lock-comment-face)))

  (when-fontifying-it "should handle stuff in strings"
    ("\"a\\bc\\n\""
     (1 2 font-lock-string-face)
     (3 4 (bold font-lock-string-face))
     (5 5 font-lock-string-face)
     (6 7 (bold font-lock-string-face)))

    ("#\"a\\bc\\n\""
     (4 5 (bold font-lock-string-face))))

  (when-fontifying-it "should handle stuff in double brackets"
    ("\"[[#'s/trim]]\""
     (1 3 font-lock-string-face)
     (4 11 (font-lock-constant-face font-lock-string-face))
     (12 14 font-lock-string-face))

    (";[[#'s/trim]]"
     (1 1 font-lock-comment-delimiter-face)
     (2 3 font-lock-comment-face)
     (4 11 (font-lock-constant-face font-lock-comment-face))
     (12 13 font-lock-comment-face)))

  (when-fontifying-it "should fontify let, when, and while type forms"
    ("(when-alist [x 1]\n  ())"
     ("when-alist" font-lock-keyword-face))

    ("(while-alist [x 1]\n  ())"
     ("while-alist" font-lock-keyword-face))

    ("(let-alist [x 1]\n  ())"
     ("let-alist" font-lock-keyword-face)))

  (when-fontifying-it "should handle comment macros"
    ("#_"
     (1 2 nil))

    ("#_#_"
     (1 2 nil))

    ("#_#_"
     (3 2 clojure-discard-face))

    ("#_ #_"
     (1 3 nil))

    ("#_ #_"
     (4 2 clojure-discard-face))

    ("#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)"
     (1 2 nil))

    ("#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)"
     (5 41 clojure-discard-face))

    ("#_#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)\n;; more crap\n (foobar tnseriao)"
     (1 4 nil))

    ("#_ #_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)\n;; more crap\n (foobar tnseriao)"
     (1 5 nil))

    ("#_#_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)\n;; more crap\n (foobar tnseriao)"
     (7 75 clojure-discard-face))

    ("#_ #_ \n;; some crap\n (lala 0101\n lao\n\n 0 0i)\n;; more crap\n (foobar tnseriao)"
     (8 75 clojure-discard-face)))

  (when-fontifying-it "should handle namespace declarations"
    ("(ns .validns)"
     (".validns" font-lock-type-face))

    ("(ns =validns)"
     ("=validns" font-lock-type-face))

    ("(ns .ValidNs=<>?+|?*.)"
     (".ValidNs=<>?+|?*." font-lock-type-face))

    ("(ns ValidNs<>?+|?*.b*ar.ba*z)"
     ("ValidNs<>?+|?*.b*ar.ba*z" font-lock-type-face))

    ("(ns other.valid.ns)"
     ("other.valid.ns" font-lock-type-face))

    ("(ns oneword)"
     ("oneword" font-lock-type-face))

    ("(ns foo.bar)"
     ("foo.bar" font-lock-type-face))

    ("(ns Foo.bar)"
     ("Foo.bar" font-lock-type-face))

    ("(ns Foo-bar)"
     ("Foo-bar" font-lock-type-face))

    ("(ns foo-Bar)"
     ("foo-Bar" font-lock-type-face))

    ("(ns one.X)"
     ("one.X" font-lock-type-face))

    ("(ns ^:md ns-name)"
     ("ns-name" font-lock-type-face))

    ("(ns ^:md \n  ns-name)"
     ("ns-name" font-lock-type-face))

    ("(ns ^:md1 ^:md2 ns-name)"
     ("ns-name" font-lock-type-face))

    ("(ns ^:md1 ^{:md2 true} ns-name)"
     ("ns-name" font-lock-type-face))

    ("(ns ^{:md2 true} ^:md1 ns-name)"
     ("ns-name" font-lock-type-face))

    ("(ns ^:md1 ^{:md2 true} \n  ns-name)"
     ("ns-name" font-lock-type-face))

    ("(ns ^{:md2 true} ^:md1 \n  ns-name)"
     ("ns-name" font-lock-type-face)))

  (when-fontifying-it "should handle one word"
    (" oneword"
     ("oneword" nil))

    ("@oneword"
     ("oneword" nil))

    ("#oneword"
     ("oneword" nil))

    (".oneword"
     ("oneword" nil))

    ("#^oneword"
     ("oneword" font-lock-type-face)) ;; type-hint

    ("(oneword)"
     ("oneword" nil))

    ("(oneword/oneword)"
     ("oneword" font-lock-type-face)
     ("/" nil)
     ("oneword" nil))

    ("(oneword/seg.mnt)"
     ("oneword" font-lock-type-face)
     ("/" nil)
     ("seg.mnt" nil))

    ("(oneword/mxdCase)"
     ("oneword" font-lock-type-face)
     ("/" nil)
     ("mxdCase" nil))

    ("(oneword/CmlCase)"
     ("oneword" font-lock-type-face)
     ("/" nil)
     ("CmlCase" nil))

    ("(colons:are:okay)"
     ("colons:are:okay" nil))

    ("(some-ns/colons:are:okay)"
     ("some-ns" font-lock-type-face)
     ("/colons:are:okay" nil))

    ("(oneword/ve/yCom|pLex.stu-ff)"
     ("oneword" font-lock-type-face)
     ("/" nil)
     ("ve/yCom|pLex.stu-ff" nil))

    ("(oneword/.ve/yCom|pLex.stu-ff)"
     ("oneword" font-lock-type-face)
     ("/." nil)
     ("ve/yCom|pLex.stu-ff" nil)))

  (when-fontifying-it "should handle a segment"
    (" seg.mnt"
     ("seg.mnt" nil))

    ("@seg.mnt"
     ("seg.mnt" nil))

    ("#seg.mnt"
     ("seg.mnt" nil))

    (".seg.mnt"
     ("seg.mnt" nil))

    ("#^seg.mnt"
     ("seg.mnt" font-lock-type-face)) ;; type-hint

    ("(seg.mnt)"
     ("seg.mnt" nil))

    ("(seg.mnt/oneword)"
     ("seg.mnt" font-lock-type-face)
     ("/" nil)
     ("oneword" nil))

    ("(seg.mnt/seg.mnt)"
     ("seg.mnt" font-lock-type-face)
     ("/" nil)
     ("seg.mnt" nil))

    ("(seg.mnt/mxdCase)"
     ("seg.mnt" font-lock-type-face)
     ("/" nil)
     ("mxdCase" nil))

    ("(seg.mnt/CmlCase)"
     ("seg.mnt" font-lock-type-face)
     ("/" nil)
     ("CmlCase" nil))

    ("(seg.mnt/ve/yCom|pLex.stu-ff)"
     ("seg.mnt" font-lock-type-face)
     ("/" nil)
     ("ve/yCom|pLex.stu-ff" nil))

    ("(seg.mnt/.ve/yCom|pLex.stu-ff)"
     ("seg.mnt" font-lock-type-face)
     ("/." nil)
     ("ve/yCom|pLex.stu-ff" nil)))

  (when-fontifying-it "should handle camelcase"
    (" CmlCase"
     ("CmlCase" nil))

    ("@CmlCase"
     ("CmlCase" nil))

    ("#CmlCase"
     ("CmlCase" nil))

    (".CmlCase"
     ("CmlCase" nil))

    ("#^CmlCase"
     ("CmlCase" font-lock-type-face)) ;; type-hint

    ("(CmlCase)"
     ("CmlCase" nil))

    ("(CmlCase/oneword)"
     ("CmlCase" font-lock-type-face)
     ("/" nil)
     ("oneword" nil))

    ("(CmlCase/seg.mnt)"
     ("CmlCase" font-lock-type-face)
     ("/" nil)
     ("seg.mnt" nil))

    ("(CmlCase/mxdCase)"
     ("CmlCase" font-lock-type-face)
     ("/" nil)
     ("mxdCase" nil))

    ("(CmlCase/CmlCase)"
     ("CmlCase" font-lock-type-face)
     ("/" nil)
     ("CmlCase" nil))

    ("(CmlCase/ve/yCom|pLex.stu-ff)"
     ("CmlCase" font-lock-type-face)
     ("/" nil)
     ("ve/yCom|pLex.stu-ff" nil))

    ("(CmlCase/.ve/yCom|pLex.stu-ff)"
     ("CmlCase" font-lock-type-face)
     ("/." nil)
     ("ve/yCom|pLex.stu-ff" nil)))

  (when-fontifying-it "should handle mixed case"
    (" mxdCase"
     ("mxdCase" nil))

    ("@mxdCase"
     ("mxdCase" nil))

    ("#mxdCase"
     ("mxdCase" nil))

    (".mxdCase"
     ("mxdCase" nil))

    ("#^mxdCase"
     ("mxdCase" font-lock-type-face)) ;; type-hint

    ("(mxdCase)"
     ("mxdCase" nil))

    ("(mxdCase/oneword)"
     ("mxdCase" font-lock-type-face)
     ("/" nil)
     ("oneword" nil))

    ("(mxdCase/seg.mnt)"
     ("mxdCase" font-lock-type-face)
     ("/" nil)
     ("seg.mnt" nil))

    ("(mxdCase/mxdCase)"
     ("mxdCase" font-lock-type-face)
     ("/" nil)
     ("mxdCase" nil))

    ("(mxdCase/CmlCase)"
     ("mxdCase" font-lock-type-face)
     ("/" nil)
     ("CmlCase" nil))

    ("(mxdCase/ve/yCom|pLex.stu-ff)"
     ("mxdCase" font-lock-type-face)
     ("/" nil)
     ("ve/yCom|pLex.stu-ff" nil))

    ("(mxdCase/.ve/yCom|pLex.stu-ff)"
     ("mxdCase" font-lock-type-face)
     ("/." nil)
     ("ve/yCom|pLex.stu-ff" nil)))

  (when-fontifying-it "should handle quotes in tail of symbols and keywords"
    ("'quot'ed'/sy'm'bol''"
     ("quot'ed'" font-lock-type-face)
     ("/sy'm'bol''" nil))

    (":qu'ote'd''/key'word'"
     ("qu'ote'd''" font-lock-type-face)
     ("/" default)
     ("key'word'" clojure-keyword-face)))

  (when-fontifying-it "should handle very complex stuff"
    ("  ve/yCom|pLex.stu-ff"
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff" nil))

    (" @ve/yCom|pLex.stu-ff"
     ("@" nil)
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff" nil))

    (" #ve/yCom|pLex.stu-ff"
     ("#ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff" nil))

    (" .ve/yCom|pLex.stu-ff"
     (".ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff" nil))

    ;; type-hint
    ("#^ve/yCom|pLex.stu-ff"
     ("#^" default)
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff" default))

    ("^ve/yCom|pLex.stu-ff"
     ("ve" font-lock-type-face)
     ("yCom|pLex.stu-ff" default))

    (" (ve/yCom|pLex.stu-ff)"
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff" nil))

    (" (ve/yCom|pLex.stu-ff/oneword)"
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff/oneword" nil))

    (" (ve/yCom|pLex.stu-ff/seg.mnt)"
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff/seg.mnt" nil))

    (" (ve/yCom|pLex.stu-ff/mxdCase)"
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff/mxdCase" nil))

    (" (ve/yCom|pLex.stu-ff/CmlCase)"
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff/CmlCase" nil))

    (" (ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff)"
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff" nil))

    (" (ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff)"
     ("ve" font-lock-type-face)
     ("/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff" nil)))

  (when-fontifying-it "should handle oneword keywords"
    (" :oneword"
     ("oneword" clojure-keyword-face))

    (" :1oneword"
     ("1oneword" clojure-keyword-face))

    ("{:oneword 0}"
     ("oneword" clojure-keyword-face))

    ("{:1oneword 0}"
     ("1oneword" clojure-keyword-face))

    ("{:#oneword 0}"
     ("#oneword" clojure-keyword-face))

    ("{:.oneword 0}"
     (".oneword" clojure-keyword-face))

    ("{:oneword/oneword 0}"
     ("oneword" font-lock-type-face)
     ("/" default)
     ("oneword" clojure-keyword-face))

    ("{:oneword/seg.mnt 0}"
     ("oneword" font-lock-type-face)
     ("/" default)
     ("seg.mnt" clojure-keyword-face))

    ("{:oneword/CmlCase 0}"
     ("oneword" font-lock-type-face)
     ("/" default)
     ("CmlCase" clojure-keyword-face))

    ("{:oneword/mxdCase 0}"
     ("oneword" font-lock-type-face)
     ("/" default)
     ("mxdCase" clojure-keyword-face))

    ("{:oneword/ve/yCom|pLex.stu-ff 0}"
     ("oneword" font-lock-type-face)
     ("/" default)
     ("ve/yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:oneword/.ve/yCom|pLex.stu-ff 0}"
     ("oneword" font-lock-type-face)
     ("/" default)
     (".ve/yCom|pLex.stu-ff" clojure-keyword-face)))

  (when-fontifying-it "should handle namespaced keywords"
    ("::foo"
     (1 5 clojure-keyword-face))

    (":_::_:foo"
     (1 9 clojure-keyword-face))

    (":_:_:foo"
     (1 8 clojure-keyword-face))

    (":foo/:bar"
     (1 9 clojure-keyword-face))

    ("::_:foo"
     (1 7 clojure-keyword-face))

    ("::_:_:foo"
     (1 9 clojure-keyword-face))

    (":_:_:foo/_"
     (1 1 clojure-keyword-face)
     (2 8 font-lock-type-face)
     (9 9 default)
     (10 10 clojure-keyword-face))

    (":_:_:foo/bar"
     (10 12 clojure-keyword-face))

    (":_:_:foo/bar/eee"
     (10 16 clojure-keyword-face))

    (":_:_:foo/bar_:foo"
     (10 17 clojure-keyword-face))

    (":_:_:foo/bar_:_:foo"
     (10 19 clojure-keyword-face))

    (":1foo/bar"
     (2 5 font-lock-type-face)
     (6 6 default)
     (7 9 clojure-keyword-face))

    (":foo/1bar"
     (2 4 font-lock-type-face)
     (5 5 default)
     (6 9 clojure-keyword-face))

    (":1foo/1bar"
     (2 5 font-lock-type-face)
     (6 6 default)
     (7 10 clojure-keyword-face)))

  (when-fontifying-it "should handle segment keywords"
    (" :seg.mnt"
     ("seg.mnt" clojure-keyword-face))

    ("{:seg.mnt 0}"
     ("seg.mnt" clojure-keyword-face))

    ("{:#seg.mnt 0}"
     ("#seg.mnt" clojure-keyword-face))

    ("{:.seg.mnt 0}"
     (".seg.mnt" clojure-keyword-face))

    ("{:seg.mnt/oneword 0}"
     ("seg.mnt" font-lock-type-face)
     ("/" default)
     ("oneword" clojure-keyword-face))

    ("{:seg.mnt/seg.mnt 0}"
     ("seg.mnt" font-lock-type-face)
     ("/" default)
     ("seg.mnt" clojure-keyword-face))

    ("{:seg.mnt/CmlCase 0}"
     ("seg.mnt" font-lock-type-face)
     ("/" default)
     ("CmlCase" clojure-keyword-face))

    ("{:seg.mnt/mxdCase 0}"
     ("seg.mnt" font-lock-type-face)
     ("/" default)
     ("mxdCase" clojure-keyword-face))

    ("{:seg.mnt/ve/yCom|pLex.stu-ff 0}"
     ("seg.mnt" font-lock-type-face)
     ("/" default)
     ("ve/yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:seg.mnt/.ve/yCom|pLex.stu-ff 0}"
     ("seg.mnt" font-lock-type-face)
     ("/" default)
     (".ve/yCom|pLex.stu-ff" clojure-keyword-face)))

  (when-fontifying-it "should handle camel case keywords"
    (" :CmlCase"
     ("CmlCase" clojure-keyword-face))

    ("{:CmlCase 0}"
     ("CmlCase" clojure-keyword-face))

    ("{:#CmlCase 0}"
     ("#CmlCase" clojure-keyword-face))

    ("{:.CmlCase 0}"
     (".CmlCase" clojure-keyword-face))

    ("{:CmlCase/oneword 0}"
     ("CmlCase" font-lock-type-face)
     ("/" default)
     ("oneword" clojure-keyword-face))

    ("{:CmlCase/seg.mnt 0}"
     ("CmlCase" font-lock-type-face)
     ("/" default)
     ("seg.mnt" clojure-keyword-face))

    ("{:CmlCase/CmlCase 0}"
     ("CmlCase" font-lock-type-face)
     ("/" default)
     ("CmlCase" clojure-keyword-face))

    ("{:CmlCase/mxdCase 0}"
     ("CmlCase" font-lock-type-face)
     ("/" default)
     ("mxdCase" clojure-keyword-face))

    ("{:CmlCase/ve/yCom|pLex.stu-ff 0}"
     ("CmlCase" font-lock-type-face)
     ("/" default)
     ("ve/yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:CmlCase/.ve/yCom|pLex.stu-ff 0}"
     ("CmlCase" font-lock-type-face)
     ("/" default)
     (".ve/yCom|pLex.stu-ff" clojure-keyword-face)))

  (when-fontifying-it "should handle mixed case keywords"
    (" :mxdCase"
     ("mxdCase" clojure-keyword-face))

    ("{:mxdCase 0}"
     ("mxdCase" clojure-keyword-face))

    ("{:#mxdCase 0}"
     ("#mxdCase" clojure-keyword-face))

    ("{:.mxdCase 0}"
     (".mxdCase" clojure-keyword-face))

    ("{:mxdCase/oneword 0}"
     ("mxdCase" font-lock-type-face)
     ("/" default)
     ("oneword" clojure-keyword-face))

    ("{:mxdCase/seg.mnt 0}"
     ("mxdCase" font-lock-type-face)
     ("/" default)
     ("seg.mnt" clojure-keyword-face))

    ("{:mxdCase/CmlCase 0}"
     ("mxdCase" font-lock-type-face)
     ("/" default)
     ("CmlCase" clojure-keyword-face))

    ("{:mxdCase/mxdCase 0}"
     ("mxdCase" font-lock-type-face)
     ("/" default)
     ("mxdCase" clojure-keyword-face))

    ("{:mxdCase/ve/yCom|pLex.stu-ff 0}"
     ("mxdCase" font-lock-type-face)
     ("/" default)
     ("ve/yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:mxdCase/.ve/yCom|pLex.stu-ff 0}"
     ("mxdCase" font-lock-type-face)
     ("/" default)
     (".ve/yCom|pLex.stu-ff" clojure-keyword-face)))

  (when-fontifying-it "should handle keywords with colons"
    (":a:a"
     (1 4 clojure-keyword-face))

    (":a:a/:a"
     (1 7 clojure-keyword-face))

    ("::a:a"
     (1 5 clojure-keyword-face))

    ("::a.a:a"
     (1 7 clojure-keyword-face)))

  (when-fontifying-it "should handle very complex keywords"
    (" :ve/yCom|pLex.stu-ff"
     ("ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff 0}"
     (":" clojure-keyword-face)
     ("ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:#ve/yCom|pLex.stu-ff 0}"
     (":" clojure-keyword-face)
     ("#ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:.ve/yCom|pLex.stu-ff 0}"
     (":" clojure-keyword-face)
     (".ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/oneword 0}"
     (":" clojure-keyword-face)
     ("ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff/oneword" clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/seg.mnt 0}"
     (":" clojure-keyword-face)
     ("ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff/seg.mnt" clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/ClmCase 0}"
     (":" clojure-keyword-face)
     ("ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff/ClmCase" clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/mxdCase 0}"
     (":" clojure-keyword-face)
     ("ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff/mxdCase" clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff 0}"
     (":" clojure-keyword-face)
     ("ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff/ve/yCom|pLex.stu-ff" clojure-keyword-face))

    ("{:ve/yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff 0}"
     (":" clojure-keyword-face)
     ("ve" font-lock-type-face)
     ("/" default)
     ("yCom|pLex.stu-ff/.ve/yCom|pLex.stu-ff" clojure-keyword-face)))

  (when-fontifying-it "should handle namespaced defs"
    ("(clojure.core/defn bar [] nil)"
     ("clojure.core" font-lock-type-face)
     ("/" nil)
     ("defn" font-lock-keyword-face)
     ("bar" font-lock-function-name-face))

    ("(clojure.core/defrecord foo nil)"
     ("clojure.core" font-lock-type-face)
     ("/" nil)
     ("defrecord" font-lock-keyword-face)
     ("foo" font-lock-type-face))

    ("(s/def ::keyword)"
     ("s" font-lock-type-face)
     ("/" nil)
     ("def" font-lock-keyword-face)
     ("::keyword" clojure-keyword-face)))

  (when-fontifying-it "should handle any known def form"
    ("(def a 1)" ("def" font-lock-keyword-face))
    ("(defonce a 1)" ("defonce" font-lock-keyword-face))
    ("(defn a [b])" ("defn" font-lock-keyword-face))
    ("(defmacro a [b])" ("defmacro" font-lock-keyword-face))
    ("(definline a [b])" ("definline" font-lock-keyword-face))
    ("(defmulti a identity)" ("defmulti" font-lock-keyword-face))
    ("(defmethod a :foo [b] (println \"bar\"))" ("defmethod" font-lock-keyword-face))
    ("(defprotocol a (b [this] \"that\"))" ("defprotocol" font-lock-keyword-face))
    ("(definterface a (b [c]))" ("definterface" font-lock-keyword-face))
    ("(defrecord a [b c])" ("defrecord" font-lock-keyword-face))
    ("(deftype a [b c])" ("deftype" font-lock-keyword-face))
    ("(defstruct a :b :c)" ("defstruct" font-lock-keyword-face))
    ("(deftest a (is (= 1 1)))" ("deftest" font-lock-keyword-face))
    ("(defne [x y])" ("defne" font-lock-keyword-face))
    ("(defnm a b)" ("defnm" font-lock-keyword-face))
    ("(defnu)" ("defnu" font-lock-keyword-face))
    ("(defnc [a])" ("defnc" font-lock-keyword-face))
    ("(defna)" ("defna" font-lock-keyword-face))
    ("(deftask a)" ("deftask" font-lock-keyword-face))
    ("(defstate a :start \"b\" :stop \"c\")" ("defstate" font-lock-keyword-face)))

  (when-fontifying-it "should ignore unknown def forms"
    ("(defbugproducer me)" ("defbugproducer" nil))
    ("(default-user-settings {:a 1})" ("default-user-settings" nil))
    ("(s/deftartar :foo)" ("deftartar" nil)))

  (when-fontifying-it "should handle variables defined with def"
    ("(def foo 10)"
     ("def" font-lock-keyword-face)
     ("foo" font-lock-variable-name-face))
    ("(def foo:bar 10)"
     ("def" font-lock-keyword-face)
     ("foo:bar" font-lock-variable-name-face)))

  (when-fontifying-it "should handle variables definitions of type string"
    ("(def foo \"hello\")"
     (10 16 font-lock-string-face))

    ("(def foo \"hello\"   )"
     (10 16 font-lock-string-face))

    ("(def foo \n  \"hello\")"
     (13 19 font-lock-string-face))

    ("(def foo \n  \"hello\"\n)"
     (13 19 font-lock-string-face)))

  (when-fontifying-it "variable-def-string-with-docstring"
    ("(def foo \"usage\" \"hello\")"
     (10 16 font-lock-doc-face)
     (18 24 font-lock-string-face))

    ("(def foo \"usage\" \"hello\"   )"
     (18 24 font-lock-string-face))

    ("(def foo \"usage\" \n  \"hello\")"
     (21 27 font-lock-string-face))

    ("(def foo \n  \"usage\" \"hello\")"
     (13 19 font-lock-doc-face))

    ("(def foo \n  \"usage\" \n  \"hello\")"
     (13 19 font-lock-doc-face)
     (24 30 font-lock-string-face))

    ("(def test-string\n  \"this\\n\n  is\n  my\n  string\")"
     (20 24 font-lock-string-face)
     (25 26 (bold font-lock-string-face))
     (27 46 font-lock-string-face)))

  (when-fontifying-it "should handle deftype"
    ("(deftype Foo)"
     ("deftype" font-lock-keyword-face)
     ("Foo" font-lock-type-face)))

  (when-fontifying-it "should handle defn"
    ("(defn foo [x] x)"
     ("defn" font-lock-keyword-face)
     ("foo" font-lock-function-name-face)))

  (when-fontifying-it "should handle fn"
    ;; try to byte-recompile the clojure-mode.el when the face of 'fn' is 't'
    ("(fn foo [x] x)"
     ("fn" font-lock-keyword-face)
     ("foo" font-lock-function-name-face)))

  (when-fontifying-it "should handle lambda-params %, %1, %n..."
    ("#(+ % %2 %3 %&)"
     ("%" font-lock-variable-name-face)
     ("%2" font-lock-variable-name-face)
     ("%3" font-lock-variable-name-face)
     ("%&" font-lock-variable-name-face)))

  (when-fontifying-it "should handle multi-digit lambda-params"
    ;; % args with >1 digit are rare and unidiomatic but legal up to
    ;; `MAX_POSITIONAL_ARITY` in Clojure's compiler, which as of today is 20
    ("#(* %10 %15 %19 %20)"
     ;; it would be better if this were just `font-lock-variable-name-face` but
     ;; it seems to work as-is
     (5 7   various-faces)
     (9 11  font-lock-variable-name-face)
     (13 15 font-lock-variable-name-face)
     (17 19 various-faces)))

  (when-fontifying-it "should handle nils"
    ("(= nil x)"
     ("nil" font-lock-constant-face))

    ("(fnil x)"
     ("nil" nil)))

  (when-fontifying-it "should handle true"
    ("(= true x)"
     ("true" font-lock-constant-face)))

  (when-fontifying-it "should handle false"
    ("(= false x)"
     ("false" font-lock-constant-face)))

  (when-fontifying-it "should handle keyword-meta"
    ("^:meta-data"
     (1 1 nil)
     (2 11 clojure-keyword-face)))

  (when-fontifying-it "should handle a keyword with allowed characters"
    (":aaa#bbb"
     (1 8 clojure-keyword-face)))

  (when-fontifying-it "should handle a keyword with disallowed characters"
    (":aaa@bbb"
     (1 5 various-faces))

    (":aaa@bbb"
     (1 4 clojure-keyword-face))

    (":aaa~bbb"
     (1 5 various-faces))

    (":aaa~bbb"
     (1 4 clojure-keyword-face))

    (":aaa@bbb"
     (1 5 various-faces))

    (":aaa@bbb"
     (1 4 clojure-keyword-face)))

  (when-fontifying-it "should handle characters"
    ("\\a"
     (1 2 clojure-character-face))

    ("\\A"
     (1 2 clojure-character-face))

    ("\\newline"
     (1 8 clojure-character-face))

    ("\\abc"
     (1 4 nil))

    ("\\newlin"
     (1 7 nil))

    ("\\newlinex"
     (1 9 nil))

    ("\\1"
     (1 2 clojure-character-face))

    ("\\u0032"
     (1 6 clojure-character-face))

    ("\\o127"
     (1 4 clojure-character-face))

    ("\\+"
     (1 2 clojure-character-face))

    ("\\."
     (1 2 clojure-character-face))

    ("\\,"
     (1 2 clojure-character-face))

    ("\\;"
     (1 2 clojure-character-face))

    ("\\Ω"
     (1 2 clojure-character-face))

    ("\\ク"
     (1 2 clojure-character-face)))

  (when-fontifying-it "should handle characters not by themselves"
    ("[\\,,]"
     (1 1 nil)
     (2 3 clojure-character-face)
     (4 5 nil))

    ("[\\[]"
     (1 1 nil)
     (2 3 clojure-character-face)
     (4 4 nil)))

  (when-fontifying-it "should handle % character literal"
    ("#(str \\% %)"
     (7 8 clojure-character-face)
     (10 10 font-lock-variable-name-face)))

  (when-fontifying-it "should handle referred vars"
    ("foo/var"
     ("foo" font-lock-type-face))

    ("@foo/var"
     ("foo" font-lock-type-face)))

  (when-fontifying-it "should handle dynamic vars"
    ("*some-var*"
     ("*some-var*" font-lock-variable-name-face))

    ("@*some-var*"
     ("*some-var*" font-lock-variable-name-face))

    ("some.ns/*var*"
     ("*var*" font-lock-variable-name-face))

    ("*some-var?*"
     ("*some-var?*" font-lock-variable-name-face)))

  (when-fontifying-it "should handle letfn binding names"
    ("(letfn [(twice [x] (* x 2))])"
     ("letfn" font-lock-keyword-face)
     ("twice" font-lock-function-name-face))

    ("(letfn [(twice [x] (* x 2)) (six-times [y] (* (twice y) 3))])"
     ("twice" font-lock-function-name-face)
     ("six-times" font-lock-function-name-face))

    ("(clojure.core/letfn [(twice [x] (* x 2))])"
     ("twice" font-lock-function-name-face))))

(describe "docstring font-locking"
  (it "should font-lock defn docstrings"
    (expect-face-of "(defn foo\n  \"docstring\"\n  [x] x)" "docstring" font-lock-doc-face))

  (it "should font-lock defprotocol docstrings"
    (expect-face-of "(defprotocol Foo\n  \"protocol doc\")" "protocol doc" font-lock-doc-face))

  (it "should font-lock protocol method docstrings"
    (expect-face-of "(defprotocol Foo\n  (bar [this]\n    \"method doc\"))" "method doc" font-lock-doc-face))

  (it "should font-lock protocol method docstrings with multiple arities"
    (expect-face-of "(defprotocol Foo\n  (bar [this] [this x]\n    \"method doc\"))" "method doc" font-lock-doc-face))

  (it "should not font-lock regular strings in protocol methods as docstrings"
    (expect-face-of "(defprotocol Foo\n  (bar [this]\n    \"not a doc\" \"method doc\"))"
                    "not a doc" font-lock-string-face)))

(provide 'clojure-mode-font-lock-test)

;;; clojure-mode-font-lock-test.el ends here
