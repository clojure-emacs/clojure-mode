;;; clojure-mode-sexp-test.el --- Clojure Mode: sexp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 Artur Malabarba <artur@endlessparentheses.com>

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
(require 'ert)

(defmacro clojure-buffer-with-text (text &rest body)
  "Run body in a temporary clojure buffer with TEXT.
TEXT is a string with a | indicating where point is. The | will be erased
and point left there."
  (declare (indent 2))
  `(progn
     (with-temp-buffer
       (erase-buffer)
       (clojure-mode)
       (insert ,text)
       (goto-char (point-min))
       (re-search-forward "|")
       (delete-char -1)
       ,@body)))

(ert-deftest test-clojure-top-level-form-p ()
  (clojure-buffer-with-text
      "(comment
         (wrong)
         (rig|ht)
         (wrong))"
      ;; make this use the native beginning of defun since this is used to
      ;; determine whether to use the comment aware version or not.
      (should (let ((beginning-of-defun-function nil))
                (clojure-top-level-form-p "comment")))))

(ert-deftest test-clojure-beginning-of-defun-function ()
  (clojure-buffer-with-text
      "(comment
          (wrong)
          (wrong)
          (rig|ht)
          (wrong))"
      (beginning-of-defun)
    (should (looking-at-p "(comment")))
  (clojure-buffer-with-text
      "(comment
          (wrong)
          (wrong)
          (rig|ht)
          (wrong))"
      (let ((clojure-toplevel-inside-comment-form t))
       (beginning-of-defun))
    (should (looking-at-p "[[:space:]]*(right)"))))

(ert-deftest test-clojure-end-of-defun-function ()
  (clojure-buffer-with-text
      "
(first form)
|
(second form)

(third form)"
      
      (end-of-defun)
    (backward-char)
    (should (looking-back "(second form)"))))


(ert-deftest test-sexp-with-commas ()
  (with-temp-buffer
    (insert "[], {}, :a, 2")
    (clojure-mode)
    (goto-char (point-min))
    (clojure-forward-logical-sexp 1)
    (should (looking-at-p " {}, :a, 2"))
    (clojure-forward-logical-sexp 1)
    (should (looking-at-p " :a, 2"))))

(ert-deftest test-sexp ()
  (with-temp-buffer
    (insert "^String #macro ^dynamic reverse")
    (clojure-mode)
    (clojure-backward-logical-sexp 1)
    (should (looking-at-p "\\^String \\#macro \\^dynamic reverse"))
    (clojure-forward-logical-sexp 1)
    (should (looking-back "\\^String \\#macro \\^dynamic reverse"))
    (insert " ^String biverse inverse")
    (clojure-backward-logical-sexp 1)
    (should (looking-at-p "inverse"))
    (clojure-backward-logical-sexp 2)
    (should (looking-at-p "\\^String \\#macro \\^dynamic reverse"))
    (clojure-forward-logical-sexp 2)
    (should (looking-back "\\^String biverse"))
    (clojure-backward-logical-sexp 1)
    (should (looking-at-p "\\^String biverse"))))

(ert-deftest test-buffer-corners ()
  (with-temp-buffer
    (insert "^String reverse")
    (clojure-mode)
    ;; Return nil and don't error
    (should-not (clojure-backward-logical-sexp 100))
    (should (looking-at-p "\\^String reverse"))
    (should-not (clojure-forward-logical-sexp 100))
    (should (looking-at-p "$")))
  (with-temp-buffer
    (clojure-mode)
    (insert "(+ 10")
    (should-error (clojure-backward-logical-sexp 100))
    (goto-char (point-min))
    (should-error (clojure-forward-logical-sexp 100))
    ;; Just don't hang.
    (goto-char (point-max))
    (should-not (clojure-forward-logical-sexp 1))
    (erase-buffer)
    (insert "(+ 10")
    (newline)
    (erase-buffer)
    (insert "(+ 10")
    (newline-and-indent)))

(ert-deftest clojure-find-ns-test ()
  ;; we should not cache the results of `clojure-find-ns' here
  (let ((clojure-cache-ns nil))
    (with-temp-buffer
     (insert "(ns ^{:doc \"Some docs\"}\nfoo-bar)")
     (newline)
     (newline)
     (insert "(in-ns 'baz-quux)")
     (clojure-mode)

     ;; From inside docstring of first ns
     (goto-char 18)
     (should (equal "foo-bar" (clojure-find-ns)))

     ;; From inside first ns's name, on its own line
     (goto-char 29)
     (should (equal "foo-bar" (clojure-find-ns)))

     ;; From inside second ns's name
     (goto-char 42)
     (should (equal "baz-quux" (clojure-find-ns))))))

(provide 'clojure-mode-sexp-test)

;;; clojure-mode-sexp-test.el ends here
