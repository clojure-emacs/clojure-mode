;;; clojure-mode-sexp-test.el --- Clojure Mode: sexp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 Artur Malabarba <artur@endlessparentheses.com>

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

;;; Code:

(require 'clojure-mode)
(require 'buttercup)

(describe "clojure-top-level-form-p"
  (it "should return true when passed the correct form"
    (with-clojure-buffer-point
        "(comment
           (wrong)
           (rig|ht)
           (wrong))"
        ;; make this use the native beginning of defun since this is used to
        ;; determine whether to use the comment aware version or not.
        (expect (let ((beginning-of-defun-function nil))
                  (clojure-top-level-form-p "comment")))))
  (it "should return true when multiple forms are present"
    (with-clojure-buffer-point
        "(+ 1 2) (comment
           (wrong)
           (rig|ht)
           (wrong))"
        (expect (let ((beginning-of-defun-function nil))
                  (clojure-top-level-form-p "comment"))))))

(describe "clojure-beginning-of-defun-function"
  (it "should go to top level form"
    (with-clojure-buffer-point
      " (comment
          (wrong)
          (wrong)
          (rig|ht)
          (wrong))"
      (clojure-beginning-of-defun-function)
      (expect (looking-at-p "(comment"))))

  (it "should eval top level forms inside comment forms when clojure-toplevel-inside-comment-form set to true"
    (with-clojure-buffer-point
      "(+ inc 1) (comment
          (wrong)
          (wrong) (rig|ht)
          (wrong))"
      (let ((clojure-toplevel-inside-comment-form t))
       (clojure-beginning-of-defun-function))
      (expect (looking-at-p "(right)"))))

  (it "should go to beginning of previous top level form"
    (with-clojure-buffer-point
     "
(formA)
|
(formB)"
     (let ((clojure-toplevel-inside-comment-form t))
       (beginning-of-defun)
       (expect (looking-at-p "(formA)")))))

  (it "should move forward to next top level form"
    (with-clojure-buffer-point
      "
(first form)
|
(second form)

(third form)"

      (end-of-defun)
      (backward-char)
      (expect (looking-back "(second form)")))))

(describe "clojure-forward-logical-sexp"
  (it "should work with commas"
    (with-clojure-buffer "[], {}, :a, 2"
      (goto-char (point-min))
      (clojure-forward-logical-sexp 1)
      (expect (looking-at-p " {}, :a, 2"))
      (clojure-forward-logical-sexp 1)
      (expect (looking-at-p " :a, 2")))))

(describe "clojure-backward-logical-sexp"
  (it "should work when used in conjunction with clojure-forward-logical-sexp"
    (with-clojure-buffer "^String #macro ^dynamic reverse"
      (clojure-backward-logical-sexp 1)
      (expect (looking-at-p "\\^String \\#macro \\^dynamic reverse"))
      (clojure-forward-logical-sexp 1)
      (expect (looking-back "\\^String \\#macro \\^dynamic reverse"))
      (insert " ^String biverse inverse")
      (clojure-backward-logical-sexp 1)
      (expect (looking-at-p "inverse"))
      (clojure-backward-logical-sexp 2)
      (expect (looking-at-p "\\^String \\#macro \\^dynamic reverse"))
      (clojure-forward-logical-sexp 2)
      (expect (looking-back "\\^String biverse"))
      (clojure-backward-logical-sexp 1)
      (expect (looking-at-p "\\^String biverse"))))

  (it "should handle a namespaced map"
    (with-clojure-buffer "first #:name/space{:k v}"
      (clojure-backward-logical-sexp 1)
      (expect (looking-at-p "#:name/space{:k v}"))
      (insert  " #::ns {:k v}")
      (clojure-backward-logical-sexp 1)
      (expect (looking-at-p "#::ns {:k v}")))))

(describe "clojure-backward-logical-sexp"
  (it "should work with buffer corners"
    (with-clojure-buffer "^String reverse"
      ;; Return nil and don't error
      (expect (clojure-backward-logical-sexp 100) :to-be nil)
      (expect (looking-at-p "\\^String reverse"))
      (expect (clojure-forward-logical-sexp 100) :to-be nil)
      (expect (looking-at-p "$")))
    (with-clojure-buffer "(+ 10"
      (expect (clojure-backward-logical-sexp 100) :to-throw 'error)
      (goto-char (point-min))
      (expect (clojure-forward-logical-sexp 100) :to-throw 'error)
      ;; Just don't hang.
      (goto-char (point-max))
      (expect (clojure-forward-logical-sexp 1) :to-be nil)
      (erase-buffer)
      (insert "(+ 10")
      (newline)
      (erase-buffer)
      (insert "(+ 10")
      (newline-and-indent))))

(describe "clojure-find-ns"
  (it "should return the namespace from various locations in the buffer"
    ;; we should not cache the results of `clojure-find-ns' here
    (let ((clojure-cache-ns nil))
      (with-clojure-buffer "(ns ^{:doc \"Some docs\"}\nfoo-bar)"
        (newline)
        (newline)
        (insert "(in-ns 'baz-quux)")

        ;; From inside docstring of first ns
        (goto-char 18)
        (expect (clojure-find-ns) :to-equal "foo-bar")

        ;; From inside first ns's name, on its own line
        (goto-char 29)
        (expect (clojure-find-ns) :to-equal "foo-bar")

        ;; From inside second ns's name
        (goto-char 42)
        (expect (equal "baz-quux" (clojure-find-ns))))
      (let ((data
             '(("\"\n(ns foo-bar)\"\n" "(in-ns 'baz-quux)" "baz-quux")
               (";(ns foo-bar)\n" "(in-ns 'baz-quux)" "baz-quux")
               ("(ns foo-bar)\n" "\"\n(in-ns 'baz-quux)\"" "foo-bar")
               ("(ns foo-bar)\n" ";(in-ns 'baz-quux)" "foo-bar"))))
        (pcase-dolist (`(,form1 ,form2 ,expected) data)
          (with-clojure-buffer form1
            (save-excursion (insert form2))
            ;; Between the two namespaces
            (expect (clojure-find-ns) :to-equal expected)
            ;; After both namespaces
            (goto-char (point-max))
            (expect (clojure-find-ns) :to-equal expected))))))

  (describe "`suppress-errors' argument"
    (let ((clojure-cache-ns nil))
      (describe "given a faulty ns form"
        (let ((ns-form "(ns )"))
          (describe "when the argument is `t'"
            (it "causes `clojure-find-ns' to return nil"
              (with-clojure-buffer ns-form
                                   (expect (equal nil (clojure-find-ns t))))))

          (describe "when the argument is `nil'"
            (it "causes `clojure-find-ns' to return raise an error"
              (with-clojure-buffer ns-form
                                   (expect (clojure-find-ns nil)
                                           :to-throw 'error)))))))))

(describe "clojure-sexp-starts-until-position"
  (it "should return starting points for forms after POINT until POSITION"
    (with-clojure-buffer "(run 1) (def b 2) (slurp \"file\")\n"
        (goto-char (point-min))
        (expect (not (cl-set-difference '(19 9 1)
                                        (clojure-sexp-starts-until-position (point-max)))))))

  (it "should return starting point for a single form in buffer after POINT"
    (with-clojure-buffer "comment\n"
        (goto-char (point-min))
        (expect (not (cl-set-difference '(1)
                                        (clojure-sexp-starts-until-position (point-max)))))))

  (it "should return nil if POSITION is behind POINT"
    (with-clojure-buffer "(run 1) (def b 2)\n"
        (goto-char (point-max))
        (expect (not (clojure-sexp-starts-until-position (- (point-max) 1)))))))

(provide 'clojure-mode-sexp-test)

;;; clojure-mode-sexp-test.el ends here
