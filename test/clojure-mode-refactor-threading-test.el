;;; clojure-mode-refactor-threading-test.el --- Clojure Mode: refactor threading tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Benedek Fazekas <benedek.fazekas@gmail.com>

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

;; The threading refactoring code is ported from clj-refactor.el
;; and mainly the work of Magnar Sveen, Alex Baranosky and
;; the rest of the clj-reafctor.el team.

;;; Code:

(require 'clojure-mode)
(require 'ert)

;; thread first

(ert-deftest test-thread-first-one-step ()
  (with-temp-buffer
    (insert "(-> (dissoc (assoc {} :key \"value\") :lock))")
    (clojure-mode)
    (clojure-thread)
    (should
     (equal
      "(-> (assoc {} :key \"value\")
    (dissoc :lock))"
      (buffer-string)))))

(ert-deftest test-thread-first-two-steps ()
  (with-temp-buffer
    (insert "(-> (dissoc (assoc {} :key \"value\") :lock))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"
      (buffer-string)))))

(ert-deftest test-thread-first-dont-thread-maps ()
  (with-temp-buffer
    (insert "(-> (dissoc (assoc {} :key \"value\") :lock))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"
      (buffer-string)))))

(ert-deftest test-thread-first-dont-thread-last-one ()
  (with-temp-buffer
    (insert "(-> (dissoc (assoc (get-a-map) :key \"value\") :lock))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(-> (get-a-map)
    (assoc :key \"value\")
    (dissoc :lock))"
      (buffer-string)))))

(ert-deftest test-thread-first-easy-on-whitespace ()
  (with-temp-buffer
    (insert "(->
 (dissoc (assoc {} :key \"value\") :lock))")
    (clojure-mode)
    (clojure-thread)
    (should
     (equal
      "(->
 (assoc {} :key \"value\")
 (dissoc :lock))"
      (buffer-string)))))

(ert-deftest test-thread-first-remove-superfluous-parens ()
  (with-temp-buffer
    (insert "(-> (square (sum [1 2 3 4 5])))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(-> [1 2 3 4 5]
    sum
    square)"
      (buffer-string)))))

(ert-deftest test-thread-first-cursor-before-threading ()
  (with-temp-buffer
    (insert "(-> (not (s-acc/mobile? session)))")
    (clojure-mode)
    (beginning-of-buffer)
    (clojure-thread)
    (should
     (equal
      "(-> (s-acc/mobile? session)
    not)"
      (buffer-string)))))

;; unwind thread first
(ert-deftest test-unwind-first-one-step ()
  (with-temp-buffer
    (insert "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))")
    (clojure-mode)
    (clojure-unwind)
    (should
     (equal
      "(-> (assoc {} :key \"value\")
    (dissoc :lock))"
      (buffer-string)))))

(ert-deftest test-unwind-first-two-steps ()
  (with-temp-buffer
    (insert "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))")
    (clojure-mode)
    (clojure-unwind)
    (clojure-unwind)
    (should
     (equal
      "(-> (dissoc (assoc {} :key \"value\") :lock))"
      (buffer-string)))))

(ert-deftest test-unwind-first-jump-out-of-threading ()
  (with-temp-buffer
    (insert "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))")
    (clojure-mode)
    (clojure-unwind)
    (clojure-unwind)
    (clojure-unwind)
    (should
     (equal
      "(dissoc (assoc {} :key \"value\") :lock)"
      (buffer-string)))))

;; thread last
(ert-deftest test-thread-last-one-step ()
  (with-temp-buffer
    (insert "(->> (map square (filter even? [1 2 3 4 5])))")
    (clojure-mode)
    (clojure-thread)
    (should
     (equal
      "(->> (filter even? [1 2 3 4 5])
     (map square))"
      (buffer-string)))))

(ert-deftest test-thread-last-two-steps ()
  (with-temp-buffer
    (insert "(->> (map square (filter even? [1 2 3 4 5])))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"
      (buffer-string)))))

(ert-deftest test-thread-last-dont-thread-vectors ()
  (with-temp-buffer
    (insert "(->> (map square (filter even? [1 2 3 4 5])))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"
      (buffer-string)))))

(ert-deftest test-thread-last-dont-thread-last-one ()
  (with-temp-buffer
    (insert "(->> (map square (filter even? (get-a-list))))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(->> (get-a-list)
     (filter even?)
     (map square))"
      (buffer-string)))))

;; unwind thread last
(ert-deftest test-unwind-last-one-step ()
  (with-temp-buffer
    (insert "(->> [1 2 3 4 5]
     (filter even?)
     (map square))")
    (clojure-mode)
    (clojure-unwind)
    (should
     (equal
      "(->> (filter even? [1 2 3 4 5])
     (map square))"
      (buffer-string)))))

(ert-deftest test-unwind-last-two-steps ()
  (with-temp-buffer
    (insert "(->> [1 2 3 4 5]
     (filter even?)
     (map square))")
    (clojure-mode)
    (clojure-unwind)
    (clojure-unwind)
    (should
     (equal
      "(->> (map square (filter even? [1 2 3 4 5])))"
      (buffer-string)))))

(ert-deftest test-unwind-last-jump-out-of-threading ()
  (with-temp-buffer
    (insert "(->> [1 2 3 4 5]
     (filter even?)
     (map square))")
    (clojure-mode)
    (clojure-unwind)
    (clojure-unwind)
    (clojure-unwind)
    (should
     (equal
      "(map square (filter even? [1 2 3 4 5]))"
      (buffer-string)))))

(ert-deftest test-unwind-function-name ()
  (with-temp-buffer
    (insert "(->> [1 2 3 4 5]
     sum
     square)")
    (clojure-mode)
    (clojure-unwind)
    (should
     (equal
      "(->> (sum [1 2 3 4 5])
     square)"
      (buffer-string)))))

(ert-deftest test-unwind-function-name-twice ()
  (with-temp-buffer
    (insert "(-> [1 2 3 4 5]
     sum
     square)")
    (clojure-mode)
    (clojure-unwind)
    (clojure-unwind)
    (should
     (equal
      "(-> (square (sum [1 2 3 4 5])))"
      (buffer-string)))))

(ert-deftest test-unwind-issue-6-1 ()
  (with-temp-buffer
    (insert "(defn plus [a b]
  (-> a (+ b)))")
    (clojure-mode)
    (clojure-unwind)
    (should
     (equal
      "(defn plus [a b]
  (-> (+ a b)))"
      (buffer-string)))))

(ert-deftest test-unwind-issue-6-2 ()
  (with-temp-buffer
    (insert "(defn plus [a b]
  (->> a (+ b)))")
    (clojure-mode)
    (clojure-unwind)
    (should
     (equal
      "(defn plus [a b]
  (->> (+ b a)))"
      (buffer-string)))))

(ert-deftest test-thread-first-some ()
  (with-temp-buffer
    (insert "(some-> (+ (val (find {:a 1} :b)) 5))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(some-> {:a 1}
        (find :b)
        val
        (+ 5))"
      (buffer-string)))))

(ert-deftest test-thread-last-some ()
  (with-temp-buffer
    (insert "(some->> (+ 5 (val (find {:a 1} :b))))")
    (clojure-mode)
    (clojure-thread)
    (clojure-thread)
    (clojure-thread)
    (should
     (equal
      "(some->> :b
         (find {:a 1})
         val
         (+ 5))"
      (buffer-string)))))

(ert-deftest test-unwind-last-first-some ()
  (with-temp-buffer
    (insert "(some-> {:a 1}
        (find :b)
        val
        (+ 5))")
    (clojure-mode)
    (clojure-unwind)
    (clojure-unwind)
    (clojure-unwind)
    (should
     (equal
      "(some-> (+ (val (find {:a 1} :b)) 5))"
      (buffer-string)))))

(ert-deftest test-unwind-thread-last-some ()
  (with-temp-buffer
    (insert "(some->> :b
         (find {:a 1})
         val
         (+ 5))")
    (clojure-mode)
    (clojure-unwind)
    (clojure-unwind)
    (clojure-unwind)
    (should
     (equal
      "(some->> (+ 5 (val (find {:a 1} :b))))"
      (buffer-string)))))

(ert-deftest test-thread-first-all ()
  (with-temp-buffer
    (insert "(->map (assoc {} :key \"value\") :lock)")
    (clojure-mode)
    (beginning-of-buffer)
    (clojure-thread-first-all nil)
    (should
     (equal
      "(-> {}
    (assoc :key \"value\")
    (->map :lock))"
      (buffer-string)))))

(ert-deftest test-thread-first-all-but-last ()
  (with-temp-buffer
    (insert "(->map (assoc {} :key \"value\") :lock)")
    (clojure-mode)
    (beginning-of-buffer)
    (clojure-thread-first-all t)
    (should
     (equal
      "(-> (assoc {} :key \"value\")
    (->map :lock))"
      (buffer-string)))))

(ert-deftest test-thread-last-all ()
  (with-temp-buffer
    (insert "(map square (filter even? (make-things)))")
    (clojure-mode)
    (beginning-of-buffer)
    (clojure-thread-last-all nil)
    (should
     (equal
      "(->> (make-things)
     (filter even?)
     (map square))"
      (buffer-string)))))

(ert-deftest test-thread-last-all-but-last ()
  (with-temp-buffer
    (insert "(map square (filter even? (make-things)))")
    (clojure-mode)
    (beginning-of-buffer)
    (clojure-thread-last-all t)
    (should
     (equal
      "(->> (filter even? (make-things))
     (map square))"
      (buffer-string)))))

(ert-deftest test-unwind-all-thread-first ()
  (with-temp-buffer
    (insert "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))")
    (clojure-mode)
    (beginning-of-buffer)
    (clojure-unwind-all)
    (should
     (equal
      "(dissoc (assoc {} :key \"value\") :lock)"
      (buffer-string)))))

(ert-deftest test-unwind-all-thread-last ()
  (with-temp-buffer
    (insert "(->> (make-things)
     (filter even?)
     (map square))")
    (clojure-mode)
    (beginning-of-buffer)
    (clojure-unwind-all)
    (should
     (equal
      "(map square (filter even? (make-things)))"
      (buffer-string)))))

(ert-deftest test-thread-last-dangling-parens ()
  (with-temp-buffer
    (insert "(map inc
     (range))")
    (clojure-mode)
    (beginning-of-buffer)
    (clojure-thread-last-all nil)
    (should
     (equal
      "(->> (range)
     (map inc))"
      (buffer-string)))))

;; fix for clojure-emacs/clj-refactor.el#259
(ert-deftest test-unwind-last-leaves-multiline-sexp-alone ()
  (with-temp-buffer
    (insert
     "(->> [a b]
     (some (fn [x]
             (when x
               10))))")
    (clojure-mode)
    (clojure-unwind-all)
    (should
     (equal
      "(some (fn [x]
        (when x
          10))
      [a b])"
      (buffer-string)))))

(provide 'clojure-mode-refactor-threading-test)

;;; clojure-mode-refactor-threading-test.el ends here
