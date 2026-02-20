;;; clojure-mode-update-ns-test.el --- Clojure Mode: update-ns tests  -*- lexical-binding: t; -*-

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

;; Tests for clojure-update-ns.

;;; Code:

(require 'clojure-mode)
(require 'buttercup)
(require 'test-helper "test/utils/test-helper")

(describe "clojure-update-ns"

  (it "should update a simple ns form"
    (with-clojure-buffer "(ns old.name)"
      (let ((clojure-expected-ns-function (lambda () "new.name")))
        (clojure-update-ns)
        (expect (buffer-string) :to-equal "(ns new.name)"))))

  (it "should update ns with metadata"
    (with-clojure-buffer "(ns ^:bar old.name)"
      (let ((clojure-expected-ns-function (lambda () "new.name")))
        (clojure-update-ns)
        (expect (buffer-string) :to-equal "(ns ^:bar new.name)"))))

  (it "should update ns with map metadata"
    (with-clojure-buffer "(ns ^{:doc \"hello\"} old.name)"
      (let ((clojure-expected-ns-function (lambda () "new.name")))
        (clojure-update-ns)
        (expect (buffer-string) :to-equal "(ns ^{:doc \"hello\"} new.name)"))))

  (it "should update ns with require forms"
    (with-clojure-buffer "(ns old.name
  (:require [clojure.string :as str]))"
      (let ((clojure-expected-ns-function (lambda () "new.name")))
        (clojure-update-ns)
        (expect (buffer-string) :to-equal "(ns new.name
  (:require [clojure.string :as str]))"))))

  (it "should update in-ns forms"
    (with-clojure-buffer "(in-ns 'old.name)"
      (let ((clojure-expected-ns-function (lambda () "new.name")))
        (clojure-update-ns)
        (expect (buffer-string) :to-equal "(in-ns 'new.name)"))))

  (it "should update the cached namespace"
    (with-clojure-buffer "(ns old.name)"
      (let ((clojure-expected-ns-function (lambda () "new.name")))
        (clojure-update-ns)
        (expect clojure-cached-ns :to-equal "new.name"))))

  (it "should signal an error when no ns form is found"
    (with-clojure-buffer "(defn foo [] 1)"
      (let ((clojure-expected-ns-function (lambda () "new.name")))
        (expect (clojure-update-ns) :to-throw 'user-error)))))

(provide 'clojure-mode-update-ns-test)

;;; clojure-mode-update-ns-test.el ends here
