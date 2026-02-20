;;; clojure-mode-toggle-keyword-string-test.el --- Clojure Mode: toggle keyword/string tests  -*- lexical-binding: t; -*-

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

;; Tests for clojure-toggle-keyword-string.

;;; Code:

(require 'clojure-mode)
(require 'buttercup)
(require 'test-helper "test/utils/test-helper")

(describe "clojure-toggle-keyword-string"

  (it "should convert a keyword to a string"
    (with-clojure-buffer-point "{:|foo 1}"
      (clojure-toggle-keyword-string)
      (expect (buffer-string) :to-equal "{\"foo\" 1}")))

  (it "should convert a string to a keyword"
    (with-clojure-buffer-point "{|\"foo\" 1}"
      (clojure-toggle-keyword-string)
      (expect (buffer-string) :to-equal "{:foo 1}")))

  (it "should convert a keyword with point in the middle"
    (with-clojure-buffer-point "{:fo|o 1}"
      (clojure-toggle-keyword-string)
      (expect (buffer-string) :to-equal "{\"foo\" 1}")))

  (it "should convert a string with point in the middle"
    (with-clojure-buffer-point "{\"fo|o\" 1}"
      (clojure-toggle-keyword-string)
      (expect (buffer-string) :to-equal "{:foo 1}")))

  (it "should handle a keyword with hyphens"
    (with-clojure-buffer-point "{:|foo-bar 1}"
      (clojure-toggle-keyword-string)
      (expect (buffer-string) :to-equal "{\"foo-bar\" 1}")))

  (it "should handle a string with hyphens"
    (with-clojure-buffer-point "{|\"foo-bar\" 1}"
      (clojure-toggle-keyword-string)
      (expect (buffer-string) :to-equal "{:foo-bar 1}")))

  (it "should handle a keyword with dots"
    (with-clojure-buffer-point "{:|foo.bar 1}"
      (clojure-toggle-keyword-string)
      (expect (buffer-string) :to-equal "{\"foo.bar\" 1}")))

  (it "should handle a namespaced keyword"
    (with-clojure-buffer-point "{:|foo/bar 1}"
      (clojure-toggle-keyword-string)
      (expect (buffer-string) :to-equal "{\"foo/bar\" 1}"))))

(provide 'clojure-mode-toggle-keyword-string-test)

;;; clojure-mode-toggle-keyword-string-test.el ends here
