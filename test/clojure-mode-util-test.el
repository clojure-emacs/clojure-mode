;;; clojure-mode-util-test.el --- Clojure Mode: util test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Bozhidar Batsov <bozhidar@batsov.com>

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
(require 'buttercup)


(describe "clojure-mode-version"
  (it "should not be nil"
    (expect clojure-mode-version)))

(let ((project-dir "/home/user/projects/my-project/")
      (clj-file-path "/home/user/projects/my-project/src/clj/my_project/my_ns/my_file.clj")
      (project-relative-clj-file-path "src/clj/my_project/my_ns/my_file.clj")
      (clj-file-ns "my-project.my-ns.my-file")
      (clojure-cache-project nil))

  (describe "clojure-project-relative-path"
    (cl-letf (((symbol-function 'clojure-project-dir) (lambda () project-dir)))
      (expect (string= (clojure-project-relative-path clj-file-path)
                       project-relative-clj-file-path))))

  (describe "clojure-expected-ns"
    (it "should return the namespace matching a path"
      (cl-letf (((symbol-function 'clojure-project-relative-path)
                 (lambda (&optional current-buffer-file-name)
                   project-relative-clj-file-path)))
        (expect (string= (clojure-expected-ns clj-file-path) clj-file-ns))))

    (it "should return the namespace even without a path"
      (cl-letf (((symbol-function 'clojure-project-relative-path)
                 (lambda (&optional current-buffer-file-name)
                   project-relative-clj-file-path)))
        (expect (string= (let ((buffer-file-name clj-file-path))
                           (clojure-expected-ns))
                         clj-file-ns))))))

(describe "clojure-namespace-name-regex"
  (it "should match common namespace declarations"
    (let ((ns "(ns foo)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (match-string 4 ns))
    (let ((ns "(ns
  foo)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "foo"))
    (let ((ns "(ns foo.baz)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "foo.baz"))
    (let ((ns "(ns ^:bar foo)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "foo"))
    (let ((ns "(ns ^:bar ^:baz foo)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "foo"))
    (let ((ns "(ns ^{:bar true} foo)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "foo"))
    (let ((ns "(ns #^{:bar true} foo)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "foo"))
  ;; TODO
  ;; (let ((ns "(ns #^{:fail {}} foo)"))
  ;;   (should (string-match clojure-namespace-name-regex ns))
  ;;   (match-string 4 ns))
  ;; (let ((ns "(ns ^{:fail2 {}} foo.baz)"))
  ;;   (should (string-match clojure-namespace-name-regex ns))
  ;;   (should (equal "foo.baz" (match-string 4 ns))))
    (let ((ns "(ns ^{} foo)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "foo"))
    (let ((ns "(ns ^{:skip-wiki true}
    aleph.netty"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "aleph.netty"))
    (let ((ns "(ns foo+)"))
      (expect (string-match clojure-namespace-name-regex ns))
      (expect (match-string 4 ns) :to-equal "foo+"))))

(describe "clojure-sort-ns"
  (it "should sort requires in a basic ns"
    (with-clojure-buffer "(ns my-app.core
    (:require [rum.core :as rum] ;comment
              [my-app.views [user-page :as user-page]]))"
      (clojure-sort-ns)
      (expect (buffer-string) :to-equal
              "(ns my-app.core
    (:require [my-app.views [user-page :as user-page]]
              [rum.core :as rum] ;comment\n))")))

  (it "should also sort imports in a ns"
    (with-clojure-buffer "\n(ns my-app.core
    (:require [my-app.views [front-page :as front-page]]
              [my-app.state :refer [state]] ; Comments too.
              ;; Some comments.
              [rum.core :as rum]
              [my-app.views [user-page :as user-page]]
              my-app.util.api)
    (:import java.io.Writer
             [clojure.lang AFunction Atom MultiFn Namespace]))"
      (clojure-mode)
      (clojure-sort-ns)
      (expect (buffer-string) :to-equal
                     "\n(ns my-app.core
    (:require [my-app.state :refer [state]] ; Comments too.
              my-app.util.api
              [my-app.views [front-page :as front-page]]
              [my-app.views [user-page :as user-page]]
              ;; Some comments.
              [rum.core :as rum])
    (:import [clojure.lang AFunction Atom MultiFn Namespace]
             java.io.Writer))"))))

(provide 'clojure-mode-util-test)

;;; clojure-mode-util-test.el ends here
