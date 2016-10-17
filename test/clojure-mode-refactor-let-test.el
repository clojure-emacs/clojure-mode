;;; clojure-mode-refactor-let-test.el --- Clojure Mode: refactor let  -*- lexical-binding: t; -*-

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

;; The refactor-let code originally was implemented in clj-refactor.el
;; and is the work of the clj-reafctor.el team.

;;; Code:

(require 'clojure-mode)
(require 'ert)

(def-refactor-test test-introduce-let
  "{:status 200
 :body (find-body abc)}"
  "{:status 200
 :body (let [body (find-body abc)]
         body)}"
  (search-backward "(find-body")
  (clojure--introduce-let-internal "body"))

(def-refactor-test test-introduce-expanded-let
  "(defn handle-request []
  {:status 200
   :length (count (find-body abc))
   :body (find-body abc)})"
  "(defn handle-request []
  (let [body (find-body abc)]
    {:status 200
     :length (count body)
     :body body}))"
  (search-backward "(find-body")
  (clojure--introduce-let-internal "body" 1))

(def-refactor-test test-let-replace-bindings-whitespace
  "(defn handle-request []
  {:status 200
   :length (count
             (find-body
               abc))
   :body (find-body abc)})"
  "(defn handle-request []
  (let [body (find-body abc)]
    {:status 200
     :length (count
              body)
     :body body}))"
  (search-backward "(find-body")
  (clojure--introduce-let-internal "body" 1))

(def-refactor-test test-let-forward-slurp-sexp
  "(defn handle-request []
  (let [body (find-body abc)]
    {:status 200
     :length (count body)
     :body body})
  (println (find-body abc))
  (println \"foobar\"))"
  "(defn handle-request []
  (let [body (find-body abc)]
    {:status 200
     :length (count body)
     :body body}
    (println body)
    (println \"foobar\")))"
  (search-backward "(count body")
  (clojure-let-forward-slurp-sexp 2))

(def-refactor-test test-let-backward-slurp-sexp
    "(defn handle-request []
  (println (find-body abc))
  (println \"foobar\")
  (let [body (find-body abc)]
    {:status 200
     :length (count body)
     :body body}))"
  "(defn handle-request []
  (let [body (find-body abc)]
    (println body)
    (println \"foobar\")
    {:status 200
     :length (count body)
     :body body}))"
  (search-backward "(count body")
  (clojure-let-backward-slurp-sexp 2))

(def-refactor-test test-move-sexp-to-let
  "(defn handle-request
  (let [body (find-body abc)]
    {:status (or status 500)
     :body body}))"
  "(defn handle-request
  (let [body (find-body abc)
        status (or status 500)]
    {:status status
     :body body}))"
  (search-backward "(or ")
  (clojure--move-to-let-internal "status"))

(def-refactor-test test-move-constant-to-when-let
  "(defn handle-request
  (when-let [body (find-body abc)]
    {:status 42
     :body body}))"
  "(defn handle-request
  (when-let [body (find-body abc)
             status 42]
    {:status status
     :body body}))"
  (search-backward "42")
  (clojure--move-to-let-internal "status"))

(def-refactor-test test-move-to-empty-let
  "(defn handle-request
  (if-let []
    {:status (or status 500)
     :body body}))"
  "(defn handle-request
  (if-let [status (or status 500)]
    {:status status
     :body body}))"
  (search-backward "(or ")
  (clojure--move-to-let-internal "status"))

(def-refactor-test test-introduce-let-at-move-to-let-if-missing
  "(defn handle-request
  {:status (or status 500)
   :body body})"
  "(defn handle-request
  {:status (let [status (or status 500)]
             status)
   :body body})"
  (search-backward "(or ")
  (clojure--move-to-let-internal "status"))

(def-refactor-test test-move-to-let-multiple-occurrences
  "(defn handle-request
  (let []
    (println \"body: \" body \", params: \" \", status: \" (or status 500))
    {:status (or status 500)
     :body body}))"
  "(defn handle-request
  (let [status (or status 500)]
    (println \"body: \" body \", params: \" \", status: \" status)
    {:status status
     :body body}))"
  (search-backward "(or ")
  (clojure--move-to-let-internal "status"))

;; clojure-emacs/clj-refactor.el#41
(def-refactor-test test-move-to-let-nested-scope
  "(defn foo []
  (let [x (range 10)]
    (doseq [x (range 10)]
      (let [x2 (* x x)]))
    (+ 1 1)))"
  "(defn foo []
  (let [x (range 10)
        something (+ 1 1)]
    (doseq [x x]
      (let [x2 (* x x)]))
    something))"
  (search-backward "(+ 1 1")
  (clojure--move-to-let-internal "something"))

;; clojure-emacs/clj-refactor.el#30
(def-refactor-test test-move-to-let-already-inside-let-binding-1
  "(deftest retrieve-order-body-test
  (let [item (get-in (retrieve-order-body order-item-response-str))]))"
  "(deftest retrieve-order-body-test
  (let [something (retrieve-order-body order-item-response-str)
        item (get-in something)]))"
  (search-backward "(retrieve")
  (clojure--move-to-let-internal "something"))

;; clojure-emacs/clj-refactor.el#30
(def-refactor-test test-move-to-let-already-inside-let-binding-2
  "(let [parent (.getParent (io/file root adrf))
      builder (string-builder)
      normalize-path (comp (partial path/relative-to root)
                           path/->normalized
                           foobar)]
  (do-something-spectacular parent builder))"
  "(let [parent (.getParent (io/file root adrf))
      builder (string-builder)
      something (partial path/relative-to root)
      normalize-path (comp something
                           path/->normalized
                           foobar)]
  (do-something-spectacular parent builder))"
  (search-backward "(partial")
  (clojure--move-to-let-internal "something"))

(provide 'clojure-mode-refactor-let-test)

;;; clojure-mode-refactor-let-test.el ends here
