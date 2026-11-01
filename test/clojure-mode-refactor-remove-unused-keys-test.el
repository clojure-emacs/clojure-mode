;;; clojure-mode-refactor-remove-unused-keys-test.el --- Clojure Mode: remove unused keys test  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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

;; Tests for `clojure-remove-unused-keys'.

;;; Code:

(require 'clojure-mode)
(require 'buttercup)
(require 'test-helper "test/utils/test-helper")

(describe "clojure-remove-unused-keys"
  (before-each
    (spy-on 'message))

  (when-refactoring-it "removes the unused names of :keys, :strs and :syms"
    "(defn f [{:keys [a b] :strs [c d] :syms [e g]}] (+ b d g))"
    "(defn f [{:keys [b] :strs [d] :syms [g]}] (+ b d g))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "goes by the name of a qualified symbol or a keyword"
    "(defn f [{:keys [x/a x/b :c :d] :foo/keys [e g]} #:bar{:keys [h i]}] (+ b d g i))"
    "(defn f [{:keys [x/b :d] :foo/keys [g]} #:bar{:keys [i]}] (+ b d g i))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "removes a name bound to a key, and an :as name"
    "(defn f [{x :x y :y :keys [a] :as m}] (+ x a))"
    "(defn f [{x :x :keys [a]}] (+ x a))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "removes the :or defaults of the names it removes"
    "(defn f [{:keys [a b c] :or {a 1 b 2}}] (+ b c))"
    "(defn f [{:keys [b c] :or {b 2}}] (+ b c))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "turns a map with nothing used into _, but keeps a used :as"
    "(defn f [{:keys [a b] :or {a 1}} {:keys [c] :as m} & {:keys [d] :as opts}] [m opts])"
    "(defn f [_ {:as m} & {:as opts}] [m opts])"
    (clojure-remove-unused-keys))

  (when-refactoring-it "drops what binds nothing along with the rest of a map"
    "(defn f [{:keys [] :as m} {:keys [a] :or {}} {:keys [b] :or {b 1 c 2}}] c)"
    "(defn f [_ _ _] c)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "leaves alone what binds nothing to begin with"
    "(defn f [{} {:as m}] m)"
    "(defn f [{} {:as m}] m)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "removes a name another one's :or default used once that one goes"
    "(defn f [{:keys [a b] :or {b a}} {:keys [c d] :or {d c}}] d)"
    "(defn f [_ {:keys [c d] :or {d c}}] d)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "removes a binding form in an :or default along with it"
    "(defn f [{:keys [a b] :or {a (fn [{:keys [x y]}] y)}}] b)"
    "(defn f [{:keys [b]}] b)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "handles nested maps"
    "(defn f [{{:keys [a b]} :x {:keys [c]} :y {:keys [d] :as z} :z}] (+ b z))"
    "(defn f [{{:keys [b]} :x {:as z} :z}] (+ b z))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "handles the maps in vectors, but not the rest of the vectors"
    "(defn f [[a {:keys [b c]}] & {:keys [d e]}] (+ c e))"
    "(defn f [[a {:keys [c]}] & {:keys [e]}] (+ c e))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "handles each arity on its own"
    "(defn f
  ([{:keys [a b]}] b)
  ([{:keys [a b]} c] (+ a c)))"
    "(defn f
  ([{:keys [b]}] b)
  ([{:keys [a]} c] (+ a c)))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "handles the bindings of let-like forms"
    "(let [{:keys [a b]} (g)
      {:keys [c]} (h)]
  (for [x b
        :let [{:keys [d e]} x]
        :when e]
    (loop [{:keys [f g]} e]
      (recur g))))"
    "(let [{:keys [b]} (g)
      _ (h)]
  (for [x b
        :let [{:keys [e]} x]
        :when e]
    (loop [{:keys [g]} e]
      (recur g))))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "handles if-let, when-first and the like"
    "(if-let [{:keys [a b]} (g)] b (when-first [{:keys [c d]} xs] d))"
    "(if-let [{:keys [b]} (g)] b (when-first [{:keys [d]} xs] d))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "handles defmethod, whose dispatch value can be a vector"
    "(defmethod f [:a :b] [{:keys [a b]}] b)"
    "(defmethod f [:a :b] [{:keys [b]}] b)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "handles the methods of reify, defrecord and the like, and letfn"
    "(defrecord R [x]
  P
  (m [this {:keys [a b]}] (+ x b))
  (n [this {:keys [c]}]
    (letfn [(g [{:keys [d e]}] e)]
      (reify Q (o [_ {:keys [h i]}] (g i))))))"
    "(defrecord R [x]
  P
  (m [this {:keys [b]}] (+ x b))
  (n [this _]
    (letfn [(g [{:keys [e]}] e)]
      (reify Q (o [_ {:keys [i]}] (g i))))))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "doesn't take a method named like a core form for one"
    "(letfn [(loop [{:keys [a b]}] b)
        (for [{:keys [c d] :as m}] d)]
  (reify P (fn [this {:keys [e g]}] (loop (for g)))))"
    "(letfn [(loop [{:keys [b]}] b)
        (for [{:keys [d]}] d)]
  (reify P (fn [this {:keys [g]}] (loop (for g)))))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "leaves alone protocol signatures, quoted forms and macro templates"
    "(defmacro m [{:keys [a b]}]
  '(let [{:keys [c]} x] 1)
  `(let [{:keys [d]} x] ~b))
(defprotocol P (n [this {:keys [e]}]) (fn [this {:keys [f]}]))"
    "(defmacro m [{:keys [b]}]
  '(let [{:keys [c]} x] 1)
  `(let [{:keys [d]} x] ~b))
(defprotocol P (n [this {:keys [e]}]) (fn [this {:keys [f]}]))"
    (goto-char (point-min))
    (clojure-remove-unused-keys)
    (goto-char (point-max))
    (clojure-remove-unused-keys))

  (when-refactoring-it "keeps a name that appears anywhere in its scope"
    "(defn f [{:keys [a b c d]}]
  {:pre [(pos? a)]}
  (let [b 1] (+ b (.-x c.y)))
  #(d %))"
    "(defn f [{:keys [a b c d]}]
  {:pre [(pos? a)]}
  (let [b 1] (+ b (.-x c.y)))
  #(d %))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "leaves alone a name marked unused with a leading _"
    "(defn f [{:keys [a _b] :as _m} {_c :c}] a)"
    "(defn f [{:keys [a _b] :as _m} {_c :c}] a)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "doesn't count a name in a keyword, a string, a comment or a qualified symbol"
    "(defn f [{:keys [a b c d]}]
  ;; a
  [:b \"c\" x/d])"
    "(defn f [_]
  ;; a
  [:b \"c\" x/d])"
    (clojure-remove-unused-keys))

  (when-refactoring-it "keeps discarded forms and reader conditionals"
    "(defn f [{:keys [a #_b #?(:clj c)]}] 1)"
    "(defn f [{:keys [#_b #?(:clj c)]}] 1)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "keeps the discarded forms and comments among the names it removes"
    "(defn f [{:keys [a #_x c d]} {:keys [e
                                     ;; TODO
                                     g h]}]
  (+ d h))"
    "(defn f [{:keys [#_x d]} {:keys [;; TODO
                                 h]}]
  (+ d h))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "doesn't take what's left with a discarded form or a comment for empty"
    "(defn f [{:keys [a #_b]}
         {:keys [c] :or {c 1 #_d}}
         {:keys [e] ; e is next
          }]
  1)"
    "(defn f [{:keys [#_b]}
         {:or {#_d}}
         {; e is next
          }]
  1)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "leaves alone bindings it can't pair up, because of #?@"
    "(defn f [{:keys [a] #?@(:clj [:as m])}]
  (let [#?@(:clj [x 1]) {:keys [b]} {:b 2}] 1))"
    "(defn f [{:keys [a] #?@(:clj [:as m])}]
  (let [#?@(:clj [x 1]) {:keys [b]} {:b 2}] 1))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "removes the metadata of a name with it"
    "(defn f [{:keys [^String a ^long b]}] b)"
    "(defn f [{:keys [^long b]}] b)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "removes the comments and discarded forms after a name's metadata with it"
    "(defn f [{:keys [x a ^long ;; c
                 b]}
         {:keys [y c ^long #_z d]}
         {:keys [e ^long #_z g h]}]
  (+ x y h))"
    "(defn f [{:keys [x]}
         {:keys [y]}
         {:keys [h]}]
  (+ x y h))"
    (clojure-remove-unused-keys))

  (when-refactoring-it "keeps the layout, and comments"
    "(defn f [{:keys [a
                 b] ; the bs
          :as m
          :or {a 1
               b 2}}]
  b)"
    "(defn f [{:keys [b] ; the bs
          :or {b 2}}]
  b)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "doesn't put a closing bracket in a comment"
    "(defn f [{:keys [a ; the as
                 b]}]
  a)"
    "(defn f [{:keys [a ; the as
                 ]}]
  a)"
    (clojure-remove-unused-keys))

  (when-refactoring-it "doesn't reindent the line the bindings start on"
    "(defn f [x]
    (foo x)
    (let [{:keys [a b]} x] b))"
    "(defn f [x]
    (foo x)
    (let [{:keys [b]} x] b))"
    (clojure-remove-unused-keys))

  (when-refactoring-with-point-it "works on the top-level form at point, and keeps point"
    "(defn f [{:keys [a b]}] b)
(defn g [{:keys [c d]}] (+ |d 1))"
    "(defn f [{:keys [a b]}] b)
(defn g [{:keys [d]}] (+ |d 1))"
    (clojure-remove-unused-keys))

  (when-refactoring-with-point-it "works with point at the start of a form after a comment"
    "(defn f [{:keys [a b]}] b)
;; g
|(defn g [{:keys [c d]}] d)"
    "(defn f [{:keys [a b]}] b)
;; g
|(defn g [{:keys [d]}] d)"
    (clojure-remove-unused-keys))

  (when-refactoring-with-point-it "works with point on the metadata of the top-level form"
    "|^:private ^{:a 1} #_x
;; why
(defn f [{:keys [a b]}] b)"
    "|^:private ^{:a 1} #_x
;; why
(defn f [{:keys [b]}] b)"
    (clojure-remove-unused-keys))

  (it "says what it removed"
    (with-clojure-buffer "(defn f [{:keys [a b] :as m}] 1)"
      (clojure-remove-unused-keys)
      (expect 'message :to-have-been-called-with "Removed %s" "a, b, m")
      (clojure-remove-unused-keys)
      (expect 'message :to-have-been-called-with "Nothing unused to remove")))

  (it "refuses a form that doesn't parse"
    (dolist (text '("(defn f [{:keys [a b]}] (g b)"
                    "^:private (defn f [{:keys [a b]}] (g b)"
                    "#_(defn f [{:keys [a b]}] (g b)"))
      (with-clojure-buffer text
        (goto-char 1)
        (expect (clojure-remove-unused-keys) :to-throw 'user-error)
        (expect (buffer-string) :to-equal text)))
    (with-clojure-buffer "(defn f [{:keys [a b]}] b))"
      (expect (clojure-remove-unused-keys) :to-throw 'user-error)))

  (it "signals an error when point isn't on a form"
    (with-clojure-buffer "(defn f [{:keys [a]}] 1)\n\n"
      (expect (clojure-remove-unused-keys) :to-throw 'user-error))
    (with-clojure-buffer ";; c\n(defn f [{:keys [a]}] 1)"
      (goto-char 2)
      (expect (clojure-remove-unused-keys) :to-throw 'user-error))))

;;; clojure-mode-refactor-remove-unused-keys-test.el ends here
