;;; clojure-mode-indentation-test.el --- Clojure Mode: indentation tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Bozhidar Batsov <bozhidar@batsov.dev>

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

(defmacro when-indenting-with-point-it (description before after)
  "Return a buttercup spec.

Check whether the swift indentation command will correctly change the buffer.
Will also check whether point is moved to the expected position.

BEFORE is the buffer string before indenting, where a pipe (|) represents
point.

AFTER is the expected buffer string after indenting, where a pipe (|)
represents the expected position of point.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  `(it ,description
    (let* ((after ,after)
           (clojure-indent-style 'always-align)
           (expected-cursor-pos (1+ (cl-position ?| after)))
           (expected-state (delete ?| after)))
      (with-clojure-buffer ,before
        (goto-char (point-min))
        (search-forward "|")
        (delete-char -1)
        (font-lock-ensure)
        (indent-according-to-mode)
        (expect (buffer-string) :to-equal expected-state)
        (expect (point) :to-equal expected-cursor-pos)))))

;; Backtracking indent
(defmacro when-indenting-it (description &optional style &rest forms)
  "Return a buttercup spec.

Check that all FORMS correspond to properly indented sexps.

STYLE allows overriding the default clojure-indent-style 'always-align.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  (when (stringp style)
    (setq forms (cons style forms))
    (setq style '(quote always-align)))
  `(it ,description
     (progn
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,form);,(replace-regexp-in-string "\n +" "\n " form))
                      (let ((clojure-indent-style ,style))
                        (indent-region (point-min) (point-max)))
                      (expect (buffer-string) :to-equal ,(concat "\n" form))))
                 forms))))

(defmacro when-aligning-it (description &rest forms)
  "Return a buttercup spec.

Check that all FORMS correspond to properly indented sexps.

DESCRIPTION is a string with the description of the spec."
  (declare (indent defun))
  `(it ,description
     (let ((clojure-align-forms-automatically t)
           (clojure-align-reader-conditionals t))
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,(replace-regexp-in-string " +" " " form))
                      (indent-region (point-min) (point-max))
                      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                                     ,(concat "\n" form)))))
                 forms))
     (let ((clojure-align-forms-automatically nil))
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-mode)
                      (insert "\n" ,(replace-regexp-in-string " +" " " form))
                      ;; This is to check that we did NOT align anything. Run
                      ;; `indent-region' and then check that no extra spaces
                      ;; where inserted besides the start of the line.
                      (indent-region (point-min) (point-max))
                      (goto-char (point-min))
                      (should-not (search-forward-regexp "\\([^\s\n]\\)  +" nil 'noerror))))
                 forms))))

;; Provide font locking for easier test editing.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group "when-indenting-with-point-it") eow)
    (1 font-lock-keyword-face))
   (,(rx "("
         (group "when-indenting-with-point-it") (+ space)
         (group bow (+ (not space)) eow)
         )
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))

(describe "indentation"
  (it "should not hang on end of buffer"
    (with-clojure-buffer "(let [a b]"
      (goto-char (point-max))
      (expect
       (with-timeout (2)
         (newline-and-indent)
         t))))

  (when-indenting-with-point-it "should have no indentation at top level"
    "|x"

    "|x")

  (when-indenting-with-point-it "should indent cond"
    "
    (cond
    |x)"

    "
    (cond
      |x)")

  (when-indenting-with-point-it "should indent cond-> with a namespaced map"
    "
(cond-> #:a{:b 1}
|x 1)"

    "
(cond-> #:a{:b 1}
  |x 1)")

  (when-indenting-with-point-it "should indent cond-> with a namespaced map 2"
    "
(cond-> #::a{:b 1}
|x 1)"

    "
(cond-> #::a{:b 1}
  |x 1)")

  (when-indenting-with-point-it "should indent threading macro with expression on first line"
    "
    (->> expr
     |ala)"

    "
    (->> expr
         |ala)")

  (when-indenting-with-point-it "should indent threading macro with expression on second line"
    "
    (->>
    |expr)"

    "
    (->>
     |expr)")

  (when-indenting-with-point-it "should not indent for def string"
    "(def foo \"hello|\")"
    "(def foo \"hello|\")")

  (when-indenting-with-point-it "should indent doc strings"
    "
    (defn some-fn
    |\"some doc string\")"
    "
    (defn some-fn
      |\"some doc string\")")

  (when-indenting-with-point-it "should not indent doc strings when correct indent already specified"
    "
    (defn some-fn
      |\"some doc string\")"
    "
    (defn some-fn
      |\"some doc string\")")

  (when-indenting-with-point-it "should handle doc strings with additional indent specified"
    "
    (defn some-fn
      |\"some doc string
        - some note\")"
    "
    (defn some-fn
      |\"some doc string
        - some note\")")

  (describe "specify different indentation for symbol with some ns prefix"
    (put-clojure-indent 'bala 0)
    (put-clojure-indent 'ala/bala 1)

    (when-indenting-with-point-it "should handle a symbol without ns"
      "
      (bala
      |one)"
      "
      (bala
        |one)")

    (when-indenting-with-point-it "should handle a symbol with ns"
      "
      (ala/bala top
      |one)"
      "
      (ala/bala top
        |one)"))

  (describe "specify an indentation for symbol"
    (put-clojure-indent 'cala 1)

    (when-indenting-with-point-it "should handle a symbol with ns"
      "
      (cala top
      |one)"
      "
      (cala top
        |one)")
    (when-indenting-with-point-it "should handle special arguments"
      "
      (cala
       |top
        one)"
      "
      (cala
          |top
        one)"))
  (describe "should respect special argument indentation"
    (before-each
     (setq clojure-special-arg-indent-factor 1))
    (after-each
     (setq clojure-special-arg-indent-factor 2))

    (put-clojure-indent 'cala 1)

    (when-indenting-with-point-it "should handle a symbol with ns"
      "
      (cala top
      |one)"
      "
      (cala top
        |one)")
    (when-indenting-with-point-it "should handle special arguments"
      "
      (cala
       |top
        one)"
      "
      (cala
        |top
        one)"))

  (describe "we can pass a lambda to explicitly set the column"
    (put-clojure-indent 'arsymbol (lambda (_indent-point _state) 0))

    (when-indenting-with-point-it "should handle a symbol with lambda"
      "
(arsymbol
|one)"
      "
(arsymbol
|one)"))

  (when-indenting-with-point-it "should indent a form with metadata"
    "
    (ns ^:doc app.core
    |(:gen-class))"
    "
    (ns ^:doc app.core
      |(:gen-class))")

  (when-indenting-with-point-it "should handle multiline sexps"
    "
    [[
      2] a
    |x]"
    "
    [[
      2] a
     |x]")

  (when-indenting-with-point-it "should indent reader conditionals"
    "
    #?(:clj :foo
    |:cljs :bar)"
    "
    #?(:clj :foo
       |:cljs :bar)")

  (when-indenting-with-point-it "should handle backtracking with aliases"
    "
    (clojure.core/letfn [(twice [x]
    |(* x 2))]
      :a)"
    "
    (clojure.core/letfn [(twice [x]
                           |(* x 2))]
      :a)")

  (when-indenting-with-point-it "should handle fixed-normal-indent"
    "
     (cond
      (or 1
          2) 3
    |:else 4)"

    "
     (cond
      (or 1
          2) 3
      |:else 4)")

  (when-indenting-with-point-it "should handle fixed-normal-indent-2"
    "
(fact {:spec-type
       :charnock-column-id} #{\"charnock\"}
|{:spec-type
       :charnock-column-id} #{\"current_charnock\"})"

    "
(fact {:spec-type
       :charnock-column-id} #{\"charnock\"}
      |{:spec-type
       :charnock-column-id} #{\"current_charnock\"})")

  (when-indenting-it "closing-paren"
    "
(ns ca
  (:gen-class)
  )")

  (when-indenting-it "default-is-not-a-define"
    "
(default a
         b
         b)"
    "
(some.namespace/default a
                        b
                        b)")


  (when-indenting-it "should handle extend-type with multiarity"
    "
(extend-type Banana
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))"

    "
(extend-protocol Banana
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")


  (when-indenting-it "should handle deftype with multiarity"
    "
(deftype Banana []
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")

  (when-indenting-it "should handle defprotocol"
    "
(defprotocol IFoo
  (foo [this]
    \"Why is this over here?\")
  (foo-2
    [this]
    \"Why is this over here?\"))")


  (when-indenting-it "should handle definterface"
    "
(definterface IFoo
  (foo [this]
    \"Why is this over here?\")
  (foo-2
    [this]
    \"Why is this over here?\"))")

  (when-indenting-it "should handle specify"
    "
(specify obj
  ISwap
  (-swap!
    ([this f] (reset! this (f @this)))
    ([this f a] (reset! this (f @this a)))
    ([this f a b] (reset! this (f @this a b)))
    ([this f a b xs] (reset! this (apply f @this a b xs)))))")

  (when-indenting-it "should handle specify!"
    "
(specify! obj
  ISwap
  (-swap!
    ([this f] (reset! this (f @this)))
    ([this f a] (reset! this (f @this a)))
    ([this f a b] (reset! this (f @this a b)))
    ([this f a b xs] (reset! this (apply f @this a b xs)))))")

  (when-indenting-it "should handle non-symbol at start"
    "
{\"1\" 2
 *3 4}")

  (when-indenting-it "should handle non-symbol at start 2"
    "
(\"1\" 2
 *3 4)")

  (when-indenting-it "should handle defrecord"
    "
(defrecord TheNameOfTheRecord
    [a pretty long argument list]
  SomeType
  (assoc [_ x]
    (.assoc pretty x 10)))")

  (when-indenting-it "should handle defrecord 2"
    "
(defrecord TheNameOfTheRecord [a pretty long argument list]
  SomeType (assoc [_ x]
             (.assoc pretty x 10)))")

  (when-indenting-it "should handle defrecord with multiarity"
    "
(defrecord Banana []
  Fruit
  (subtotal
    ([item]
     (* 158 (:qty item)))
    ([item a]
     (* a (:qty item)))))")

  (when-indenting-it "should handle letfn"
    "
(letfn [(f [x]
          (* x 2))
        (f [x]
          (* x 2))]
  (a b
     c) (d)
  e)")

  (when-indenting-it "should handle reify"
    "
(reify Object
  (x [_]
    1))"

    "
(reify
  om/IRender
  (render [this]
    (let [indent-test :fail]
      ...))
  om/IRender
  (render [this]
    (let [indent-test :fail]
      ...)))")

  (when-indenting-it "proxy"
    "
(proxy [Writer] []
  (close [] (.flush ^Writer this))
  (write
    ([x]
     (with-out-binding [out messages]
       (.write out x)))
    ([x ^Integer off ^Integer len]
     (with-out-binding [out messages]
       (.write out x off len))))
  (flush []
    (with-out-binding [out messages]
      (.flush out))))")

  (when-indenting-it "should handle defmethod"
    "
(defmethod foo :bar
  [x]
  (println x))"

    "
(defmethod foo :default
  [x y]
  (+ x y))")

  (when-indenting-it "should handle multi-arity defn"
    "
(defn foo
  ([x]
   (foo x 1))
  ([x y]
   (+ x y)))"

    "
(defn foo
  \"docstring\"
  ([x]
   (foo x 1))
  ([x y]
   (+ x y)))"

    "
(defn ^:private foo
  ([x]
   (foo x 1))
  ([x y]
   (+ x y)))")

  (when-indenting-it "should handle nested letfn"
    "
(letfn [(foo [x]
          (let [y (inc x)]
            (* y y)))
        (bar [a]
          (letfn [(baz [b]
                    (+ a b))]
            (baz 1)))]
  (foo (bar 10)))")

  (when-indenting-it "should handle try/catch/finally"
    "
(try
  (dangerous)
  (catch Exception e
    (println e))
  (finally
    (cleanup)))")

  (when-indenting-it "should handle as->"
    "
(as-> x $
  (inc $)
  (* $ 2))")

  (when-indenting-it "should indent with-* forms like body even without explicit specs"
    "
(with-open [f (io/reader \"x\")]
  (slurp f))"

    "
(with-redefs [foo bar]
  (test-stuff))"

    "
(with-custom-thing [x y]
  (body x y))")

  (when-indenting-it "should handle condp"
    "
(condp = x
  1 \"one\"
  2 \"two\"
  \"other\")")

  (when-indenting-it "should handle namespace-qualified special forms"
    "
(clojure.core/let [x 1]
  (inc x))"

    "
(clojure.core/when true
  (do-stuff))"

    "
(clojure.core/defn foo
  [x]
  (inc x))")

  (when-indenting-it "should indent unknown def forms like body"
    "
(defwhatever my-thing
  :some-option true
  :another false)"

    "
(my.ns/defwhatever my-thing
  :some-option true
  :another false)")

  (when-indenting-it "should handle fn"
    "
(fn [x]
  (inc x))"

    "
(fn my-fn [x]
  (inc x))"

    "
(fn
  ([x]
   (inc x))
  ([x y]
   (+ x y)))")

  (when-indenting-it "should handle def"
    "
(def x
  (+ 1 2))"

    "
(def ^:dynamic *x*
  42)")

  (when-indenting-it "should handle bound-fn"
    "
(bound-fn [x]
  (inc x))")

  (when-indenting-it "should handle if"
    "
(if (even? x)
  (inc x)
  (dec x))")

  (when-indenting-it "should handle if-not"
    "
(if-not (nil? x)
  (use x)
  (default))")

  (when-indenting-it "should handle case"
    "
(case x
  :a 1
  :b 2
  3)")

  (when-indenting-it "should handle when"
    "
(when (pos? x)
  (println x)
  (inc x))")

  (when-indenting-it "should handle when-not"
    "
(when-not (nil? x)
  (println x))")

  (when-indenting-it "should handle when-first"
    "
(when-first [x xs]
  (println x))")

  (when-indenting-it "should handle while"
    "
(while (pos? @counter)
  (swap! counter dec))")

  (when-indenting-it "should handle do"
    "
(do
  (println 1)
  (println 2))")

  (when-indenting-it "should handle delay"
    "
(delay
  (expensive-computation))")

  (when-indenting-it "should handle future"
    "
(future
  (long-running-task))")

  (when-indenting-it "should handle comment"
    "
(comment
  (foo 1 2)
  (bar 3 4))")

  (when-indenting-it "should handle binding"
    "
(binding [*out* writer]
  (println \"hello\"))")

  (when-indenting-it "should handle loop"
    "
(loop [i 0]
  (when (< i 10)
    (recur (inc i))))")

  (when-indenting-it "should handle for"
    "
(for [x (range 10)
      :when (even? x)]
  (* x x))")

  (when-indenting-it "should handle doseq"
    "
(doseq [x xs]
  (println x))")

  (when-indenting-it "should handle dotimes"
    "
(dotimes [i 10]
  (println i))")

  (when-indenting-it "should handle when-let"
    "
(when-let [x (foo)]
  (bar x))")

  (when-indenting-it "should handle if-let"
    "
(if-let [x (foo)]
  (bar x)
  (baz))")

  (when-indenting-it "should handle when-some"
    "
(when-some [x (foo)]
  (bar x))")

  (when-indenting-it "should handle if-some"
    "
(if-some [x (foo)]
  (bar x)
  (baz))")

  (when-indenting-it "should handle doto"
    "
(doto (java.util.HashMap.)
  (.put \"a\" 1)
  (.put \"b\" 2))")

  (when-indenting-it "should handle locking"
    "
(locking obj
  (alter-state! obj))")

  (when-indenting-it "should handle fdef"
    "
(fdef my-fn
  :args (s/cat :x int?)
  :ret int?)")

  (when-indenting-it "should handle this-as"
    "
(this-as self
  (.method self))")

  ;; clojure.test
  (when-indenting-it "should handle testing"
    "
(testing \"some feature\"
  (is (= 1 1)))")

  (when-indenting-it "should handle deftest"
    "
(deftest my-test
  (is (= 1 1)))")

  (when-indenting-it "should handle are"
    "
(are [x y] (= x y)
  1 1
  2 2)")

  (when-indenting-it "should handle use-fixtures"
    "
(use-fixtures :each
  my-fixture)")

  (when-indenting-it "should handle async"
    "
(async done
  (do-stuff)
  (done))")

  ;; core.logic
  (when-indenting-it "should handle run"
    "
(run [q]
  (== q 1))")

  (when-indenting-it "should handle run*"
    "
(run* [q]
  (== q 1))")

  (when-indenting-it "should handle fresh"
    "
(fresh [a b]
  (== a 1)
  (== b 2))")

  ;; core.async
  (when-indenting-it "should handle go"
    "
(go
  (<! ch)
  (println \"done\"))")

  (when-indenting-it "should handle go-loop"
    "
(go-loop [x 0]
  (>! ch x)
  (recur (inc x)))")

  (when-indenting-it "should handle thread"
    "
(thread
  (blocking-op))")

  (when-indenting-it "should handle alt!"
    "
(alt!
  ch1 ([v] (println v))
  ch2 ([v] (println v)))")

  (when-indenting-it "should handle alt!!"
    "
(alt!!
  ch1 ([v] (println v))
  ch2 ([v] (println v)))")

  (when-indenting-it "should handle reader conditionals"
    "#?@ (:clj []
     :cljs [])")

  (when-indenting-it "should handle an empty close paren"
    "
(let [x]
  )"

    "
(ns ok
  )"

    "
(ns ^{:zen :dikar}
    ok
  )")

  (when-indenting-it "should handle unfinished sexps"
    "
(letfn [(tw [x]
          dd")

  (when-indenting-it "should handle symbols ending in crap"
    "
(msg? ExceptionInfo
      10)"

    "
(thrown-with-msg? ExceptionInfo
                  #\"Storage must be initialized before use\"
                  (f))"

    "
(msg' 1
      10)")

  (when-indenting-it "should handle let, when and while forms"
    "(let-alist [x 1]\n  ())"
    "(while-alist [x 1]\n  ())"
    "(when-alist [x 1]\n  ())"
    "(if-alist [x 1]\n  ())"
    "(indents-like-fn-when-let-while-if-are-not-the-start [x 1]\n                                                     ())")

(defun indent-cond (indent-point state)
  (goto-char (elt state 1))
  (let ((pos -1)
        (base-col (current-column)))
    (forward-char 1)
    ;; `forward-sexp' will error if indent-point is after
    ;; the last sexp in the current sexp.
    (condition-case nil
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (clojure-forward-logical-sexp 1)
          (cl-incf pos))
      ;; If indent-point is _after_ the last sexp in the
      ;; current sexp, we detect that by catching the
      ;; `scan-error'. In that case, we should return the
      ;; indentation as if there were an extra sexp at point.
      (scan-error (cl-incf pos)))
    (+ base-col (if (cl-evenp pos) 0 2))))
(put-clojure-indent 'test-cond #'indent-cond)

(defun indent-cond-0 (_indent-point _state) 0)
(put-clojure-indent 'test-cond-0 #'indent-cond-0)


  (when-indenting-it "should handle function spec"
    "
(when me
  (test-cond
    x
  1
    2
  3))"

    "
(when me
  (test-cond-0
x
1
2
3))")

  (when-indenting-it "should respect indent style 'align-arguments"
    'align-arguments

    "
(some-function
  10
  1
  2)"

    "
(some-function 10
               1
               2)")

  (when-indenting-it "should respect indent style 'always-indent"
    'always-indent

    "
(some-function
  10
  1
  2)"

    "
(some-function 10
  1
  2)")

  (when-aligning-it "should basic forms"
    "
{:this-is-a-form b
 c               d}"

    "
{:this-is b
 c        d}"

    "
{:this b
 c     d}"

    "
{:a b
 c  d}"

    "
(let [this-is-a-form b
      c              d])"

    "
(let [this-is b
      c       d])"

    "
(let [this b
      c    d])"

    "
(let [a b
      c d])")

  (when-aligning-it "should handle a blank line"
    "
(let [this-is-a-form b
      c              d

      another form
      k       g])"

    "
{:this-is-a-form b
 c               d

 :another form
 k        g}")

  (when-aligning-it "should handle basic forms (reversed)"
    "
{c               d
 :this-is-a-form b}"
  "
{c        d
 :this-is b}"
  "
{c     d
 :this b}"
  "
{c  d
 :a b}"

  "
(let [c              d
      this-is-a-form b])"

  "
(let [c       d
      this-is b])"

  "
(let [c    d
      this b])"

  "
(let [c d
      a b])")

  (when-aligning-it "should handle incomplete sexps"
    "
(cond aa b
      casodkas )"

    "
(cond aa b
      casodkas)"

    "
(cond aa b
      casodkas "

    "
(cond aa b
      casodkas"

    "
(cond aa       b
      casodkas a)"

    "
(cond casodkas a
      aa       b)"

    "
(cond casodkas
      aa b)")


  (when-aligning-it "should handle multiple words"
    "
(cond this     is    just
      a        test  of
      how      well
      multiple words will work)")

  (when-aligning-it "should handle nested maps"
    "
{:a    {:a    :a
        :bbbb :b}
 :bbbb :b}")

  (when-aligning-it "should regard end as a marker"
    "
{:a {:a                                :a
     :aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa :a}
 :b {:a  :a
     :aa :a}}")

  (when-aligning-it "should handle trailing commas"
    "
{:a {:a  :a,
     :aa :a},
 :b {:a  :a,
     :aa :a}}")

  (when-aligning-it "should handle standard reader conditionals"
    "
#?(:clj  2
   :cljs 2)")

  (when-aligning-it "should handle splicing reader conditional"
    "
#?@(:clj  [2]
    :cljs [2])")

  (when-aligning-it "should handle sexps broken up by line comments"
    "
(let [x  1
      ;; comment
      xx 1]
  xx)"

    "
{:x   1
 ;; comment
 :xxx 2}"

    "
(case x
  :aa 1
  ;; comment
  :a  2)")
 
  (when-aligning-it "should work correctly when margin comments appear after nested, multi-line, non-terminal sexps"
    "
(let [x  {:a 1
          :b 2} ; comment
      xx 3]
  x)"

    "
{:aa {:b  1
      :cc 2} ;; comment
 :a  1}}"

    "
(case x
  :a  (let [a  1
            aa (+ a 1)]
       aa); comment
  :aa 2)")

  (it "should handle improperly indented content"
    (let ((content "(let [a-long-name 10\nb 20])")
          (aligned-content "(let [a-long-name 10\n      b           20])"))
      (with-clojure-buffer content
        (call-interactively #'clojure-align)
        (expect (buffer-string) :to-equal aligned-content))))

  (it "should not align reader conditionals by default"
    (let ((content "#?(:clj 2\n   :cljs 2)"))
      (with-clojure-buffer content
        (call-interactively #'clojure-align)
        (expect (buffer-string) :to-equal content))))

  (it "should align reader conditionals when clojure-align-reader-conditionals is true"
    (let ((content "#?(:clj 2\n   :cljs 2)"))
      (with-clojure-buffer content
        (setq-local clojure-align-reader-conditionals t)
        (call-interactively #'clojure-align)
        (expect (buffer-string) :not :to-equal content))))

  (it "should remove extra commas"
    (with-clojure-buffer "{:a 2, ,:c 4}"
      (call-interactively #'clojure-align)
      (expect (string= (buffer-string) "{:a 2, :c 4}")))))

(describe "clojure--valid-indent-spec-p"
  (it "should accept integers"
    (expect (clojure--valid-indent-spec-p '1) :to-be-truthy))

  (it "should accept :defn and :form keywords"
    (expect (clojure--valid-indent-spec-p ':defn) :to-be-truthy)
    (expect (clojure--valid-indent-spec-p ':form) :to-be-truthy))

  (it "should accept quoted list specs"
    (expect (clojure--valid-indent-spec-p '(quote (2 :form :form (1)))) :to-be-truthy))

  (it "should accept nested specs like letfn's ((:defn))"
    (expect (clojure--valid-indent-spec-p '(quote (1 ((:defn)) nil))) :to-be-truthy))

  (it "should accept nil as a valid spec element"
    (expect (clojure--valid-indent-spec-p '(quote (1))) :to-be-truthy)
    (expect (clojure--valid-indent-spec-p '(quote (:defn))) :to-be-truthy)))

(describe "clojure--valid-put-clojure-indent-call-p"
  (it "should accept letfn-style indent spec"
    (expect (clojure--valid-put-clojure-indent-call-p
             '(put-clojure-indent 'letfn '(1 ((:defn)) nil)))
            :to-be-truthy))

  (it "should accept simple indent specs"
    (expect (clojure--valid-put-clojure-indent-call-p
             '(put-clojure-indent 'defrecord '(2 :form :form (1))))
            :to-be-truthy))

  (it "should accept keyword indent specs"
    (expect (clojure--valid-put-clojure-indent-call-p
             '(put-clojure-indent 'fn :defn))
            :to-be-truthy))

  (it "should reject invalid specs"
    (expect (clojure--valid-put-clojure-indent-call-p
             '(put-clojure-indent 'foo "bar"))
            :to-throw)))

(describe "clojure-indent-keyword-style"
  (it "should align keyword forms with always-align (default)"
    (let ((clojure-indent-keyword-style 'always-align))
      ;; Case A: arg on same line → align with it
      (with-clojure-buffer "\n(ns foo\n(:require\n[bar]))"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(ns foo\n  (:require\n   [bar]))"))
      ;; Case B: no arg on same line → align with keyword
      (with-clojure-buffer "\n(ns foo\n(:require [bar]\n[baz]))"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(ns foo\n  (:require [bar]\n            [baz]))"))))

  (it "should indent keyword forms with always-indent"
    (let ((clojure-indent-keyword-style 'always-indent))
      ;; Case A: arg on same line → still indented like body
      (with-clojure-buffer "\n(ns foo\n(:require [bar]\n[baz]))"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(ns foo\n  (:require [bar]\n    [baz]))"))
      ;; Case B: no arg on same line → indented like body
      (with-clojure-buffer "\n(ns foo\n(:require\n[bar]))"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(ns foo\n  (:require\n    [bar]))"))))

  (it "should indent keyword forms with align-arguments"
    (let ((clojure-indent-keyword-style 'align-arguments))
      ;; Case A: arg on same line → align with first arg
      (with-clojure-buffer "\n(ns foo\n(:require [bar]\n[baz]))"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(ns foo\n  (:require [bar]\n            [baz]))"))
      ;; Case B: no arg on same line → indented like body
      (with-clojure-buffer "\n(ns foo\n(:require\n[bar]))"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(ns foo\n  (:require\n    [bar]))")))))

(describe "clojure-use-backtracking-indent"
  (it "should still indent simple specs correctly when disabled"
    (let ((clojure-use-backtracking-indent nil))
      ;; Integer spec (when has spec 1)
      (with-clojure-buffer "\n(when true\nbody)"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(when true\n  body)"))
      ;; :defn spec
      (with-clojure-buffer "\n(defn foo\n[x]\nx)"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(defn foo\n  [x]\n  x)"))))

  (it "should lose context for complex specs when disabled"
    (let ((clojure-use-backtracking-indent nil))
      ;; Without backtracking, the body of a letfn binding won't get
      ;; :defn-style indentation because the backtracking that walks
      ;; up to letfn's spec (1 ((:defn)) nil) is disabled.
      (with-clojure-buffer "\n(letfn [(foo [x]\n(+ x 1))]\n(foo 1))"
        (indent-region (point-min) (point-max))
        ;; The body of foo should NOT get :defn-style (2-space) indent
        ;; relative to foo — instead it gets default alignment.
        (let ((result (buffer-string)))
          ;; Just verify it differs from the backtracking result
          (expect result :not :to-equal "\n(letfn [(foo [x]\n          (+ x 1))]\n  (foo 1))"))))))

(describe "clojure-max-backtracking"
  (it "should limit how far up the sexp tree backtracking goes"
    ;; With max-backtracking = 0, even one level of backtracking is
    ;; disabled, so letfn bindings lose :defn-style indentation.
    (let ((clojure-max-backtracking 0))
      (with-clojure-buffer "\n(letfn [(foo [x]\n(+ x 1))]\n(foo 1))"
        (indent-region (point-min) (point-max))
        (let ((result (buffer-string)))
          (expect result :not :to-equal "\n(letfn [(foo [x]\n          (+ x 1))]\n  (foo 1))")))))

  (it "should indent correctly with sufficient backtracking depth"
    ;; With the default depth (3), letfn works fine.
    (let ((clojure-max-backtracking 3))
      (with-clojure-buffer "\n(letfn [(foo [x]\n(+ x 1))]\n(foo 1))"
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "\n(letfn [(foo [x]\n          (+ x 1))]\n  (foo 1))")))))

(provide 'clojure-mode-indentation-test)

;;; clojure-mode-indentation-test.el ends here
