;;; clojure-ts-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;;; Commentary:
;; Clojure tree-sitter proof of concept
;; Clojure tree-sitter grammer used is here: git@github.com:sogaiu/tree-sitter-clojure.git
;; Currently requires Emacs built from the emacs-29 branch
;;
;; Follow the instructions found in the Emacs source to build tree-sitter getting started guide
;;
;; To build the tree-sitter grammers:
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide
;;
;; To summarize, run:
;;; $ git clone git@github.com:casouri/tree-sitter-module.git
;; Apply the ./tree-sitter-module-clojure-support.patch (found in this repo) to that repository, then run:
;;; $ ./batch.sh
;; Change this next line to point to the newly built dist directory built from the patched `tree-sitter-module` repo
;; (setq treesit-extra-load-path '( "~/dev/tree-sitter-module/dist"))

;; Then evaluate the rest of this file
;; Open up test.clj to see it in action.
;; Try `M-x treesit-explore-mode` from the clojure buffer to examine the parse tree

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'clojure-mode)
(require 'treesit)

(defconst clojure-ts-mode--builtin-dynamic-var-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt ;; TODO: taken from clojure-font-lock-keywords, needs refactor to share defs
             '("*1" "*2" "*3" "*agent*"
               "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
               "*command-line-args*" "*compile-files*"
               "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
               "*e" "*err*" "*file*" "*flush-on-newline*"
               "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
               "*print-dup*" "*print-length*" "*print-level*"
               "*print-meta*" "*print-readably*"
               "*read-eval*" "*source-path*"
               "*unchecked-math*"
               "*use-context-classloader*" "*warn-on-reflection*"))
            "$")))

(defconst clojure-ts-mode--builtin-symbol-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt ;; TODO: taken from clojure-font-lock-keywords, needs refactor to share defs
             '("do"
               "if"
               "let*"
               "var"
               "fn"
               "fn*"
               "loop*"
               "recur"
               "throw"
               "try"
               "catch"
               "finally"
               "set!"
               "new"
               "."
               "monitor-enter"
               "monitor-exit"
               "quote"

               "->"
               "->>"
               ".."
               "amap"
               "and"
               "areduce"
               "as->"
               "assert"
               "binding"
               "bound-fn"
               "case"
               "comment"
               "cond"
               "cond->"
               "cond->>"
               "condp"
               "declare"
               "delay"
               "doall"
               "dorun"
               "doseq"
               "dosync"
               "dotimes"
               "doto"
               "extend-protocol"
               "extend-type"
               "for"
               "future"
               "gen-class"
               "gen-interface"
               "if-let"
               "if-not"
               "if-some"
               "import"
               "in-ns"
               "io!"
               "lazy-cat"
               "lazy-seq"
               "let"
               "letfn"
               "locking"
               "loop"
               "memfn"
               "ns"
               "or"
               "proxy"
               "proxy-super"
               "pvalues"
               "refer-clojure"
               "reify"
               "some->"
               "some->>"
               "sync"
               "time"
               "vswap!"
               "when"
               "when-first"
               "when-let"
               "when-not"
               "when-some"
               "while"
               "with-bindings"
               "with-in-str"
               "with-loading-context"
               "with-local-vars"
               "with-open"
               "with-out-str"
               "with-precision"
               "with-redefs"
               "with-redefs-fn"))
            "$")))

(defface clojure-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Clojure keywords (:something).")

(defface clojure-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Clojure character literals.")

(defconst clojure--definition-keyword-regexp
  (rx
   (or (group line-start (or "ns" "fn") line-end)
       (group "def"
              (+ (or alnum
                     ;; What are valid characters for symbols? is a negative match better?
                     "-" "_" "!" "@" "#" "$" "%" "^" "&" "*" "|" "?" "<" ">" "+" "=" ":"))))))

(defconst clojure--variable-keyword-regexp
  (rx line-start (or "def" "defonce") line-end))

(defconst clojure--type-keyword-regexp
  (rx line-start (or "defprotocol"
                     "defmulti"
                     "deftype"
                     "defrecord"
                     "definterface"
                     "defmethod"
                     "defstruct")
      line-end))

(defvar clojure--treesit-settings
  (treesit-font-lock-rules
   :feature 'string
   :language 'clojure
   '((str_lit) @font-lock-string-face
     (regex_lit) @font-lock-string-face)

   :feature 'regex
   :language 'clojure
   :override t
   '((regex_lit marker: _ @font-lock-property-face))

   :feature 'number
   :language 'clojure
   '((num_lit) @font-lock-number-face)

   :feature 'constant
   :language 'clojure
   '([(bool_lit) (nil_lit)] @font-lock-constant-face)

   :feature 'char
   :language 'clojure
   '((char_lit) @clojure-character-face)

   ;; :namespace/keyword is highlighted  with the namespace as font-lock-type-face
   ;; and the name clojure-keyword-face
   ;; I believe in order to do this, the grammer will have to be updated to provide these "fields"
   :feature 'keyword
   :language 'clojure
   '((kwd_ns) @font-lock-type-face
     (kwd_name) @clojure-keyword-face
     (kwd_lit
       marker: _ @clojure-keyword-face
       delimiter: _ :? @default))

   :feature 'builtin
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face))
      (:match ,clojure-ts-mode--builtin-symbol-regexp  @font-lock-keyword-face))
     ((sym_name) @font-lock-builtin-face
      (:match ,clojure-ts-mode--builtin-dynamic-var-regexp @font-lock-builtin-face)))

   :feature 'symbol
   :language 'clojure
   '((sym_ns) @font-lock-type-face
     ;; (sym_name) @default
     ;; (sym_lit delimiter: _ :? @default)
     )

   ;; How does this work for defns nested in other forms, not at the top level?
   ;; Should I match against the source node to only hit the top level? Can that be expressed?
   ;; What about valid usages like `(let [closed 1] (defn +closed [n] (+ n closed)))'??
   ;; No wonder the tree-sitter-clojure grammar only touches syntax, and not semantics
   :feature 'definition ;; defn and defn like macros
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-function-name-face))
      (:match ,clojure--definition-keyword-regexp
              @font-lock-keyword-face))
     ((anon_fn_lit
       marker: "#" @font-lock-property-face)))

   :feature 'variable ;; def, defonce
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-variable-name-face))
      (:match ,clojure--variable-keyword-regexp @font-lock-keyword-face)))

   :feature 'type ;; deftype, defmulti, defprotocol, etc
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-type-face))
      (:match ,clojure--type-keyword-regexp @font-lock-keyword-face)))

   :feature 'metadata
   :language 'clojure
   :override t
   `((meta_lit marker: "^" @font-lock-property-face)
     (meta_lit value: (kwd_lit) @font-lock-property-face) ;; metadata
     (meta_lit value: (sym_lit (sym_name) @font-lock-type-face)) ;; typehint
     (old_meta_lit marker: "#^" @font-lock-property-face)
     (old_meta_lit value: (kwd_lit) @font-lock-property-face) ;; metadata
     (old_meta_lit value: (sym_lit (sym_name) @font-lock-type-face))) ;; typehint

   :feature 'tagged-literals
   :language 'clojure
   :override t
   '((tagged_or_ctor_lit marker: "#" @font-lock-preprocessor-face
                         tag: (sym_lit) @font-lock-preprocessor-face))

   ;; TODO, also account for `def'
   ;; Figure out how to highlight symbols in docstrings.
   :feature 'doc
   :language 'clojure
   :override t
   `(((list_lit :anchor (sym_lit) @def_symbol
                :anchor (sym_lit) @function_name
                :anchor (str_lit) @font-lock-doc-face)
      (:match ,clojure--definition-keyword-regexp @def_symbol)))

   :feature 'quote
   :language 'clojure
   '((quoting_lit
      marker: _ @font-lock-delimiter-face)
     (var_quoting_lit
      marker: _ @font-lock-delimiter-face)
     (syn_quoting_lit
      marker: _ @font-lock-delimiter-face)
     (unquoting_lit
      marker: _ @font-lock-delimiter-face)
     (unquote_splicing_lit
      marker: _ @font-lock-delimiter-face)
     (var_quoting_lit
      marker: _ @font-lock-delimiter-face))

   :feature 'bracket
   :language 'clojure
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face
     (set_lit :anchor "#" @font-lock-bracket-face))

   :feature 'comment
   :language 'clojure
   :override t
   '((comment) @font-lock-comment-face
     (dis_expr
      marker: "#_" @font-lock-comment-delimiter-face
      value: _ @font-lock-comment-face)
     ((list_lit :anchor (sym_lit (sym_name) @font-lock-comment-delimiter-face))
      (:match "^comment$" @font-lock-comment-delimiter-face)))

   :feature 'deref ;; not part of clojure-mode, but a cool idea?
   :language 'clojure
   '((derefing_lit
      marker: "@" @font-lock-warning-face))))

(defvar clojure-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    map))

;;;###autoload
(define-derived-mode clojure-ts-mode prog-mode "Clojure[TS]"
  "Major mode for editing Clojure code.
Requires Emacs 29 and libtree-sitter-clojure.so available somewhere in
`treesit-extra-load-path'.

\\{clojure-ts-mode-map}"
  (when (treesit-ready-p 'clojure)
    (treesit-parser-create 'clojure)
    (setq-local treesit-font-lock-settings clojure--treesit-settings)
    (setq-local treesit-defun-prefer-top-level t
                treesit-defun-tactic 'top-level
                treesit-defun-type-regexp (rx (or "list_lit" "vec_lit" "map_lit")))
    (setq-local treesit-font-lock-feature-list
                '((comment string char number)
                  (keyword constant symbol bracket builtin)
                  (deref quote metadata definition variable type doc regex tagged-literals)))
    (treesit-major-mode-setup)
    ;; TODO: revisit these, what can be replaced with treesiter
    (clojure-mode-variables)
    (add-hook 'paredit-mode-hook #'clojure-paredit-setup)
    ;; TODO: setup treesit indentation
    (add-hook 'electric-indent-function #'clojure-mode--electric-indent-function)))

;; We won't autoload this, users can opt in by requiring this library or adding
;; clojure-ts-mode to (auto|interpreter)-mode-alist
(progn
  (add-hook 'clojure-ts-mode-hook 'treesit-inspect-mode)
  (add-to-list 'auto-mode-alist
               '("\\.\\(clj\\|cljd\\|dtm\\|edn\\)\\'" . clojure-ts-mode))
  ;; TODO: Create clojurec-ts-mode and clojurescript-ts-mode
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-ts-mode))
  ;; boot build scripts are Clojure source files
  (add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-ts-mode))
  ;; babashka scripts are Clojure source files
  (add-to-list 'interpreter-mode-alist '("bb" . clojure-ts-mode))
  ;; nbb scripts are ClojureScript source files
  (add-to-list 'interpreter-mode-alist '("nbb" . clojure-ts-mode)))

(provide 'clojure-ts-mode)

;;; clojure-ts-mode.el ends here
