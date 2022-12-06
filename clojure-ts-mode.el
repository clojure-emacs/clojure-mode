;; Clojure tree-sitter proof of concept
;; Clojure tree-sitter grammer used is here: git@github.com:sogaiu/tree-sitter-clojure.git
;; Currently requires Emacs built from the master branch
;;
;;
;; Follow the instructions found in the emacs source to build tree-sitter getting started guide
;; to build the tree-sitter  grammers
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide
;;
;; To summarize:
;; install tree sitter (package manager, or from source) then
;;; $ git clone git@github.com:casouri/tree-sitter-module.git
;; Apply the ./tree-sitter-module-clojure-support.patch (found in this repo) to that repository, then run
;;; $ ./batch.sh
;; Change this next line to point to the newly built dist directory built from the patched `tree-sitter-module` repo
(setq treesit-extra-load-path '( "~/dev/tree-sitter-module/dist"))

;; Then evaluate the rest of this file
;; Open up test.clj to see it in action.
;; Try `M-x treesit-explore-mode` from the clojure buffer to examine the parse tree
;;
;; semi-colon comments, strings, and numbers working right now, nothing else

(require 'treesit)
(unless (treesit-available-p)
  (message "Tree sitter is broke"))

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

(eval-and-compile
  (defconst clojure--ts-sym-forbidden-ns-part-chars "][\";@\\^`~\(\)\{\}\\,\s\t\n\r\/"
    "A list of chars that a Clojure symbol cannot contain.
See definition of `macros': URL `http://git.io/vRGLD'.

This is distinct from `'clojure--sym-forbidden-rest-chars' in clojure-mode
because it also forbids the `/' character.")

  (defconst clojure--namespaced-keyword-regexp
    (concat "^\\(::?\\)\\([^" clojure--ts-sym-forbidden-ns-part-chars "]*\\)/")
    "A regex matching namespaced keywords.
Captures the forward `:' or `::' marker in group 1, and the namespace in group 2.")

  (defconst clojure--namespaced-symbol-regexp
    (concat "^\\([^" clojure--ts-sym-forbidden-ns-part-chars "]*\\)/")
    "A regex matching namespaced symbols.
Captures the namespaced portion of the symbol in group1."))

(defun clojure-ts-mode--fontify-keyword  (node override start end &rest _)
  "Fontify keywords, distinguishing between the namespace and name parts.
For NODE, OVERRIDE, START, and END, see `treesit-font-lock-rules'."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node))
        (kw-text (treesit-node-text node t)))
    (treesit-fontify-with-override start end 'clojure-keyword-face override)
    (when (string-match clojure--namespaced-keyword-regexp kw-text)
      (let* ((marker (match-string 1 kw-text))
             (namespace (match-string 2 kw-text))
             (ns-start (+ start (length marker)))
             (ns-end (+ ns-start (length namespace))))
        ;; The namespace
        (treesit-fontify-with-override ns-start ns-end 'font-lock-type-face t)
        ;; The / delimiter
        (treesit-fontify-with-override ns-end (+ 1 ns-end) 'default t)))))

(defun clojure--ts-node-at-function-position-p (node)
  "Return nil if NODE is not in the function position of its parent."
  (treesit-node-eq node (treesit-node-child (treesit-node-parent node) 1)))

(defun clojure-ts-mode--fontify-symbol  (node override start end &rest _)
  "Fontify symbols, distinguishing between the namespace and name parts.
For NODE, OVERRIDE, START, and END, see `treesit-font-lock-rules'."
  (let ((sym-text (treesit-node-text node t))
        (start (treesit-node-start node)))
    (if (string-match clojure--namespaced-symbol-regexp sym-text)
        (let* ((namespace (match-string 1 sym-text))
               (ns-end (+ start (length namespace))))
          (treesit-fontify-with-override start ns-end 'font-lock-type-face override))
      ;; TODO: consider highlighting everyting in a function position
      ;; I don't think we normally do this actually.
      ;; Instead, we'll highlight just specific "builtin" keywords, those defined in clojure.core
      ;; This option still exists though, matchs any symbol in a function position basically.
      ;; (when (and
      ;;        (equal "list_lit" (treesit-node-type (treesit-node-parent node)))
      ;;        (clojure--ts-node-at-function-position-p node))
      ;;   (let ((end (treesit-node-end node)))
      ;;     (treesit-fontify-with-override start end 'font-lock-keyword-face override)))
      )))

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
   '((kwd_lit) @clojure-ts-mode--fontify-keyword)

   :feature 'builtin
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face))
      (:match ,clojure-ts-mode--builtin-symbol-regexp  @font-lock-keyword-face))
     ((sym_name) @font-lock-builtin-face
      (:match ,clojure-ts-mode--builtin-dynamic-var-regexp @font-lock-builtin-face)))

   :feature 'symbol
   :language 'clojure
   '((sym_name) @clojure-ts-mode--fontify-symbol)

   :feature 'definition ;; defn and defn like macros
   :language 'clojure
   ;:override t ;; need to override str_lit for font-lock-doc-face
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-function-name-face))
      (:match ,clojure--definition-keyword-regexp
              @font-lock-keyword-face))
     ((anon_fn_lit
       marker: "#" @font-lock-property-face)))

   :feature 'variable ;; def, defonce
   :language 'clojure
   ; :override t
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-variable-name-face))
      (:match ,clojure--variable-keyword-regexp @font-lock-keyword-face)))

   :feature 'type ;; deftype, defmulti, defprotocol, etc
   :language 'clojure
   ; :override t
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

   ;; Possible to combine this with declaration??
   ;; docstrings are optional, and not part of every definition form
   :feature 'doc
   :language 'clojure
   :override t
   `(((list_lit :anchor (sym_lit) @declaration
                :anchor (sym_lit) @name
                :anchor (str_lit) @font-lock-doc-face)
      (:match ,clojure--definition-keyword-regexp @declaration)))

   ;; :feature 'quote
   ;; :language 'clojure
   ;; '((quoting_lit
   ;;    marker: _ @font-lock-delimiter-face)
   ;;   (var_quoting_lit
   ;;    marker: _ @font-lock-delimiter-face)
   ;;   (syn_quoting_lit
   ;;    marker: _ @font-lock-delimiter-face)
   ;;   (unquoting_lit
   ;;    marker: _ @font-lock-delimiter-face)
   ;;   (unquote_splicing_lit
   ;;    marker _ @font-lock-delimiter-face)
   ;;   (var_quoting_lit marker: _ @font-lock-property-face))

   :feature 'bracket
   :language 'clojure
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :feature 'comment
   :language 'clojure
   :override t
   '((comment) @font-lock-comment-face
     (dis_expr
      marker: "#_" @default
      value: _ @font-lock-comment-face)
     ((list_lit :anchor (sym_lit (sym_name) @font-lock-comment-delimiter-face))
      (:match "^comment$" @font-lock-comment-delimiter-face)))

   :feature 'deref ;; not part of clojure-mode, but a cool idea?
   :language 'clojure
   '((derefing_lit
      marker: "@" @font-lock-warning-face))))

(define-derived-mode clojure-ts-mode prog-mode "Clojure"
  (when (treesit-ready-p 'clojure)
    (treesit-parser-create 'clojure)
    (setq-local treesit-font-lock-settings clojure--treesit-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string char bracket)
                  (keyword constant symbol number builtin)
                  (deref quote metadata definition variable type doc regex))))
    (treesit-major-mode-setup)
    (message "Clojure Treesit Mode"))

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))


;; (let ((query
;;        `((meta_lit marker: "^" @font-lock-property-face)
;;          (meta_lit value: (kwd_lit) @font-lock-property-face) ;; metadata
;;          (meta_lit value: (sym_lit) @font-type-type-face) ;; typehin
;;          ;; OLD metadata sugar #^
;;          (old_meta_lit marker: "#^" @font-lock-property-face)
;;          (old_meta_lit value: (kwd_lit) @font-lock-property-face) ;; metadata
;;          (old_meta_lit value: (sym_lit) @font-type-type-face) ;; typehint
;;          )))

;;   (treesit-query-string "(def #^:type sym 1 )" query 'clojure)
;;   )
