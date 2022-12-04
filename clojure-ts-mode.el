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

(defface clojure-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Clojure keywords (:something).")

(defface clojure-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Clojure character literals.")

(defvar clojure--definition-keyword-regexp
  (rx
   (or (group line-start (or "ns" "fn") line-end)
       (group "def"
              (+ (or alnum
                     ;; What are valid characters for symbols? is a negative match better?
                     "-" "_" "!" "@" "#" "$" "%" "^" "&" "*" "|" "?" "<" ">" "+" "=" ":"))))))

(defvar clojure--variable-keyword-regexp
  (rx line-start (or "def" "defonce") line-end))

(defvar clojure--type-keyword-regexp
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
   '((str_lit) @font-lock-string-face)

   :feature 'number
   :language 'clojure
   :override t
   '((num_lit) @font-lock-number-face)

   :feature 'constant
   :language 'clojure
   '([(bool_lit) (nil_lit)] @font-lock-constant-face)

   :feature 'char
   :language 'clojure
   '((char_lit) @clojure-character-face)

   :feature 'definition
   :language 'clojure
   ;:override t ;; need to override str_lit for font-lock-doc-face
   `(((list_lit :anchor (sym_lit) @font-lock-keyword-face
                :anchor (sym_lit) @font-lock-function-name-face)
      (:match ,clojure--definition-keyword-regexp
              @font-lock-keyword-face)))

   :feature 'variable
   :language 'clojure
   ;:override t ;; override definition
   `(((list_lit :anchor (sym_lit) @font-lock-keyword-face
                :anchor (sym_lit) @font-lock-variable-name-face)
      (:match ,clojure--variable-keyword-regexp @font-lock-keyword-face)))

   :feature 'type
   :language 'clojure
   ;:override t
   `(((list_lit :anchor (sym_lit) @font-lock-keyword-face
                :anchor (sym_lit) @font-lock-type-face)
      (:match ,clojure--type-keyword-regexp @font-lock-keyword-face)))

   :feature 'metadata
   :language 'clojure
   :override t
   `((meta_lit marker: "^" @font-lock-property-face) ;; just the ^
     (meta_lit marker: "^" @font-lock-property-face ;; typehint, or metadata shorthand
               value: [(sym_lit) (kwd_lit)] @font-lock-property-face)
     (old_meta_lit marker: "#^" @font-lock-property-face) ;; just the #^
     (old_meta_lit marker: "#^" @font-lock-property-face ;;
                   value: [(sym_lit) (kwd_lit)] @font-lock-property-face))

   ;; Possible to combine this with declaration??
   ;; docstrings are optional, and not part of every definition form
   :feature 'doc
   :language 'clojure
   :override t
   `(((list_lit :anchor (sym_lit) @declaration
                :anchor (sym_lit) @name
                :anchor (str_lit) @font-lock-doc-face)
      (:match ,clojure--declaration-regexp @declaration)))

   ;; :namespace/keyword is highlighted  with the namespace as font-lock-type-face
   ;; and the name clojure-keyword-face
   ;; I believe in order to do this, the grammer will have to be updated to provide these "fields"
   :feature 'keyword
   :language 'clojure
   '((kwd_lit) @clojure-keyword-face)
  
   ;; :feature 'quote
   ;; :language 'clojure
   ;; '((quoting-lit
   ;;    marker: "'" @font-lock-delimiter-face)
   ;;   (syn-quoting-lit
   ;;    marker: "`" @font-lock-delimiter-face)
   ;;   (unquoting-lit
   ;;    marker: "~" @font-lock-delimiter-face)
   ;;   (unquote-splicing-lit
   ;;    marker "~@" @font-lock-delimiter-face))

   :feature 'bracket
   :language 'clojure
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :feature 'comment
   :language 'clojure
   :override t
   '((comment) @font-lock-comment-face
     (dis_expr
      marker: "#_" @default-face
      value: _ @font-lock-comment-face)
     ((list_lit :anchor (sym_lit) @font-lock-comment-delimiter-face)
      (:match "^comment$" @font-lock-comment-delimiter-face)))

   :feature 'deref ;; not part of clojure-mode, but a cool idea?
   :language 'clojure
   '((derefing_lit
      marker: "@" @font-lock-warning-face))
   ))

(define-derived-mode clojure-ts-mode prog-mode "Clojure"
  (when (treesit-ready-p 'clojure)
    (treesit-parser-create 'clojure)
    (setq-local treesit-font-lock-settings clojure--treesit-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string char bracket)
                  (keyword constant symbol number)
                  (deref quote metadata definition variable type doc))))
    (treesit-major-mode-setup)
    (message "Clojure Treesit Mode"))

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))
