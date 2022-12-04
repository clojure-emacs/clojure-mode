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

(defvar clojure--declaration-regexp
  (rx
   (or (group line-start (or "ns" "fn") line-end)
       (group "def"
              (* (or alnum
                     ;; What are valid characters for symbols? is a negative match better?
                     "-" "_" "!" "@" "#" "$" "%" "^" "&" "*" "|" "?" "<" ">" "+" "=" ":"))))))

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

   :feature 'declaration
   :language 'clojure
   :override t ;; need to override str_lit for font-lock-doc-face
   `(((list_lit :anchor (sym_lit) @font-lock-keyword-face
                :anchor (sym_lit) @font-lock-type-face)
      (:match ,clojure--declaration-regexp
              @font-lock-keyword-face)))

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

   :feature 'comment
   :language 'clojure
   '((comment)  @font-lock-comment-face)

   :feature 'expression-comment
   :language 'clojure
   :override t
   '((dis_expr
      marker: "#_" @default-face
      value: _ @font-lock-comment-face))

   :feature 'bracket
   :language 'clojure
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)
   ))

(define-derived-mode clojure-ts-mode prog-mode "Clojure"
  (when (treesit-ready-p 'clojure)
    (treesit-parser-create 'clojure)
    (setq-local treesit-font-lock-settings clojure--treesit-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string char bracket)
                  (keyword constant symbol number)
                  (deref declaration doc expression-comment))))
    (treesit-major-mode-setup)
    (message "Clojure Treesit Mode"))

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))
