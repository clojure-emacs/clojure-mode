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

(defvar clojure--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'clojure
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'clojure
   '((str_lit) @font-lock-string-face)

   :feature 'number
   :language 'clojure
   :override t
   '((num_lit) @font-lock-number-face)

   ;; The rest of the features are not working yet :), I'm still experimenting and have no idea how this works
   :feature 'function-name
   :language 'clojure
   :override t
   '((function_definition ;; not this easy, first detect `def` nodes and then match the correct symbols.
      name: (sym_lit) @font-lock-function-name-face))

   :feature 'constant
   :language 'clojure
   :override t
   '([(bool_lit) (nil_lit)] @font-lock-constant-face)
   
   :feature 'bracket
   :language 'clojure
   :override t
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   ))

(define-derived-mode clojure-ts-mode prog-mode "Clojure"
  (when (treesit-ready-p 'clojure)
    (treesit-parser-create 'clojure)
    (setq-local treesit-font-lock-settings clojure--treesit-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string keyword builtin constant bracket number function-name)))
    (treesit-major-mode-setup)
    (message "Clojure Treesit Mode")))

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))
