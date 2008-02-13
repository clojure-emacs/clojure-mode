;;; clojure-mode.el -- Major mode for Clojure code

;; Copyright (C) 2007, 2008 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

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


(defvar clojure-load-command "(load-file \"%s\")\n"
  "*Format-string for building a Clojure expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Clojure expression that will command the inferior Clojure
to load that file."
;;  :type 'string :group 'clojure-mode
)


(defvar clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e\C-x" 'lisp-eval-defun)
    (define-key map "\C-x\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-l" 'clojure-load-file)
    (define-key map "\C-c\C-r" 'lisp-eval-region)
    (define-key map "\C-c\C-z" 'run-lisp)
    map)
  "Keymap for ordinary Clojure mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")


(easy-menu-define clojure-menu clojure-mode-map "Menu used in `clojure-mode'."
                  '("Clojure"
                    ["Eval defun"        lisp-eval-defun         t]
                    ["Eval defun and go" lisp-eval-defun-and-go  t]
                    ["Eval last sexp"    lisp-eval-last-sexp     t]
                    ["Eval region"       lisp-eval-region        t]
                    ["Eval region and go" lisp-eval-region-go    t]
                    ["Load file..."      clojure-load-file       t]
                    ["Run Lisp"          run-lisp                t]))


(defvar clojure-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?~ "'   " table)
    (modify-syntax-entry ?, "    " table)
    table))


(defvar clojure-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `clojure-load-file' or `clojure-compile-file' command.")


(defun clojure-mode ()
  "Major mode for editing Clojure code - similar to Lisp mode..
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map clojure-mode-map)
  (setq major-mode 'clojure-mode)
  (setq mode-name "Clojure")
  (lisp-mode-variables)
  (set-syntax-table clojure-mode-syntax-table)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (make-local-variable 'lisp-indent-function)
  (setq lisp-indent-function 'clojure-indent-function)
  (setq font-lock-defaults
	'(clojure-font-lock-keywords
	  nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
	  (font-lock-mark-block-function . mark-defun)
	  (font-lock-syntactic-face-function
	   . lisp-font-lock-syntactic-face-function)))
  (run-mode-hooks 'clojure-mode-hook))


(defconst clojure-font-lock-keywords
  (eval-when-compile
    `( ;; Definitions.
      (,(concat "(\\(def"
		;; Function declarations.
		"\\(n\\|multi\\|macro\\|method\\|"
		;; Variable declarations.
                ""
		"\\)\\)\\>"
		;; Any whitespace
		"[ \t]*"
                ;; Possibly type
                "\\(?:#^\\sw+[ \t]*\\)?"
                "\\(\\sw+\\)?")
        (1 font-lock-keyword-face)
        (3 font-lock-function-name-face nil t))
      ;; Control structures
      (,(concat
         "(\\(?:clojure/\\)?" 
         (regexp-opt
          '("cond" "for" "if" "loop" "let" "recur" "do" "binding" "with-meta" "when"
            "when-not" "delay" "lazy-cons" "." ".." "->" "and" "or" "locking"
            "sync" "doseq" "dotimes" "import" "unimport" "in-ns" "refer"
            "implement" "time" "try" "catch" "finally"
            "doto" "with-open" "with-local-vars" ) t)
         "\\>")
        .  1)
      ;; (fn name? args ...)
      (,(concat "(\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face nil t))
      ;; Constant values.
      ("\\<:\\sw+\\>" 0 font-lock-builtin-face)
      ;; Meta type annotation #^Type
      ("#^\\sw+" 0 font-lock-type-face)
      ))
  "Default expressions to highlight in Clojure mode.")


(defun clojure-load-file (file-name)
  "Load a Lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load Clojure file: " clojure-prev-l/c-dir/file
				  '(clojure-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq clojure-prev-l/c-dir/file (cons (file-name-directory file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-lisp-proc)
		      (format clojure-load-command file-name))
  (switch-to-lisp t))



(defun clojure-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function
which has a non-nil property `lisp-indent-function',
that specifies how to do the indentation.  The property value can be
* `defun', meaning indent `defun'-style;
* an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
* a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
		(progn (goto-char calculate-lisp-indent-last-sexp)
		       (beginning-of-line)
		       (parse-partial-sexp (point)
					   calculate-lisp-indent-last-sexp 0 t)))
	    ;; Indent under the list or under the first sexp on the same
	    ;; line as calculate-lisp-indent-last-sexp.  Note that first
	    ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (get (intern-soft function) 'clojure-indent-function))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method indent-point state)))))))


(put 'loop 'clojure-indent-function 1)
(put 'for 'clojure-indent-function 1)   ; FIXME (for seqs expr) and (for seqs filter expr)
(put 'do 'clojure-indent-function 0)
(put 'let 'clojure-indent-function 1)
(put 'if 'clojure-indent-function 1)
(put 'when 'clojure-indent-function 1)
(put 'when-not 'clojure-indent-function 1)
(put 'locking 'clojure-indent-function 1)
(put 'defmulti 'clojure-indent-function 1)
(put 'binding 'clojure-indent-function 1)
(put 'sync 'clojure-indent-function 1)
(put 'doseq 'clojure-indent-function 2)
(put 'dotimes 'clojure-indent-function 2)
(put 'implement 'clojure-indent-function 1)
(put 'doto 'clojure-indent-function 1)
(put 'with-open 'clojure-indent-function 2)
(put 'with-local-vars 'clojure-indent-function 1)



(provide 'clojure-mode)

;;; clojure-mode.el ends here
