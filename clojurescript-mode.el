;;; clojurescript-mode.el --- Major mode for ClojureScript code

;; Copyright (C) 2011 Luke Amdor
;;
;; Authors: Luke Amdor <luke.amdor@gmail.com>
;; URL: http://github.com/rubbish/clojurescript-mode
;; Version: 1.3.0-beta
;; Keywords: languages, lisp, javascript

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides an REPL to the
;; ClojureScript language. (http://github.com/clojure/clojurescript)

;; For information on how to start up the REPL correctly see
;; https://github.com/clojure/clojurescript/tree/master/samples/repl
;; and
;; https://github.com/clojure/clojurescript/wiki/The-REPL-and-Evaluation-Environments

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

(require 'clojure-mode)

(defvar clojurescript-home
  (getenv "CLOJURESCRIPT_HOME")
  "Path to ClojureScript home directory")

(defvar clojurescript-clj-repl
  (expand-file-name "script/repl" clojurescript-home)
  "Path to the ClojureScript Clojure REPL")

(defvar clojurescript-repl-interface
  "cljs.repl.browser"
  "Which ClojureScript REPL interface to use")

(defun clojurescript-repl-init-commands ()
  (concat "(require 'cljs.repl)" "\n"
          "(require '" clojurescript-repl-interface ")" "\n"
          "(cljs.repl/repl (" clojurescript-repl-interface "/repl-env)" ")" "\n"))

(defun clojurescript-start-cljs-repl ()
  (comint-send-string (inferior-lisp-proc) (clojurescript-repl-init-commands)))

(define-derived-mode clojurescript-mode clojure-mode "ClojureScript"
  "Major mode for ClojureScript"
  
  (set (make-local-variable 'inferior-lisp-program) clojurescript-clj-repl)
  (add-hook 'inferior-lisp-mode-hook 'clojurescript-start-cljs-repl))

(add-hook 'clojurescript-mode-hook '(lambda () (slime-mode -1)))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(provide 'clojurescript-mode)
