;;; clojure-test-mode --- Minor mode for Clojure tests

;; Copyright (C) 2009 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://emacswiki.org/cgi-bin/wiki/ClojureTestMode
;; Version: 0.1
;; Keywords: languages, lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides support for running Clojure tests (using the
;; test-is framework) via SLIME and seeing feedback in the test buffer
;; about which tests passed and which failed or errored.

;;; Installation:

;; (0) Add this file to your load-path, usually the ~/.emacs.d directory.
;; (1) Either:
;;     Add these lines to your .emacs:
;;      (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;;      (add-hook 'clojure-mode-hook
;;                (lambda () (save-excursion
;;                        (goto-char (point-min))
;;                        (if (search-forward "(deftest" nil t)
;;                            (clojure-test-mode)))))
;;     Or generate autoloads with the `update-directory-autoloads' function.

;;; Code:

(require 'clojure-mode)
(require 'slime)

(defvar clojure-test-results nil
  "An alist containing the results of the last test run for each test.")

;; Support Functions

(defun clojure-test-eval (string handler)
  (slime-eval-async `(swank:eval-and-grab-output ,string) handler))

(defun clojure-test-focused-test ()
  (save-excursion
    (end-of-line)
    (search-backward-regexp "(deftest \\(.*\\)")
    (match-string 1)))

(defun clojure-test-load-reporting ()
  "Redefine the test-is report function to store results in metadata."
  (slime-repl-eval-string
   "(use 'clojure.contrib.test-is)
(ns clojure.contrib.test-is)
(defonce old-report report)
(defn report [event msg expected actual]
  (if-let [current-test (last *testing-vars*)]
          (alter-meta! current-test
                       assoc :status [event msg (str expected) (str actual)
                                            ((file-position 2) 1)]))
  (old-report event msg expected actual))"));; embedded clojure broke font-lock"

(defun clojure-test-get-results (result)
  (clojure-test-eval "(map #(cons (str (:name (meta %))) (:status (meta %))) (vals (ns-interns 'clojure.contrib.test-is.test)))"
                     #'clojure-test-extract-results))

(defun clojure-test-extract-results (results)
  ;; slime-eval-async hands us a cons with a useless car
  (mapcar #'clojure-test-extract-result (read (cadr results))))

(defun clojure-test-extract-result (result)
  (destructuring-bind (test event msg expected actual line) result
    (let ((message (format "Expected %s, got %s" expected actual)))
      (unless (equal :pass event)
        (clojure-test-highlight-problem line event message)))))

(defun clojure-test-highlight-problem (line event message)
  (save-excursion
    (goto-line line)
    (set-mark-command nil)
    (end-of-line)
    (let ((overlay (make-overlay (mark) (point))))
      (overlay-put overlay 'face '(background-color . "red"))
      (overlay-put overlay 'message message))))

;; Commands

(defun clojure-test-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  (remove-overlays)
  (setq clojure-test-results nil)
  (slime-load-file (buffer-file-name))
  ;; (format "(run-tests '%s)" (slime-current-package))
  (clojure-test-eval "(run-tests)" #'clojure-test-get-results))

;; (defun clojure-test-run-focused-test ()
;;   "Run the test under point."
;;   ;; TODO: this doesn't work.
;;   (interactive)
;;   (slime-load-file (buffer-file-name))
;;   (slime-interactive-eval (format "(test-var #'%s)" (clojure-test-focused-test)))
;;   (clojure-test-get-results))

(defun clojure-test-show-result ()
  "Show the result of the test under point."
  (interactive)
  (message (overlay-get (car (overlays-at (point))) 'message)))

(defvar clojure-test-mode-map (make-keymap))

;;;###autoload
(define-minor-mode clojure-test-mode
  "A minor mode for running Clojure tests."
  (clojure-test-load-reporting)
  (slime-mode t))

(define-key clojure-test-mode-map (kbd "C-c C-,") 'clojure-test-run-tests)
(define-key clojure-test-mode-map (kbd "C-c ,") 'clojure-test-run-focused-test)
(define-key clojure-test-mode-map (kbd "C-c '") 'clojure-test-show-result)

;;;###autoload
(add-hook 'clojure-mode-hook
          (lambda () (save-excursion
                  (goto-char (point-min))
                  (if (search-forward "(deftest" nil t)
                      (clojure-test-mode t)))))

(provide 'clojure-test-mode)

