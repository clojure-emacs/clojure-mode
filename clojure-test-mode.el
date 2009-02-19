;;; clojure-test-mode --- Minor mode for Clojure tests

;; Copyright (C) 2009 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://emacswiki.org/cgi-bin/wiki/ClojureTestMode
;; Version: 0.2
;; Keywords: languages, lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides support for running Clojure tests (using the
;; test-is framework) via SLIME and seeing feedback in the test buffer
;; about which tests failed or errored.

;;; Installation:

;; (0) Add this file to your load-path, usually the ~/.emacs.d directory.
;; (1) Either:
;;     Add these lines to your .emacs:
;;      (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;;      (add-hook 'clojure-mode-hook
;;                (lambda () (save-excursion
;;                        (goto-char (point-min))
;;                        (if (or (search-forward "(deftest" nil t)
;;                                (search-forward "(with-test" nil t))
;;                            (clojure-test-mode t)))))
;;
;;     Or generate autoloads with the `update-directory-autoloads' function.

;;; TODO:

;; * Summary message in minibuffer
;; * Errors *loading* the tests are not reported
;; * Errors occasionally fail to highlight. Not consistently reproducible
;; * Highlight tests as they fail? (big job, probably, useful for slow suites)

;;; Code:

(require 'clojure-mode)
(require 'slime)

;; Faces

(defface clojure-test-failure-face
  '((((class color) (background light))
     :background "orange red") ;; TODO: Hard to read strings over this.
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in Clojure tests."
  :group 'clojure-test-mode)

(defface clojure-test-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for errors in Clojure tests."
  :group 'clojure-test-mode)

;; Support Functions

(defun clojure-test-eval (string &optional handler)
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (or handler #'identity)))

(defun clojure-test-load-reporting ()
  "Redefine the test-is report function to store results in metadata."
  (clojure-test-eval
   "(use 'clojure.contrib.test-is)
 (ns clojure.contrib.test-is)
 (defonce old-report report)
 (defn report [event msg expected actual]
  (if-let [current-test (last *testing-vars*)]
          (alter-meta! current-test
                       assoc :status (conj (:status ^current-test)
                                       [event msg (str expected) (str actual)
                                        ((file-position 2) 1)])))
  (old-report event msg expected actual))"))

(defun clojure-test-get-results (result)
  (clojure-test-eval
   "(map #(cons (str (:name (meta %)))
                (:status (meta %))) (vals (ns-interns *ns*)))"
   #'clojure-test-extract-results))

(defun clojure-test-extract-results (results)
  ;; slime-eval-async hands us a cons with a useless car
  (setq the-results nil)
  (mapcar #'clojure-test-extract-result (read (cadr results)))
  ;; TODO: (message "Ran 7 tests containing 16 assertions. 4 failures, 9 errors.")
  )

(defun clojure-test-extract-result (result)
  "Parse the result from a single test. May contain multiple is blocks."
  (dolist (is-result (rest result))
    (setq the-is-result is-result)
    (destructuring-bind (event msg expected actual line) (coerce is-result 'list)
      (if (equal :fail event)
          (clojure-test-highlight-problem
           line event (format "Expected %s, got %s" expected actual))
        (if (equal :error event)
            (clojure-test-highlight-problem line event actual))))))

(defun clojure-test-highlight-problem (line event message)
  ;; (add-to-list 'the-results (list line event message))
  (save-excursion
    (goto-line line)
    (set-mark-command nil)
    (end-of-line)
    (let ((overlay (make-overlay (mark) (point))))
      (overlay-put overlay 'face (if (equal event :fail)
                                     'clojure-test-failure-face
                                   'clojure-test-error-face))
      (overlay-put overlay 'message message))))

;; Commands

(defun clojure-test-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  ;; TODO: this is async; might need to make sure it finishes before
  ;; we load the file next.
  (clojure-test-clear) 
  (slime-load-file (buffer-file-name))
  (clojure-test-eval "(clojure.contrib.test-is/run-tests)"
                     #'clojure-test-get-results))

(defun clojure-test-show-result ()
  "Show the result of the test under point."
  (interactive)
  (let ((overlay (car (overlays-at (point)))))
    (if overlay
        (message (overlay-get overlay 'message)))))

(defun clojure-test-clear ()
  "Remove overlays and clear stored results."
  (interactive)
  (remove-overlays)
  (clojure-test-eval
   "(doseq [t (vals (ns-interns *ns*))]
      (alter-meta! t assoc :status [])
      (alter-meta! t assoc :test nil))"))

(defvar clojure-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-,") 'clojure-test-run-tests)
    (define-key map (kbd "C-c '")   'clojure-test-show-result)
    map)
  "Keymap for Clojure test mode.")

;;;###autoload
(define-minor-mode clojure-test-mode
  "A minor mode for running Clojure tests."
  nil " Test" clojure-test-mode-map
  (if (slime-connected-p)
      (clojure-test-load-reporting)
    (slime)
    (add-hook 'slime-connected-hook 'clojure-test-load-reporting)))

;;;###autoload
(add-hook 'clojure-mode-hook
          (lambda () (save-excursion
                  (goto-char (point-min))
                  (if (or (search-forward "(deftest" nil t)
                          (search-forward "(with-test" nil t))
                      (clojure-test-mode t)))))
;; Don't want to make this a defun since that means the hook would
;; autoload the whole file.

(provide 'clojure-test-mode) ;;; clojure-test-mode.el ends here
