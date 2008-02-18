
(defun clojure-open-brace (&optional n)
  "A horrible hackish fix to prevent adding a space between #^ and {}"
  (interactive "P")
  (with-syntax-table clojure-mode-brace-syntax-table
    (paredit-open-brace n)))

(defun clojure-paredit-hook ()
  (paredit-mode +1)

  ;; Part of the horrible hackish fix to prevent adding a space between #^ and {}
  (set (make-local-variable 'clojure-mode-brace-syntax-table)
       (copy-syntax-table clojure-mode-syntax-table))
  (modify-syntax-entry ?^ "    " clojure-mode-brace-syntax-table)
  
  (define-key clojure-mode-map "{" 'clojure-open-brace)
  (define-key clojure-mode-map "}" 'paredit-close-brace))

(add-hook 'clojure-mode-hook 'clojure-paredit-hook)
