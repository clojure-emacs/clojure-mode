
(defun clojure-paredit-hook ()
  (paredit-mode +1)
  (define-key clojure-mode-map "{" 'paredit-open-brace)
  (define-key clojure-mode-map "}" 'paredit-close-brace))

(add-hook 'clojure-mode-hook 'clojure-paredit-hook)
