;;;; Clojure mode auto loads

;; Autoload Clojure
(autoload 'clojure-mode "clojure-mode" "A mode for clojure lisp" t)

;; Automode for clojure
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(provide 'clojure-auto)