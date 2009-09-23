(ns clojure-mode.test
  (:use [clojure.test]))

(deftest test-str
  (is (= "o hai" (str "o" "hai"))))

(deftest test-errs
  (is (({} :hi)))
  (is (str "This one doesn't actually error."))
  (is (= 0 (/ 9 0))))

(deftest test-bad-math
  (is (= 0 (* 8 2)))
  (is (= 5 (+ 2 2))))

(deftest test-something-that-actually-works
  (is (= 1 1)))

;; For debugging
;; (map #(cons (str (:name (meta %))) (:status (meta %))) (vals (ns-interns *ns*)))
;; (insert (pp the-result))

(comment ;; for indentation
  (with-hi heya
    somebuddy)

  (deftoggle cap
    gabba)

  (couch/with-db hi
    your-db)

  (clo/defguppy gurgle
    minnow))