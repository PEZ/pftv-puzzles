(ns pftv.remove-nth)

(use 'criterium.core)

(defn remove-at [n c]
  ((if (vector? c) (comp vec concat) concat) (take n c) (drop (inc n) c)))

(comment
  (remove-at 3 [1 2 3 4 5 6])  ; => [1 2 3 5 6]
  (remove-at 3 '(1 2 3 4 5 6)) ; => (1 2 3 5 6)
  (remove-at 3 (range 1 7))    ; => (1 2 3 5 6)
  (remove-at 3 [])             ; => []
  (remove-at 3 [1])            ; => [1]
  (remove-at 0 [1 2])          ; => [2]
  (remove-at 1 [1 2])          ; => [1]
  (remove-at -1 [1 2])         ; => [1 2]
  (with-progress-reporting (quick-bench (remove-at 10000 (range 1 20000))))
  (with-progress-reporting (quick-bench (remove-at 1000000 (range 1 2000000)))))