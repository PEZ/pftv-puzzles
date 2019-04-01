(ns pftv.remove-nth)

(use 'criterium.core)

;; Suspecting Eric is pulling my leg here, but anyway
(defn remove-at [n c]
  (concat (take n c) (drop (inc n) c)))

(comment
  (remove-at 3 [1 2 3 4 5 6])
  (remove-at 3 '(1 2 3 4 5 6))
  (with-progress-reporting (quick-bench (remove-at 1000000 (range 1 2000000))))
  (remove-at 3 [])
  (remove-at 3 [1])
  (remove-at 3 [1 2])
  (remove-at 3 [1 2 3])
  (remove-at 3 [1 2 3 4])
  (remove-at 6 [1 2 3 4 5 6])
  (take 6 (range 10))

  (lazy-cat (seq ["lazy-cat" "is" "my" "favorite" "function"])))