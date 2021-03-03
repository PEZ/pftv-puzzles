(ns pftv.gapfulness)

(defn closest-gapful [num]
  (if (< num 100)
    100
    (let [gapful? (fn [n]
                    (when (and n (> n 99))
                      (let [d (as-> n $
                                (str $)
                                (str (first $) (last $))
                                (read-string $))]
                        (= (mod n d) 0))))]
      (loop [candidates [num num]]
        (let [recruit (first (filter gapful? candidates))]
          (if recruit
            recruit
            (recur [(dec (first candidates)) (inc (last candidates))])))))))


(comment
  (closest-gapful 1024))