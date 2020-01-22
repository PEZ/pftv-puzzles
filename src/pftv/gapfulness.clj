(ns pftv.gapfulness)

(defn gapful [num]
  (if (< num 100)
    100
    (loop [candidates [num num]]
      (let [gapful? (fn [n]
                      (when (and n (> n 99))
                        (let [d (as-> n v
                                  (str v)
                                  (str (first v) (last v))
                                  (read-string v))]
                          (= (mod n d) 0))))
            recruit (first (filter gapful? candidates))]
        (if recruit
          recruit
          (recur [(dec (first candidates)) (inc (last candidates))]))))))


(comment
  (gapful -57657104))