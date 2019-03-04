(ns drop-every)

(defn drop-every [nth coll]
  (->> coll
       (map-indexed #(when (< 0 (rem (inc %1) nth)) %2))
       (remove nil?)))

(defn drop-every-td [nth coll]
  (loop [selected '()
         candidates coll]
    (if (empty? candidates)
      selected
      (recur (concat selected (take (dec nth) candidates)) (drop nth candidates)))))



(comment
  (drop-every 3 [:a :b :c :d :e :f :g])
  (drop-every-td 3 [:a :b :c :d :e :f :g])
  ;=> (:a :b :d :e :g)
  (time (count (take 1000 (drop-every 3 (range 100000)))))

  (take 2 [:a])
  (drop 1 [:a :b :c]))