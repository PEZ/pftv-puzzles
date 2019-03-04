(ns drop-every)

(defn drop-every [nth coll]
  (->> coll
       (map-indexed #(when (< 0 (rem (inc %1) nth)) %2))
       (remove nil?)))

(defn drop-every [nth coll]
  (loop [selected '()
         candidates coll]
    (if (empty? candidates)
      selected
      (recur (concat selected (take (dec nth) candidates)) (drop nth candidates)))))

(defn drop-every [nth coll]
  (let [nth-1 (dec nth)]
    (->> coll
         (partition-all nth)
         (map #(take nth-1 %))
         (apply concat))))

(defn drop-every [nth coll]
  (keep-indexed #(when (< 0 (rem (inc %1) nth)) %2) coll))

(defn drop-every [nth coll]
  (lazy-seq
   (when (not-empty coll)
     (concat (take (dec nth) coll)
             (drop-every nth (drop nth coll))))))

(defn drop-every
  ([n s] (drop-every 1 n s))
  ([counter n s]
   (when (not-empty s)
     (if (= counter n)
       (drop-every 1 n (rest s))  ;; skip and reset counter to 1
       (lazy-seq (cons (first s) (drop-every (inc counter) n (rest s))))))))

(defn drop-every [nth coll]
  (apply concat (#(partition (- %2 1) %2 nil %1) coll nth)))

(defn drop-every [nth coll]
  (apply concat (partition-all (dec nth) nth coll)))

(comment
  (drop-every 3 [:a :b :c :d :e :f :g])
  (drop-every-p 3 [:a :b :c :d :e :f :g])
  (drop-every-td 3 [:a :b :c :d :e :f :g])
  (drop-every-td 3 (range 10000))
  ;=> (:a :b :d :e :g)
  (time (count (drop-every 3 (range 1000000))))
  (time (count (drop-every-keep 3 (range 1000000))))
  (time (count (drop-every-p 3 (range 1000000))))
  (time (count (take 10 (drop-every 3 (range 1000000)))))
  (time (count (drop-every-td 3 (range 1000000))))
  (time (count (take 10 (drop-every-td 3 (range 10000)))))

  (take 2 [:a])
  (drop 1 [:a :b :c]))