(ns pftv.run-length)

(defn rle-first-try
  ([c]
   (rle-first-try (lazy-seq) c))
  ([e c]
   (if (empty? c)
     e
     (let [p (first (take 1 c))
           e|c (partition-by #(not= p %) c)
           l (count (first (take 1 e|c)))]
       (rle-first-try
        (lazy-cat e [[l p]])
        (drop l c))))))


(defn rle
  [c]
  ((fn encode [e c]
     (if (empty? c)
       e
       (let [p (first c)
             l (or (first (keep-indexed #(when (not= %2 p) %1) c)) (count c))]
         (encode
          (lazy-cat e [[l p]])
          (drop l c)))))
   (lazy-seq) c))

(defn rld [e]
  (mapcat #(repeat (first %) (second %)) e))


(comment
  (count (rle (range 10000)))
  (= (rle [:a :a :a :b :c :d :d :d :d])
     '([3 :a] [1 :b] [1 :c] [4 :d]))
  (rld '([3 :a] [1 :b] [1 :c] [4 :d] [2 "foo"]))
  ;=> (:a :a :a :b :c :d :d :d :d))

  (def c [:a :a :a :b :c :d :d :d :d])
  (drop nil c)
  (keep-indexed #(when (not= %2 :d) %1) [:d :d :d])
  (drop 4 c)
  (->> c
       (partition-all 2)
       (partition-by #(not= (take 1 %) (drop 1 %))))
  (let [ec (partition-by #(not= :a %) c)]
    (flatten (drop 1 ec)))
  (take 1 c)
  (->> c
       (partition-by #(not= :a %))))