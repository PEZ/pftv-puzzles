(ns pftv.run-length)

;; https://stackoverflow.com/questions/4830900/how-do-i-find-the-index-of-an-item-in-a-vector

(defn rle
  ([c]
   (rle (lazy-seq) c))
  ([e c]
   (if (empty? c)
     e
     (let [p (first (take 1 c))
           ec (partition-by #(not= p %) c)
           l (count (first (take 1 ec)))]
       (rle
        (lazy-cat e [[l p]])
        (drop l c))))))

(defn rle
  ([c]
   (rle (lazy-seq) c))
  ([e c]
   (if (empty? c)
     e
     (let [p (first (take 1 c))
           ec (partition-by #(not= p %) c)
           l (count (first (take 1 ec)))]
       (rle
        (lazy-cat e [[l p]])
        (drop l c))))))


(comment
  (= (rle [:a :a :a :b :c :d :d :d :d])
     '([3 :a] [1 :b] [1 :c] [4 :d]))
  (type (rle [:a :a :a :b :c :d :d :d :d]))
  (rld '([3 :a] [1 :b] [1 :c] [4 :d]))
  ;=> (:a :a :a :b :c :d :d :d :d))

  (def c [:a :a :a :b :c :d :d :d :d])
  (drop 4 c)
  (->> c
       (partition-all 2)
       (partition-by #(not= (take 1 %) (drop 1 %))))
  (let [ec (partition-by #(not= :a %) c)]
    (flatten (drop 1 ec)))
  (take 1 c)
  (->> c
       (partition-by #(not= :a %))))