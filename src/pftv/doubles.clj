(ns pftv.doubles)

(defn duplicate-letter-1?
  [s]
  (boolean
   (some->> (partition 2 1 s)
            (filter (partial apply =))
            not-empty)))

(defn duplicate-letter-2?
  [s]
  (boolean
   (some->> (partition-by identity s)
            (group-by count)
            keys
            (reduce +)
            (< 1))))

(defn duplicate-letter?
  [s]
  (boolean
   (->> (map = s (rest s))
        (drop-while false?)
        first)))

(comment
  (duplicate-letter? "Hello world")
  ;; => true
  
  (duplicate-letter? "Hey there!")
  ;; => false
  
  (duplicate-letter? "")
  ;; => false

  )