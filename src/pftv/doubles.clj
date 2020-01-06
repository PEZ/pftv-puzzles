(ns pftv.doubles)

(defn duplicate-letter?
  [s]
  (boolean
   (some->> (partition 2 1 s)
            (filter (partial apply =))
            not-empty)))

(comment
  (duplicate-letter? "Hello world")
  ;; => true
  
  (duplicate-letter? "Hey there!")
  ;; => false
  
  (duplicate-letter? "")
  ;; => false

  )