(ns drop-every)

; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-316-avoid-licensing-and-support-issues-with-the-right-jdk-for-you/

(defn drop-every [nth coll]
  (->> coll
       (map-indexed #(when (< 0 (rem (inc %1) nth)) %2))
       (remove nil?)))

; Stack overflow! See: https://stuartsierra.com/2015/04/26/clojure-donts-concat
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


; Daw-Ran Liou on Slack
(defn drop-every [nth coll]
  (lazy-seq
   (when (not-empty coll)
     (concat (take (dec nth) coll)
             (drop-every nth (drop nth coll))))))

; boris on Slack
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
  ;=> (:a :b :d :e :g)
  (time (count (drop-every 3 (range 1000000))))
  (time (count (take 10 (drop-every 3 (range 1000000))))))