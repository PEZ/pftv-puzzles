(ns brave.fylke
  (:require clojure.string))


(hash-map 1 2 :a "c")

(hash-set :a :b [1 2])

(defn adder [size]
  (fn [n] (+ size n)))

(defn suber [size]
  (fn [n] (- n size)))

(def dec9 (suber 9))

(defn mapset [f coll]
  (reduce 
   (fn [result item] 
     (conj result (f item)))
   #{} 
   coll))

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn complete-body-alien [incomplete add-dimensions]
  (reduce (fn [complete part]
            (into complete (set (flatten [part (map #(assoc part :name (clojure.string/replace (:name part) #"^left-" %)) add-dimensions )]))))
          []
          incomplete))

(comment
  ((adder 100) 10)
  (dec9 10)
  (mapset rand-int [1 1 1 2 2 3 4])
  (complete-body-alien asym-hobbit-body-parts ["right-" "mid-"]))