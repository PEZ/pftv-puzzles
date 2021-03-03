(ns brave.hobbit
  (:require clojure.string))

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

(defn complete-body [incomplete]
  (into []
        (reduce #(set (conj %1 (assoc %2 :name (clojure.string/replace (:name %2) #"^left-" "rigth-")))) incomplete incomplete)))

(defn complete-body-b [incomplete]
  (into []
        (into (set incomplete)
              (map #(reduce-kv (fn [m k v] (assoc m k (clojure.string/replace v #"^left-" "rigth-"))) % %) incomplete))))

(defn complete-body-a [incomplete]
  (reduce (fn [complete part]
            (into complete (set [part (assoc part :name (clojure.string/replace (:name part) #"^left-" "rigth-"))])))
          []
          incomplete))

(comment
  (complete-body asym-hobbit-body-parts)
  (complete-body-a asym-hobbit-body-parts))