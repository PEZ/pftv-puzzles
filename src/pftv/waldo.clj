(ns pftv.waldo
  (:require [clojure.test :refer [is]]))

(defn wheres-waldo
  "Find the path to waldo"
  {:test (fn []
           (is (= [7 1]
                  (wheres-waldo :W ;; look for :W
                                [[:A :B :C]
                                 [:D :E :F]
                                 [:G :H :I]
                                 [:J :K :L]
                                 [:M :N :O]
                                 [:P :Q :R]
                                 [:S :T :U]
                                 [:V :W :X]
                                 [:Y :and :Z]])))
           (is (= nil (wheres-waldo :W [[:A :B :C]])))
           (is (= [1 2]
                  (wheres-waldo [:i :am :waldo]
                                [[:a :b "ceee"]
                                 [[:i :am :not :waldo] [:i :am :spartacus] [:i :am :waldo]]
                                 ["d" "e" 6]]))))}
  [waldo grid]
  (->> (map-indexed (fn [i v] [i (.indexOf v waldo)]) grid)
       (filter #(not= -1 (second %)))
       first)
  )

(defn wheres-waldo-bonus
  "Find the path to waldo"
  {:test (fn []
           (is (= [7 1 0]
                  (wheres-waldo-bonus :W
                                      [[:A :B :C]
                                       [:D :E :F]
                                       [:G :H :I]
                                       [:J :K :L]
                                       [:M :N :O]
                                       [:P :Q :R]
                                       [:S :T :U]
                                       [:V [:W] :X]
                                       [:Y :and :Z]])))
           (is (= nil (wheres-waldo-bonus :W
                                          [[:A :B :C]])))
           (is (= [1]
                  (wheres-waldo-bonus :W
                                      [:V :W :X])))
           (is (= [1 2 1]
                  (wheres-waldo-bonus [:i :am :waldo]
                                      [[:a :b "ceee"]
                                       [[:i :am :not :waldo] [:i :am :spartacus] [:hello [:i :am :waldo] [:who :are :you?]]]
                                       ["d" "e" 6]]))))}
  ([waldo vektor]
   (wheres-waldo-bonus waldo vektor []))
  ([waldo vektor path]
   (when (vector? vektor)
     (let [i (.indexOf vektor waldo)]
       (if (> i -1)
         (conj path i)
         (->> (map #(wheres-waldo-bonus waldo %1 (conj path %2)) vektor (range))
              (remove nil?)
              first))))))