(ns pftv.vowels 
  (:require [clojure.string :as string]))

(def l9n-vowels
  {:english "aeiou"
   :swedish "aeiouåäö"})

(def l9n-pattern
 (->> l9n-vowels
      (reduce-kv (fn [m k v]
                   (let [vowel-re (re-pattern (str "(?i)[" v "](?=[^" v "]*$)"))]
                     (assoc m k vowel-re)))
                 {})))

(defn remove-last-vowel
  [text lang]
  (string/replace text (l9n-pattern lang) ""))

(comment
  (remove-last-vowel "Hi there!" :english)
  ;; => "Hi ther!"
  (remove-last-vowel "This is not a test." :english)
  ;; => "This is not a tst."
  (remove-last-vowel "Hippopotamus" :english)
  ;; => "Hippopotams"
  (remove-last-vowel "Heja Eric, du är bäst!" :swedish)
  ;; => "Heja Eric, du är bst!"
  )