(ns sandbox.pirate-lang 
  (:require [clojure.string :as string]))

(def english {:alphabet    "abcdefghijklmnopqrstuvwxyz"
              :vowels      "aeiou"
              :pirate-char "o"})

(def swedish {:alphabet    "abcdefghijklmnopqrstuvwxyzåäö"
              :vowels      "aeiouåäö"
              :pirate-char "o"})

(defn- configure
  [{:keys [alphabet vowels pirate-char]}]
  (let [alphabet   (set (seq (string/upper-case alphabet)))
        vowels     (set (seq (string/upper-case vowels)))
        consonants (set (remove vowels alphabet))
        pirates    (if (vowels pirate-char)
                     vowels
                     consonants)]
    {:pirate-char pirate-char
     :pirates pirates}))

(defn to-pirate-talk
  [text language]
  (let [{:keys [pirate-char pirates]} (configure language)]
    (apply str (mapcat (fn [c]
                         (if (pirates (first (string/upper-case c)))
                           (interpose pirate-char (repeat 2 c))
                           [c]))
                       text))))

(defn from-pirate-talk
  [text language]
  (let [{:keys [pirate-char pirates]} (configure language)
        pattern (re-pattern (str "(?i)([" (apply str pirates) "])" pirate-char "\\1"))]
    (string/replace text pattern "$1")))

(comment
  (to-pirate-talk "Have you heard about Pirate talk?" english)
  ;; => "HoHavove yoyou hohearordod aboboutot PoPiroratote totalolkok?"

  (from-pirate-talk "HoHavove yoyou hohearordod aboboutot PoPiroratote totalolkok?" english)
  ;; => "Have you heard about Pirate talk?"

  (to-pirate-talk "Har du hört talas om rövarspråket?" swedish)
  ;; => "HoHaror dodu hohörortot totalolasos omom rorövovarorsospoproråkoketot?"

  (from-pirate-talk "HoHaror dodu hohörortot totalolasos omom rorövovarorsospoproråkoketot?" swedish)
  ;; => "Har du hört talas om rövarspråket?"
  )