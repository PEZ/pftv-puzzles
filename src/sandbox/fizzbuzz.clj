(ns sandbox.fizzbuzz
  (:require [clojure.test :refer [is]]))

(defn- divizable?
  "Is `n` divisable by `d`?"
  {:test (fn []
           (is (divizable? 49 7))
           (is (not (divizable? 48 7))))}
  [n d]
  (zero? (mod n d)))

(comment
  (divizable? 5 5)
  ;; => true
  (divizable? 1 0)
  ;; => Execution error (ArithmeticException) at fizzy/divizable? (anagram.clj:22).
  ;;    Divide by zero
  )

(defn fizz-buzz
  "FizzBuzz it"
  {:test (fn []
           (is (= 4
                  (fizz-buzz 4)))
           (is (= ["Buzz" 11 "Fizz" "FizzBuzz"]
                  (map fizz-buzz [10 11 12 15])))
           (is (= [1 2 "Fizz"]
                  (fizz-buzz 1 4)))
           (is (= "FizzBuzz"
                  (nth (fizz-buzz) (dec 90))))
           (is (= 100
                  (count (fizz-buzz)))))}
  ([n]
   (cond
     (divizable? n (* 3 5)) "FizzBuzz"
     (divizable? n (* 3)) "Fizz"
     (divizable? n (* 5)) "Buzz"
     :else n))
  ([start end]
   (map fizz-buzz (range start end)))
  ([]
   (fizz-buzz 1 101)))

(comment
  (fizz-buzz 4)
  ;; => 4
  (fizz-buzz 1 15)
  ;; => (1 2 "Fizz" 4 "Buzz" "Fizz" 7 8 "Fizz" "Buzz" 11 "Fizz" 13 14)
  (fizz-buzz))
