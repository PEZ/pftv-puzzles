(ns eratosthenes
  (:require [clojure.core.async :as a]
            [clojure.core.reducers :as r]
            [taoensso.tufte :as tufte]))

;; From https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
;; 1. Create a list of consecutive integers from 2 through n: (2, 3, 4, ..., n).
;; 2. Initially, let p equal 2, the smallest prime number.
;; 3. Enumerate the multiples of p by counting in increments of p from 2p to n,
;;    and mark them in the list (these will be 2p, 3p, 4p, ...;
;;    the p itself should not be marked).
;; 4. Find the first number greater than p in the list that is not marked.
;;    If there was no such number, stop. Otherwise, let p now equal this
;;    new number (which is the next prime), and repeat from step 3.
;; 5. When the algorithm terminates, the numbers remaining not marked in the
;;    list are all the primes below n.

(use 'criterium.core)

(defn not-divisible-by [n d]
  (when-not (= 0 (rem n d))
    n))

(comment
  (not-divisible-by 3 3)
  (not-divisible-by 4 3)
  (rem 4 3))

(defn sieve-first [n]
  (cond
    (< n 2) ()
    :else (loop [found-primes [2]
                 candidates (range 2 (inc n))]
            (let [highest-found-prime (last found-primes)
                  remaining (->> candidates
                                 (map #(not-divisible-by % highest-found-prime))
                                 (remove nil?))]
              (if (= remaining candidates)
                (concat found-primes candidates)
                (recur (concat found-primes (take 1 remaining)) (drop 1 remaining)))))))

(comment
  (sieve-first 30)
  (time (str "mine: " (count (sieve-first 100000)) " primes"))
  (with-progress-reporting (bench (sieve-first 100000) :verbose)))

; Refinement: it is sufficient to mark the numbers in step 3 starting
; from p2, as all the smaller multiples of p will have already been marked
; at that point. The algorithm is allowed to terminate in step 4 when p2
; is greater than n

(defn sieve1 [n]
  (cond
    (< n 2) ()
    (< n 3) (range 2 3)
    :else (let [sqrt-n (-> n Math/sqrt int)]
            (loop [known (range 2 4)
                   candidates (range 5 (inc n) 2)
                   p 3]
              (if (< sqrt-n p)
                (concat known candidates)
                (let [remaining (remove #(= 0 (rem % p)) candidates)
                      next-p (take 1 remaining)]
                  (recur (concat known next-p) (drop 1 remaining) (first next-p))))))))

(use 'clojure.set)

(defn sieve2 [^long n]
  (if (< n 2)
    ()
    (let [sqrt-n (Math/sqrt n)]
      (loop [primes (set (range 3 (inc n) 2))
             p 3]
        (if-not (< p sqrt-n)
          (concat '(2) (sort primes))
          (recur (difference primes (set (range (* p p) n (+ p p)))) (+ p 2)))))))

(comment
  (run-test sieve2)
  (sieve2 100)
  (quick-bench (doall (sieve2 1000000)))
  (quick-bench (count (sieve2 1000000)))
  (tufte/profile {}
                 (count (sieve2 1000000))))

(comment
  (sieve2 30)

    
  (time (str "sieve1: " (count (sieve1 100000)) " primes"))
  (with-progress-reporting (quick-bench (sieve1 100000) :verbose))
  (time (str "sieve2:  " (count (sieve2 100000)) " primes"))
  (with-progress-reporting (quick-bench (sieve2 100000) :verbose)))


;; Clojure lazy-seq function to generate n prime numbers. 
;; It generates .5 million prime numbers in 20 secs using
;; the naive non-sieve algo
;; Author: Abhishek Gupta (@abhilater)

(defn primes-to-n
  [n]
  (filter (fn [num]
            (loop [end (int (Math/sqrt num)), div 2, re (rem num div)]
              (cond
                (< num 2) false
                (= num 2) true
                (= re 0) false
                (> div end) true
                :else (recur end (inc div) (rem num div))))) (range (inc n))))

(comment
  (primes-to-n 70)
  (time (str "@abhilater's: " (count (primes-to-n 1000000)) " primes"))
  (with-progress-reporting (quick-bench (count (primes-to-n 100000)) :verbose)))


; https://gist.github.com/ericnormand/b29ef113401ad2a6f656c4b701fb08a7#file-steve-miner-clj
(defn classic-sieve
  "Returns sequence of primes less than N"
  [n]
  (loop [nums (transient (vec (range n))) i 2]
    (cond
      (> (* i i) n) (remove nil? (nnext (persistent! nums)))
      (nums i) (recur (loop [nums nums j (* i i)]
                        (if (< j n)
                          (recur (assoc! nums j nil) (+ j i))
                          nums))
                      (inc i))
      :else (recur nums (inc i)))))


; https://gist.github.com/ericnormand/b29ef113401ad2a6f656c4b701fb08a7#file-valentin-waeselynck-clj
(defn primes-below
  "Finds all prime numbers less than n, returns them sorted in a vector"
  [n]
  (if (< n 2)
    []
    (let [sieve (boolean-array n false)
          s (-> n Math/sqrt Math/floor int)]
      (loop [p 2]
        (if (> p s)
          (into []
                (remove #(aget sieve %))
                (range 2 n))
          (do
            (when-not (aget sieve p)
              (loop [i (* 2 p)]
                (when (< i n)
                  (aset sieve i true)
                  (recur (+ i p)))))
            (recur (inc p))))))))

; Teodor Heggelund, https://clojureverse.org/t/eratosthenes-party-time-a-k-a-feedback-wanted-on-this-implementation-of-eratosthenes-sieve/3801/7?u=pez
(defn bitset-sieve [n]
  (let [primes (java.util.BitSet. n)
        imax (-> n Math/sqrt int inc)]
    ;; Start by assuming they are all primes
    (.set primes 2 n)
    ;; Now, iterate through the primes.
    (loop [i (.nextSetBit primes 0)]
      ;; Imperative code ...
      (if (or (> i imax)
              (= i -1))
        primes
        (do
          (doseq [j (range (+ i i) n i)]
            ;; Not prime -- multiple of i!
            (.clear primes j))
          (recur (.nextSetBit primes (inc i))))))))

(defn bitset-sieve-2 [n]
  (let [primes (java.util.BitSet. n)
        imax (-> n Math/sqrt int inc)]
    ;; Start by assuming they are all primes
    (.set primes 2 n)
    ;; Now, iterate through the primes.
    (loop [i (.nextSetBit primes 0)]
      ;; Imperative code ...
      (if (or (> i imax)
              (= i -1))
        (filter #(.get primes %)
                (range 2 n))
        (do
          (doseq [j (range (* i i) n i)]
            ;; Not prime -- multiple of i!
            (.clear primes j))
          (recur (.nextSetBit primes (inc i))))))))

(defn bitset->vec [bs]
  (->> (range (.size bs))
       (filter (fn [x] (.get bs x)))
       (into [])))


(defn boolean-array-sieve [n]
  ;; Start by assuming they are all primes
  (let [primes (boolean-array n true)
        imax (-> n Math/sqrt int inc)]
    ;; Now, iterate through the primes.
    (loop [i 2]
      ;; Imperative code ...
      (if (or (> i imax)
              (= i -1))
        (filter #(aget primes %)
                (range 2 n))
        (do
          (doseq [j (range (* i i) n i)]
            ;; Not prime -- multiple of i!
            (aset primes j false))
          (recur (inc i)))))))

(defn bs-sieve [^long n]
  (let [primes (java.util.BitSet. n)
        sqrt-n (long (Math/ceil (Math/sqrt n)))]
    (loop [p 2]
      (if-not (< p sqrt-n)
        (remove #(.get primes %)
                (range 2 (inc n)))
        (do
          (loop [i (* p p)]
            (when (< i n)
              (.set primes i)
              (recur (+ i p))))
          (recur (.nextClearBit primes (inc p))))))))

(comment
  (run-test bs-sieve)
  (bs-sieve 100)
  (quick-bench (doall (bs-sieve 1000000)))
  (quick-bench (count (bs-sieve 1000000)))
  (tufte/profile {}
                 (count (bs-sieve 1000000))))
(defn ba-sieve [^long n]
  (let [primes (boolean-array (inc n) false)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (loop [p 2]
      (if-not (< p sqrt-n)
        (remove #(aget primes %)
                (range 2 (inc n)))
        (do
          (when-not (aget primes p)
            (loop [i (* p p)]
              (if (<= i n)
                (do
                  (aset primes i true)
                  (recur (+ i p))))))
          (recur (inc p)))))))

(comment
  (run-test ba-sieve)
  (ba-sieve 100)
  (quick-bench (doall (ba-sieve 1000000)))
  (quick-bench (count (ba-sieve 1000000)))
  (tufte/profile {}
                 (count (ba-sieve 1000000))))

(defn ba-sieve-2 [^long n]
  (let [hn (int (Math/ceil (/ n 2)))
        max-p (int (Math/ceil (Math/sqrt n)))
        sieved (boolean-array hn true)]
    (loop [p 3]
      (if-not (< p max-p)
        (concat [2] (map #(inc (* % 2))
                     (filter #(aget sieved %) 
                             (range 1 hn))))
        (let [sp (/ (dec p) 2)]
          (when (aget sieved sp)
            (loop [i (* p p)]
              (if (< i n)
                (do
                  (aset sieved (/ (dec i) 2) false)
                  (recur (+ i p))))))
          (recur (inc (inc p))))))))



(defn primes-below-2
  "Finds all prime numbers less than n, returns them sorted in a vector"
  [^long n]
  (if (< n 2)
    []
    (let [sieve (boolean-array n true)
          s (-> n Math/sqrt Math/floor int)]
      (loop [p 2]
        (if (> p s)
          (filter #(aget sieve %)
                 (range 3 n 2))
          (do
            (when (aget sieve p)
              (loop [i (* p p)]
                (when (< i n)
                  (aset sieve i false)
                  (recur (+ i p)))))
            (recur (+ 1 p))))))))

(defn sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (filter #(aget primes %)
                          (range 3 (inc n) 2)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))

;; What if we use an int-array from 0 to n and set all primes to 0,
;; then filter out the 0's by converting to a set?
;; Answer: it goes much, much slower!
(defn sieve-2 [^long n]
  (let [primes (int-array (inc n) (->> (range 3 (inc n) 2)
                                      (interpose 0)
                                      (concat [0 1 2])))
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (loop [p 3]
      (if (< sqrt-n p)
        (drop 2 (sort (set primes)))
        (do
          (when-not (= 0 (aget primes p))
            (loop [i (* p p)]
              (when (<= i n)
                (aset primes i 0)
                (recur (+ i p p)))))
          (recur (+ p 2)))))))

(defn stuart-sieve [^long upper]
  (let [xs (->> (range 3 (inc upper))
                (set))]
    (sort (reduce #(if (or (= 1 %2) (not (%1 %2)))
                     %1
                     (let [multiples (range (* 2 %2) (inc upper) %2)]
                       (set (remove (set multiples) %1))))
                  (concat '(2) xs) 
                  (range 1 (Math/sqrt (inc upper)))))))

(defn sieve3 [^long n]
  (if (< n 2)
    ()
    (let [sqrt-n (Math/sqrt n)]
      (loop [primes (range 3 (inc n) 2)
             p 3]
        (if-not (< p sqrt-n)
          (concat '(2) (sort primes))
          (recur (difference (set primes) 
                             (range (* p p) n (+ p p))) (+ p 2)))))))


(defn em-sieve-mask [n]
  (let [upper (inc n)
        mask (transient (vec (repeat upper true)))]
    (loop [p 2
           mask mask]
      (if (< n (* p p))
        (persistent! mask)
        (if (get mask p)
          (recur (inc p) (reduce #(assoc! %1 %2 false) mask (range (* 2 p) upper p)))
          (recur (inc p) mask))))))

(defn em-sieve [n]
  (let [mask (map vector (range (inc n)) (em-sieve-mask n))]
    (->> (filter second mask)
         (mapv first)
         (drop 2))))

(import java.util.BitSet)
(defn mbjarland-bs-sieve [^long end]
  (let [bs (BitSet. end)]
    (loop [x 2]
      (if (= x end)
        (drop 2 (-> (doto bs (.flip 0 end)) .stream .toArray vec))
        (do (doseq [y (range (* x 2) end x)]
              (.set bs y))
            (recur (.nextClearBit bs (inc x))))))))

(defn pez-mbjarland-sieve [end]
  (let [bs (BitSet. end)]
    (loop [x 2]
      (if (= x end)
        (drop 2 (-> (doto bs (.flip 0 end)) bitset->vec))
        (do (doseq [y (range (* x 2) end x)]
              (.set bs y))
            (recur (.nextClearBit bs (inc x))))))))

(defn pez-ba-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (filter #(aget primes %)
                          (range 3 (inc n) 2)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))
(comment
  (run-test pez-ba-sieve)
  (pez-ba-sieve 100)
  (quick-bench (doall (pez-ba-sieve 1000000)))
  (quick-bench (count (pez-ba-sieve 1000000)))
  (tufte/profile {}
                 (count (pez-ba-sieve 1000000))))


(defn pez-ba-reduce-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (reduce
                   (fn [acc v]
                     (if (aget primes v)
                       (conj acc v)
                       acc))
                   []
                   (range 3 (inc n) 2)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))
(comment
  (run-test pez-ba-reduce-sieve)
  (pez-ba-reduce-sieve 100)
  (quick-bench (doall (pez-ba-reduce-sieve 1000000)))
  (quick-bench (count (pez-ba-reduce-sieve 1000000)))
  (tufte/profile {}
                 (count (pez-ba-reduce-sieve 1000000))))

(defn pez-ba-reducer-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (into [] (r/filter #(aget primes %)
                                     (range 3 (inc n) 2))))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))
(comment
  (run-test pez-ba-reducer-sieve)
  (pez-ba-reducer-sieve 100)
  (quick-bench (doall (pez-ba-reducer-sieve 1000000)))
  (quick-bench (count (pez-ba-reducer-sieve 1000000)))
  (tufte/profile {}
                 (count (pez-ba-reducer-sieve 1000000))))

(defn pez-ba-transduce-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (transduce
                   (filter #(aget primes %))
                   conj
                   (range 3 (inc n) 2)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))

(comment
  (run-test pez-ba-transduce-sieve)
  (pez-ba-transduce-sieve 100)
  (quick-bench (doall (pez-ba-transduce-sieve 1000000)))
  (quick-bench (count (pez-ba-transduce-sieve 1000000)))
  (tufte/profile {}
                 (count (pez-ba-transduce-sieve 1000000))))

(defn pez-ba-areduce-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (drop 1
                        (areduce primes idx acc []
                                 (if (and (aget primes idx)
                                          (pos? (mod idx 2)))
                                   (conj acc idx)
                                   acc))))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))
(comment
  (run-test pez-ba-areduce-sieve)
  (areduce (boolean-array 10) i acc [] (if (even? i)
                                         (conj acc i)
                                         acc))
  (pez-ba-areduce-sieve 100)
  (quick-bench (doall (pez-ba-areduce-sieve 1000000)))
  (tufte/add-basic-println-handler! {})
  (tufte/profile {}
                 #_(count (pez-ba-sieve 1000000))
                 (quick-bench (doall (pez-ba-sieve 1000000)))))

(defn pez-ba-for-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (for [i (concat [2] (range 3 (inc n) 2))
                :when (aget primes i)]
            i)
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))

(comment
  (run-test pez-ba-for-sieve)
  (pez-ba-for-sieve 100)
  (quick-bench (doall (pez-ba-for-sieve 1000000)))
  (quick-bench (count (pez-ba-for-sieve 1000000)))
  (quick-bench (count (concat [2] (range 3 (inc 1000000) 2))))
  (tufte/profile {}
                 (count (pez-ba-for-sieve 1000000))))

(defn pez-ba-loop-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (loop [res []
                 i 3]
            (if (<= i n)
              (recur (if (aget primes i)
                       (conj res i)
                       res)
                     (+ i 2))
              (concat [2] res)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))

(comment
  (run-test pez-ba-loop-sieve)
  (pez-ba-loop-sieve 100)
  (quick-bench (doall (pez-ba-loop-sieve 1000000)))
  (quick-bench (count (pez-ba-loop-sieve 1000000)))
  (tufte/profile {}
                 (count (pez-ba-loop-sieve 1000000))))

(defn pez-ba-loop-transient-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (loop [res (transient [])
                 i 3]
            (if (<= i n)
              (recur (if (aget primes i)
                       (conj! res i)
                       res)
                     (+ i 2))
              (concat [2] (persistent! res))))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))

(comment
  (run-test pez-ba-loop-transient-sieve)
  (pez-ba-loop-transient-sieve 100)
  (quick-bench (doall (pez-ba-loop-transient-sieve 1000000)))
  (quick-bench (count (pez-ba-loop-transient-sieve 1000000)))
  (tufte/profile {}
                 (count (pez-ba-loop-transient-sieve 1000000))))

(defn pez-ba-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (concat '(2)
                  (filter #(aget primes %)
                          (range 3 (inc n) 2)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))

(comment
  (run-test pez-ba-loop-transient-sieve)
  (pez-ba-loop-transient-sieve 100)
  (quick-bench (doall (pez-ba-loop-transient-sieve 1000000)))
  (quick-bench (count (pez-ba-loop-transient-sieve 1000000)))
  (tufte/profile {}
                 (count (pez-ba-loop-transient-sieve 1000000))))

(defn pez-ba-pipeline-sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (let [to-ch (a/chan 1)
                from-ch (a/to-chan (concat [2] (range 3 (inc n) 2)))]
            (a/pipeline 4 to-ch (filter #(aget primes %)) from-ch)
            (a/<!! (a/into [] to-ch)))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))

(comment
  (def ^long n 1000000)
  (def ba (boolean-array n))
  (def prob 0.075)
  (def sample-size (long (* n prob)))
  (doseq [i (take sample-size
                  (random-sample 0.15 (range n)))]
    (aset ba i true))
  (def every-other (range 1 n 2))

  (let [n 1000000
        ba (boolean-array n)
        every-other (range 1 n 2)
        prob 0.15
        sample-size (long (* n prob))
        to-ch (a/chan 1)
        from-ch (a/to-chan every-other)]
    (doseq [i (take sample-size
                    (random-sample prob (range n)))]
      (aset ba i true))

    (println "loop")
    (quick-bench #_time
                 (count
                  (loop [res []
                         i 1]
                    (if (<= i n)
                      (recur (if (aget ba i)
                               (conj res i)
                               res)
                             (+ i 2))
                      res))))

    (println "loop-transient")
    (quick-bench #_time
                 (count
                  (loop [res (transient [])
                         i 1]
                    (if (<= i n)
                      (recur (if (aget ba i)
                               (conj! res i)
                               res)
                             (+ i 2))
                      (persistent! res)))))

    (println "loop-transient-unchecked")
    (quick-bench #_time
                 (count
                  (loop [res (transient [])
                         i 1]
                    (if (<= i n)
                      (recur (if (aget ba i)
                               (conj! res i)
                               res)
                             (unchecked-add-int i 2))
                      (persistent! res)))))

    (println "filter")
    (quick-bench #_time
                 (count
                  (filter #(aget ba %) every-other)))

    (println "transduce")
    (quick-bench #_time
                 (count
                  (transduce
                   (filter #(aget ba %))
                   conj
                   every-other)))

    (println "core.reducers/filter")
    (quick-bench #_time
                 (count
                  (into [] (r/filter #(aget ba %) every-other))))

    (println "core.async/pipeline")
    (quick-bench #_time
                 (do
                   (a/pipeline 4 to-ch (filter #(aget ba %)) from-ch)
                   (count
                    (a/<!! (a/into [] to-ch)))))

    (println "for")
    (quick-bench #_time
                 (count
                  (for [i every-other
                        :when (aget ba i)]
                    i))))

  


  (def tch (a/to-chan (range 10)))
  (def tv (a/into [] (a/to-chan (range 10))))
  (a/<!! (a/into [] (a/to-chan (range 10))))
  (a/close! tch)
  (a/<!! tv)
  (def test-ch (a/chan 1))
  (doseq [i (range 10)]
    (a/go (a/>! test-ch i)))
  (a/<!! (a/close! (a/into [] test-ch)))
  (def vs (a/into [] test-ch))
  (a/close! test-ch)
  (a/<!! vs)
  (run-test pez-ba-pipeline-sieve)
  (pez-ba-pipeline-sieve 100)
  (quick-bench (doall (pez-ba-pipeline-sieve 1000000)))
  (quick-bench (count (pez-ba-pipeline-sieve 1000000)))
  (quick-bench (count (concat [2] (range 3 (inc 1000000) 2))))
  (tufte/profile {}
                 (count (pez-ba-for-sieve 1000000))))

(defn pez-bs-sieve-2 [^long n]
  (let [primes (doto (java.util.BitSet. n) (.set 0 (inc n)))
        sqrt-n (long (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 2]
        (if-not (< p sqrt-n)
          (drop 2 (-> primes .stream .toArray))
          (do
            (loop [i (* p p)]
              (when (<= i n)
                (.clear primes i)
                (recur (+ i p))))
            (recur (.nextSetBit primes (+ p 1)))))))))
(comment
  (pez-bs-sieve-2 10)
  (tufte/add-basic-println-handler! {})
  (tufte/profile {}
                 #_(count (pez-ba-sieve 1000000))
                 (dotimes [_ 20] (doall (pez-bs-sieve-2 1000000)))
                 #_(quick-bench (doall (pez-bs-sieve-2 1000000)))))
(comment
  (pez-bs-sieve-2 10)

  (pez-ba-transduce-sieve 30)
  (pez-bs-sieve-2 7)
  (quick-bench (doall (pez-ba-sieve 1000000)))
  (quick-bench (doall (pez-ba-reduce-sieve 1000000)))
  (quick-bench (doall (pez-ba-reducer-sieve 1000000)))
  (quick-bench (doall (pez-ba-transduce-sieve 1000000)))
  (quick-bench (doall (pez-bs-sieve-2 1000000)))


  (run-test pez-ba-transduce-sieve)
  (run-test pez-bs-sieve-2))

(defn pez-bs-sieve-1 [^long n]
  (let [primes (java.util.BitSet. n)
        sqrt-n (long (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if-not (< p sqrt-n)
          (concat '(2)
                  (remove #(.get primes %)
                          (range 3 (inc n) 2)))
          (do
            (loop [i (* p p)]
              (when (<= i n)
                (.set primes i)
                (recur (+ i p p))))
            (recur (.nextClearBit primes (+ p 1)))))))))

(defn pez-bs-sieve [^long n]
  (let [primes (doto (java.util.BitSet. n) (.set 0 (inc n)))
        sqrt-n (long (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 2]
        (if-not (< p sqrt-n)
          (drop 2 (-> primes .stream .toArray))
          (do
            (loop [i (* p p)]
              (when (<= i n)
                (.clear primes i)
                (recur (+ i p))))
            (recur (.nextSetBit primes (+ p 1)))))))))
(comment
  (pez-bs-sieve 10)
  (run-test pez-bs-sieve)
  (quick-bench (doall (pez-bs-sieve 1000000)))
  (run-test pez-bs-sieve-2))

(defn em-transient-sieve [n]
  (let [limit (inc n)
        primes (transient (into [] (repeat limit true)))
        sqrt-n (int (Math/ceil (Math/sqrt n)))
        filter-multiple (fn [primes p]
                          (reduce #(assoc! %1 %2 false)
                                  primes
                                  (range (* p p) limit (* p 2))))
        sieve-prime (fn [primes p]
                      (if (get primes p)
                        (filter-multiple primes p)
                        primes))]
    (let [mask (reduce sieve-prime primes (range 3 sqrt-n 2))]
      (cons 2 (filter #(get mask %) (range 3 limit 2))))))

(defn mbjarland-bs-sieve-2 [end]
  (let [bs (BitSet. end)]
    (loop [x 2]
      (if (= x end)
        (drop 2 (-> (doto bs (.flip 0 end)) .stream .toArray))
        (do (loop [y (+ x x)]
              (when (< y end)
                (.set bs y)
                (recur (+ y x))))
            (recur (.nextClearBit bs (inc x))))))))
(comment
  (mbjarland-bs-sieve-2 7)
  (run-test mbjarland-bs-sieve-2))

(defn p-mbjarland-bs-sieve-2 [end]
  (let [bs (BitSet. end)
        sqrt-end (int (Math/ceil (Math/sqrt end)))]
    (loop [x 3]
      (if (>= x sqrt-end)
        (drop 1 (remove even? (-> (doto bs (.flip 0 end)) .stream .toArray)))
        (do (loop [y (+ x x)]
              (when (< y end)
                (.set bs y)
                (recur (+ y x))))
            (recur (.nextClearBit bs (inc x))))))))

(comment
  (p-mbjarland-bs-sieve-2 100)
  (mbjarland-bs-sieve-2 100)
  (pez-bs-sieve 100)
  (em-transient-sieve 100)
  (quick-bench (count (mbjarland-bs-sieve 1000000)))
  (quick-bench (count (mbjarland-bs-sieve-2 1000000)))
  (quick-bench (count (p-mbjarland-bs-sieve-2 1000000)))
  (quick-bench (count (pez-bs-sieve 1000000)))
  (quick-bench (count (pez-sieve 1000000)))
  (with-progress-reporting (quick-bench (doall (em-transient-sieve 1000000)) :verbose))
  (with-progress-reporting (quick-bench (doall (sieve 1000000)) :verbose))
  (with-progress-reporting (quick-bench (doall (pez-mbjarland-sieve 1000000)) :verbose))
  (with-progress-reporting (quick-bench (doall (mbjarland-sieve 1000000)) :verbose))
  (sieve 1)
  (sieve-2 2)
  (sieve-2 7)
  (sieve-2 11)
  (sieve-2 30)
  (sieve 30)
  (sieve-2 47)
  (sieve   47)
  (bs-sieve 10)
  (primes-below 1)
  (primes-below 2)
  (primes-below 3)
  (primes-below-2 30)
  (primes-below 47)

  (def ba (boolean-array 30 false))
  (aset ba 7 true)
  (nth (vec ba) 7)
  (nth ba 7)
  (remove #(aget ba %)
          (range 30))
  (boolean-array-sieve 30)


  (bitset-sieve-2 30)
  (bitset-sieve-2 47)
  (-> (bitset-sieve-2 30)
      (.cardinality))
  (time (str "boolean-array-sieve: " (boolean-array-sieve 1000000) " primes"))
  (time (str "boolean-array-sieve: " (count (boolean-array-sieve 1000000)) " primes"))
  (time (str "bitset-sieve: " (.cardinality (bitset-sieve-2 10000000)) " primes"))
  (time (str "primes-below: " (count (primes-below 1000000)) " primes"))
  (time (str "Stuart's sieve: " (count (stuart-sieve 1000000)) " primes"))
  (time (str "em's sieve: " (count (em-sieve 1000000)) " primes"))
  (time (str "mbjarland's sieve: " (count (mbjarland-sieve 1000000)) " primes"))
  (time (str "bs-sieve: " (count (bs-sieve 1000000)) " primes"))
  (time (str "@pez's sieve-first: " (count (sieve-first 1000000)) " primes"))
  (time (str "@pez's sieve: " (doall (sieve 1000000)) " primes"))
  (time (str "@pez's sieve-2: " (count (sieve-2 1000000)) " primes"))
  (time (str "@pez's sieve2: " (count (sieve2 1000000)) " primes"))
  (time (str "@pez's sieve1: " (count (sieve1 1000000)) " primes"))
  (time (str "@pez's sieve3: " (count (sieve3 1000000)) " primes"))
  (with-progress-reporting (quick-bench (.cardinality (bitset-sieve-2 100000)) :verbose))
  (with-progress-reporting (quick-bench (count (primes-below 100000)) :verbose))
  (with-progress-reporting (quick-bench (count (boolean-array-sieve 100000)) :verbose))
  (with-progress-reporting (quick-bench (boolean-array-sieve 100000) :verbose))
  (with-progress-reporting (quick-bench (count (sieve 100000)) :verbose))
  (with-progress-reporting (quick-bench (count (ba-sieve-2 100000)) :verbose))
  (time (str "ba-sieve: " (count (ba-sieve 1000000)) " primes"))
  (with-progress-reporting (quick-bench (count (ba-sieve 100000)) :verbose))
  (with-progress-reporting (quick-bench (ba-sieve-2 100000) :verbose))
  (time (str "ba-sieve-2: " (count (ba-sieve-2 1000000)) " primes"))
  (with-progress-reporting (quick-bench (count (ba-sieve-2 100000)) :verbose))
  (with-progress-reporting (quick-bench (sieve 1000000) :verbose))
  (time (str "sieve: " (count (sieve 1000000)) " primes"))
  (with-progress-reporting (quick-bench (count (sieve 1000000)) :verbose))
  (time (str "primes-below: " (count (primes-below 10000000)) " primes"))
  (with-progress-reporting (quick-bench (count (primes-below 100000)) :verbose))
  (time (str "primes-below-2: " (count (primes-below-2 1000000)) " primes"))
  (with-progress-reporting (quick-bench (count (primes-below-2 100000)) :verbose))
  (ba-sieve 30)
  (ba-sieve 47)
  (bs-sieve 30)
  (bs-sieve 47)
  (boolean-array-sieve 47)
  (time (str "ba-sieve: " (ba-sieve 1000000) " primes"))
  (time (str "ba-sieve: " (count  e (ba-sieve 1000000)) " primes"))
  (time (str "bs-sieve: " (count (bs-sieve 1000000)) " primes"))
  (time (str "boolean-array-sieve: " (count (boolean-array-sieve 1000000)) " primes"))
  (with-progress-reporting (quick-bench (count (boolean-array-sieve 100000)) :verbose))
  (with-progress-reporting (quick-bench (count (bs-sieve 100000)) :verbose))
  (with-progress-reporting (quick-bench (count (ba-sieve 100000)) :verbose)))


(comment
  (def tests (clojure.edn/read-string (slurp "etc/eratosthenes-tests.edn")))

  (with-open [w (clojure.java.io/writer "tests.edn")]
    (binding [*print-length* nil]
      (.write w "[")
      (.write w (clojure.string/join "\n  " (map pr-str tests)))
      (.write w "]"))))

(defmacro time2 [expr]
  `(let [t0# (System/nanoTime)
         r# ~expr]
     [(- (System/nanoTime) t0#) r#]))

(def allowed-time
  (* 30 1e9)) ;; 30 seconds in nanoseconds

(comment
 (defn run-test [f]
   (let [run? (atom true)]
     (doto (Thread.
            (fn []
              (loop [minutes 1]
                (Thread/sleep 60000)
                (when @run?
                  (println minutes "minute")
                  (recur (inc minutes))))))
       .start)
     (try
       (println "Running basic tests.")
       (doseq [[n ex] tests]
         (let [[t ac] (time2 (f n))]
           (assert (= ex ac) (format "Test failed for n=%d." n))
           (assert (<= t allowed-time) (format "Went over allowed time for n=%d." n))))
       (println "Passed tests. Running benchmark.")
       (quick-bench (count (f 100000))) ;; make sure it's realized if it's lazy
       (finally
         (reset! run? false))))
   :done))

(comment
  (run-test sieve)            ;   2.8 ms
  (run-test pez-bs-sieve)         ;   4.6 ms
  (run-test mbjarland-sieve)  ;   crash
  (run-test mbjarland-sieve-2)  ;   crash
  (run-test pez-mbjarland-sieve)  ;   crash
  (run-test primes-below)  ;   9.5 ms (if we remove the test for n=2, otherwise it fails)
  (run-test ba-sieve)      ;   6.1 ms
  (run-test primes-to-n)   ; 410   ms
  (run-test sieve1)        ; 125   ms
  (run-test sieve2)        ; 115   ms
  (run-test sieve-2))       ;  41   ms
  

(defn mirror [x]
  (loop [m 0
         p x]
    (if-not (> p 0)
      m
      (recur (+ (* m 10) (mod p 10)) (int (/ p 10))))))

(defn palprimes [n]
  (filter #(= % (mirror %)) (sieve n)))

(comment
  (int (/ 123 10))
  (mod 123 10)
  (mirror 12345)
  (sieve 1000)
  ((set [1 2 3]) 4)
  (count (palprimes 1000))
  (time (str "palprimes: " (count (palprimes 1000000)))))
