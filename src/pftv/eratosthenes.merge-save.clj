(ns eratosthenes)

; From https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
; 1. Create a list of consecutive integers from 2 through n: (2, 3, 4, ..., n).
; 2. Initially, let p equal 2, the smallest prime number.
; 3. Enumerate the multiples of p by counting in increments of p from 2p to n,
;    and mark them in the list (these will be 2p, 3p, 4p, ...;
;    the p itself should not be marked).
; 4. Find the first number greater than p in the list that is not marked.
;    If there was no such number, stop. Otherwise, let p now equal this
;    new number (which is the next prime), and repeat from step 3.
; 5. When the algorithm terminates, the numbers remaining not marked in the
;    list are all the primes below n.

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

; Refinement: The algorithm is allowed to terminate in step 4 when p2
; is greater than n

(defn sieve [n]
  (cond
    (< n 2) ()
    (< n 3) (range 2 3)
    :else (let [sqrt-n (Math/sqrt n)]
            (loop [known (range 2 4)
                   candidates (range 5 (inc n) 2)
                   p 3]
              (if-not (< p sqrt-n)
                (concat known candidates)
                (let [remaining (remove #(= 0 (rem % p)) candidates)
                      next-p (take 1 remaining)]
                  (recur (concat known next-p) (drop 1 remaining) (first next-p))))))))

(comment
  (sieve 47)
  (time (str "mine: " (count (sieve 1000000)) " primes"))
  (with-progress-reporting (quick-bench (sieve 100000) :verbose)))


(defn ba-sieve-2 [n]
  (let [half-n (int (/ n 2))
        primes (boolean-array half-n false)
        max-p (int (/ (Math/sqrt n) 2))]
    (loop [p 1]
      (if-not (< p max-p)
        #_primes
        (remove #(aget primes %)
                (range 2 half-n))
        (do
          (when-not (aget primes p)
            (loop [i (int (/ (*' p p) 2))]
              (if (< i half-n)
                (do
                  (aset primes i true)
                  (recur (+' i p))))))
          (recur (inc p)))))))


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
          (when (aget primes i)
            (doseq [j (range (* i i) n i)]
              ;; Not prime -- multiple of i!
              (aset primes j false)))
          (recur (inc i)))))))

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
              (if (< i n)
                (do
                  (aset primes i true)
                  (recur (+ i p))))))
          (recur (inc p)))))))

(defn bs-sieve [^long n]
  (let [primes (java.util.BitSet. n)
        sqrt-n (long (Math/ceil (Math/sqrt n)))]
    (loop [p 2]
      (if-not (< p sqrt-n)
        (remove #(.get primes %)
                (range 2 (inc n)))
        (do
          (loop [i (* p p)]
            (if (< i n)
              (do
                (.set primes i)
                (recur (+ i p)))))
          (recur (.nextClearBit primes (inc p))))))))

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

(comment
  (Math/ceil (Math/sqrt 47))
  (def ba (boolean-array 10 true))
  (Array/fill ba 2 true 3 true)

  (ba-sieve 30)
  (ba-sieve 47)
  (bs-sieve 30)
  (bs-sieve 47)
  (ba-sieve-2 47)
  (boolean-array-sieve 47)
  (time (str "ba-sieve: " (ba-sieve 1000000) " primes"))
  (time (str "ba-sieve: " (count (ba-sieve 1000000)) " primes"))
  (time (str "bs-sieve: " (count (bs-sieve 1000000)) " primes"))
  (time (str "boolean-array-sieve: " (count (boolean-array-sieve 1000000)) " primes"))
  (with-progress-reporting (quick-bench (count (boolean-array-sieve 100000)) :verbose))
  (with-progress-reporting (quick-bench (count (bs-sieve 100000)) :verbose))
  (with-progress-reporting (quick-bench (count (primes-below 100000)) :verbose))
  (with-progress-reporting (quick-bench (count (ba-sieve 100000)) :verbose)))

; As a refinement, it is sufficient to mark the numbers in step 3 starting
; from p2, as all the smaller multiples of p will have already been marked
; at that point. 

(defn sieve-refined-2 [n]
  (cond
    (< n 2) ()
    (< n 3) (range 2 3)
    :else (let [sqrt-n (Math/sqrt n)]
            (loop [iteration 1
                   known-primes (range 2 4)
                   candidates (range 5 (inc n) 2)]
              (let [next-eliminator (nth known-primes iteration)]
                (if (> next-eliminator sqrt-n)
                  (concat known-primes candidates)
                  (let [squared-idx (.indexOf candidates (* next-eliminator next-eliminator))
                        found-primes (take squared-idx candidates)
                        remaining (->> (drop (inc squared-idx) candidates)
                                       (map #(not-divisible-by % next-eliminator))
                                       (remove nil?))
                        primes (concat known-primes found-primes)]
                    (recur
                     (inc iteration)
                     primes
                     remaining))))))))

(comment
  (sieve-refined-2 50)
  (time (str "mine: " (count (sieve-refined-2 100000)) " primes"))
  (with-progress-reporting (bench (sieve-refined-2 100000) :verbose)))

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

