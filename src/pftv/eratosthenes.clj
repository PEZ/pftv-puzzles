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

(defn sieve [n]
  (loop [found-primes [2 3]
         candidates (range 5 (inc n) 2)]
    (let [highest-found-prime (last found-primes)
          remaining (->> candidates
                         (map #(not-divisible-by % highest-found-prime))
                         (remove nil?))]
      (if (= remaining candidates)
        (concat found-primes candidates)
        (recur (concat found-primes (take 1 remaining)) (drop 1 remaining))))))

(comment
  (sieve 30)
  (time (str "mine: " (count (sieve 1000000)) " primes"))
  (with-progress-reporting (quick-bench (sieve 1000) :verbose)))

; As a refinement, it is sufficient to mark the numbers in step 3 starting from p2, as all the smaller multiples of p will have already been marked at that point. This means that the algorithm is allowed to terminate in step 4 when p2 is greater than n

(defn sieve-refined [n]
  (let [sqrt-n (Math/sqrt n)]
    (loop [found-primes (range 2 4)
           candidates (range 5 (inc n) 2)
           highest-found-prime 3]
      (if (> highest-found-prime sqrt-n)
        (concat found-primes candidates)
        (let [remaining (->> candidates
                             (map #(not-divisible-by % highest-found-prime))
                             (remove nil?))
              highest (take 1 remaining)]
          (recur (concat found-primes highest) (drop 1 remaining) (first highest)))))))

(comment
  (sieve-refined 30)
  (time (str "mine: " (count (sieve-refined 1000000)) " primes"))
  (with-progress-reporting (quick-bench (sieve-refined 10000) :verbose)))

(defn sieve-refined-2 [n]
  (let [sqrt-n (Math/sqrt n)]
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
             remaining)))))))

(comment
  (sieve-refined-2 50)
  (time (str "mine: " (count (sieve-refined-2 10000)) " primes"))
  (with-progress-reporting (quick-bench (sieve-refined-2 1000) :verbose)))

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
  (time (str "@abhilater's: " (count (primes-to-n 100000)) " primes"))
  (with-progress-reporting (quick-bench (count (primes-to-n 10000)) :verbose)))