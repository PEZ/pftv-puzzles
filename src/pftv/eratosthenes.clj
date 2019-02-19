(ns eratosthenes)

; From https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
; 1. Create a list of consecutive integers from 2 through n: (2, 3, 4, ..., n).
; 2. Initially, let p equal 2, the smallest prime number.
; 3. Enumerate the multiples of p by counting in increments of p from 2p to n, and mark them in the list (these will be 2p, 3p, 4p, ...; the p itself should not be marked).
; 4. Find the first number greater than p in the list that is not marked. If there was no such number, stop. Otherwise, let p now equal this new number (which is the next prime), and repeat from step 3.
; 5. When the algorithm terminates, the numbers remaining not marked in the list are all the primes below n.

(defn not-divisible-by [n d]
  (when-not (integer? (/ n d))
    n))

(defn sieve [n]
  (loop [found-primes [2]
         candidates (range 3 (inc n))]
    (let [highest-found-prime (last found-primes)
          remaining (->> candidates
                         (map #(not-divisible-by % highest-found-prime))
                         (remove nil?))]
      (if (= remaining candidates)
        (concat found-primes candidates)
        (recur (conj found-primes (first remaining)) (rest remaining))))))

(comment
  (sieve 1000))

; As a refinement, it is sufficient to mark the numbers in step 3 starting from p2, as all the smaller multiples of p will have already been marked at that point. This means that the algorithm is allowed to terminate in step 4 when p2 is greater than n

(defn sieve-refined [n]
  (let [sqrt-n (Math/sqrt n)]
    (loop [found-primes [2]
           candidates (range 3 (inc n))]
      (let [highest-found-prime (last found-primes)]
        (if (> highest-found-prime sqrt-n)
          (concat found-primes candidates)
          (let [remaining
                (->> candidates
                  (map #(not-divisible-by % highest-found-prime))
                  (remove nil?))]
            (recur (conj found-primes (first remaining)) (rest remaining))))))))

(comment
  (sieve-refined 1000))