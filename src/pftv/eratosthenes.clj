(ns eratosthenes)


; 1. Create a list of consecutive integers from 2 through n: (2, 3, 4, ..., n).
; 2. Initially, let p equal 2, the smallest prime number.
; 3. Enumerate the multiples of p by counting in increments of p from 2p to n, and mark them in the list (these will be 2p, 3p, 4p, ...; the p itself should not be marked).
; 4. Find the first number greater than p in the list that is not marked. If there was no such number, stop. Otherwise, let p now equal this new number (which is the next prime), and repeat from step 3.
; 5. When the algorithm terminates, the numbers remaining not marked in the list are all the primes below n.

(defn not-divisable-by [n d]
  (when-not (integer? (/ n d))
    n))

(defn sieve [n]
  (loop [found-primes [2]
         candidates (range 3 (inc n))]
    (let [highest-found-prime (last found-primes)
          remaining (->> candidates
                         (map #(not-divisable-by % highest-found-prime))
                         (remove nil?))]
      (if (= remaining candidates)
        (concat found-primes candidates)
        (recur (conj found-primes (first remaining)) (rest remaining))))))

(comment
  (not-divisable-by 3 3)
  (sieve 500))