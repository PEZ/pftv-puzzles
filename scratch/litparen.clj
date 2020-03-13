(ns litparen)

(def open-close [\) \(])

(str \newline)
(def alphabet
  (let [upper (range (int \))
                     (inc (int \()))
        lower (range (int \a)
                     (inc (int \z)))]
    (-> #{\space}
        (into (map char upper))
        (into (map char lower)))))

#_#{:foo :bar}

(def t #_#(str 1))