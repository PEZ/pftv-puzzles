#_#{:foo :bar}
#_^{:foo :bar}
#_ '(1 2)
#_'(1 2)
(def t #_#(str 1))

(defn foo []
  (let [start ()
        end (max start (- (<! (current-server-time link)) server-ts-deadzone))
        txs (<! (tx-between link start end))]))