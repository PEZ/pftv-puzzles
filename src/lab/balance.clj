(ns lab.balance)

(defn- scan [{:keys [opens _closes] :as tally} c]
  (case c
    \( (update tally :opens inc)
    \) (if (zero? opens)
         (update tally :closes inc)
         (update tally :opens dec))
    tally))

(defn- unbalances [s]
  (reduce scan {:opens 0 :closes 0} s))

(defn count-edits [s]
  (as-> s $
    (unbalances $)
    (+ (:opens $) (:closes $))))

(comment
  (unbalances ")")
  ;; => {:closes 1, :opens 0}
  (unbalances "(")
  ;; => {:closes 0, :opens 1}
  (unbalances "foo)(bar")
  ;; => {:closes 1, :opens 1}
  (unbalances "(foo)")
  ;; => {:closes 0, :opens 0}
  (unbalances ")))(((())(")
  ;; => {:closes 3, :opens 3}
  (count-edits ")))(((())(")
  ;; => 6
  )