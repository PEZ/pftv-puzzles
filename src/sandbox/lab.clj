(ns sandbox.lab
  (:require [clojure.core.async :refer [go]]))

(comment
  (go
    (while true
      (println "hello")
      (Thread/sleep 1000))))