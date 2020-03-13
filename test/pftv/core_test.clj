(ns pftv.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [pftv.core :as sut]))

(deftest a-test
  (testing "Silly stub"
    (is (= "nothing to see here" sut/foo))))
