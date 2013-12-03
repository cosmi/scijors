(ns scijors.engine.commons-test
  (:use [scijors.engine grammar variables commons]
        [clojure.test])
  (:require [instaparse.core :as insta]
            [clojure.string :as strings]))


(def parser (get-parser :start :AssocList))


(deftest assoc-list-test
  (with-input {}
    (is (= [[[:a] 4] [[:b] 5]] ((-> "a = 4, b = 5" parser parse-assoc-list))))
    (is (= [[[:a] 4] [[:b] 4]] ((-> "a = 4, b = a" parser parse-assoc-list))))
    (is (= [[[:a] 4] [[:b :c] 4]] ((-> "a = 4, b.c = a" parser parse-assoc-list))))
    (is (= {:a 4 :b {:c 4}} (apply-assoc-list-resp {} ((-> "a = 4, b.c = a" parser parse-assoc-list)))))
  ))
