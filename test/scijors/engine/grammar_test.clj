(ns scijors.engine.grammar-test
  (:use scijors.engine.grammar
        clojure.test))

(deftest replace-or-append-test
  (let [tval [[:a 1] [:b 2] [:c 3]]]
   (is (= (replace-or-append
           tval :b 4)
          [[:a 1] [:b 4] [:c 3]]))
   (is (= (replace-or-append
           tval :d 4)
          [[:a 1] [:b 2] [:c 3] [:d 4]]))))
