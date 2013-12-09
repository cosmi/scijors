(ns scijors.core-test
  (:require [clojure.test :refer :all]
            [scijors.core :refer :all]
            [scijors.engine.loader :as loader])
  (:use scijors.core))

(set-default-loader! (-> loader/file-loader
                         (wrap-prefix "test-resources/")))

(deftest load-multi
  (is (= ((get-template "multi.html") {})
         (slurp "test-resources/outputs/multi1.html"))))


  
