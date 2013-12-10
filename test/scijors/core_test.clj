(ns scijors.core-test
  (:require [clojure.test :refer :all]
            [scijors.core :refer :all]
            [scijors.engine.loader :as loader])
  (:use scijors.core))

(set-default-loader! (-> loader/file-loader
                         (wrap-prefix "test-resources/")))

(deftest load-multi-test
  (is (= ((create-template "multi.html") {})
         (slurp "test-resources/outputs/multi1.html"))))

(deftest load-error-test
  (is (thrown? clojure.lang.ExceptionInfo ((create-template "error.html") {}))))

  
