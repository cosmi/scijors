(ns scijors.engine.text-test
  (:use [scijors.engine text markers]
        [clojure.test]))


(defn tag-compiler [s]
  (->> ((get-parser) s :start :Tag) compile-tag))


(deftest single-tag-compiler
  (is (const? (tag-compiler "{{1}}")))
  (is (safe? (tag-compiler "{{1 | safe}}")))
  (is (= "1" ( (tag-compiler "{{1}}"))))
  (is (= "<hr>" ( (tag-compiler "{{\"<hr>\" | safe}}"))))
  
  (is (= "&lt;hr&gt;" ( (tag-compiler "{{\"<hr>\" | safe |unsafe}}"))))
  
  (is (safe? (tag-compiler "{{a}}")))
  (is (safe? (tag-compiler "{{a |safe}}")))
  )
