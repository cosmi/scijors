(ns scijors.engine.text-test
  (:use [scijors.engine grammar text filters markers]
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


(deftest if-tag-compiler
  (is (= "1" ( (tag-compiler "{% if true %}1{%endif%}"))))
  (is (= nil ( (tag-compiler "{% if false %}1{%endif%}"))))
  
  (is (= "1" ( (tag-compiler "{% if true %}1{%else%}0{%endif%}"))))
  (is (= "0" ( (tag-compiler "{% if false %}1{%else%}0{%endif%}"))))
  )
