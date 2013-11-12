(ns scijors.engine.compiler-test
  (:use [scijors.engine compiler variables text]
        clojure.test
        ))

(defn compile-and-run [s & [inp]]
  (with-input (or inp {})
    ((get-in (compile-template s) [:blocks :root]))))

(deftest simple-input-test
  (is (= "4" (compile-and-run "{{4}}")))
  (is (= "4" (compile-and-run "4")))
  (is (= "44" (compile-and-run "4{{4}}")))
  (is (= "result: 72" (compile-and-run  "result: {{a+70}}" {:a 2})))
  (is (= "&lt;hr&gt;" (compile-and-run  "{{ \"<hr>\"}}"  {:a 2})))
  (is (= "<hr>" (compile-and-run  "{{( \"<hr>\" | safe)}}"  {:a 2})))
  (is (= "&lt;hr&gt;" (compile-and-run  "{{ \"<hr>\" | esc}}"  {:a 2})))
  
  (is (= "&lt;hr&gt;" (compile-and-run  "{{ \"<hr>\" | esc html}}"  {:a 2})))

  )
