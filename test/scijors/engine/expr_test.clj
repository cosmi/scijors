(ns scijors.engine.expr-test
  (:use scijors.engine.expr
        clojure.test
        scijors.engine.variables)
  (:require [instaparse.core :as insta]))

(def parser (insta/parser expr-grammar :start :Expr))


(deftest constants-test
  (is (= [:Keyword "abc"] (parser ":abc" :start :Keyword)))
  (is (= [:Integer "40"] (parser "40" :start :Integer)))
  (is (= [:Ratio "30" "40"] (parser "30/40" :start :Ratio)))
  (is (= [:Double "40."] (parser "40." :start :Double)))
  (is (= [:Double ".04"] (parser ".04" :start :Double)))
  (is (= [:Double "40.04"] (parser "40.04" :start :Double)))
  (is (= [:String "abc"] (parser "\"abc\"" :start :String)))
  (is (= [:String "a\\\""] (parser "\"a\\\"\"" :start :String)))
  )



(deftest multiple-constants-test
  (is (= '([:Keyword "abc"]) (parser ":abc" :start :ConstItem)))
  (is (= '([:Integer "40"]) (parser "40" :start :ConstItem)))
  (is (= '([:Ratio "30" "40"]) (parser "30/40" :start :ConstItem)))
  (is (= '([:Double "40."]) (parser "40." :start :ConstItem)))
  (is (= '([:Double ".04"]) (parser ".04" :start :ConstItem)))
  (is (= '([:Double "40.04"]) (parser "40.04" :start :ConstItem)))
  (is (= '([:String "abc"]) (parser "\"abc\"" :start :ConstItem)))
  (is (= '([:String "a\\\""]) (parser "\"a\\\"\"" :start :ConstItem)))
  )

(deftest struct-test
  (is (= '([:Keyword "abc"]) (parser ":abc" :start :Expr)))
  (is (= '([:Map
            [:Keyword "a"] [:Integer "1"]
            [:Keyword "b"] [:Integer "2"]
            [:Keyword "c"] [:Integer "3"]
            ]) (parser "{:a:1, :b: 2, :c :3}" :start :Expr)))
  (is (= '([:Map
            [:Vector [:Integer "1"] [:Integer "2"]]
            [:Vector [:Integer "3"] [:Integer "4"]]])
         (parser "{[1, 2]: [3, 4]}" :start :Expr)))
  (is (= '([:Vector
            [:Vector [:Integer "1"] [:Integer "2"]]
            [:Vector [:Integer "3"] [:Integer "4"]]])
         (parser "[[1, 2], [3 4]]" :start :Expr)))
  )



(deftest compile-constants
  (let [tests {"4" 4 "8." 8. "-3" -3 "-.4" -0.4 "-2/4" -1/2
               "\"abc\"" "abc"  "\"a\\\"bc\"" "a\"bc"
               "nil" nil "null" nil "false" false "true" true ":a" :a
               ":a/a" :a/a
               "[1,2]" [1,2]
               "[]" []
               "{}" {}
               "{:a 1 :b 2}" {:a 1 :b 2}
               "{:a: [1, 2], :b: 2}" {:a [1, 2] :b 2}
               "false || nil" false
               "false && nil" false
               "1 && 2" 2
               "1 && false || 4" 4
               "1 + 2" 3
               "1 * 2 + 3 * 4" 14
               "1 / 2" 1/2
               "5 // 3" 1
               "5 - 3" 2
               "5 - 15." -10.
               "(1 + 2) * 2" 6
               "!(4 || 3)" false
               "3<4" true
               "3.===3" true
               "3.==3" false
               "3.===6/2" true
               "3&2" "32"
               "3&2&\"a\"" "32a"
               }]
    (doseq [[s v] tests]
      (testing (str "Parsing: " (prn-str s))
        (is (=  (-> s parser compile-expr const?)))
        (is (= v ((-> s parser compile-expr))))))))
  
