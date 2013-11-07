(ns scijors.engine.elements-test
  (:use scijors.engine.elements
        clojure.test)
  (:require [instaparse.core :as insta]))

(def parser (insta/parser elements-grammar))

(deftest basic-elements
  (is (= '("abc$a?") (parser "abc$a?" :start :sym)))
  (is (= '("a0/abc$a?") (parser "a0/abc$a?" :start :ns-sym)))
  (is (= '("abc$a+?") (parser "\\abc$a+?\\" :start :sym)))
  (is (= '("a0/abc$a+?") (parser "\\a0/abc$a+?\\" :start :ns-sym)))
  (is (= '("a0.a1.a2") (parser "a0.a1.a2" :start :ns)))
  (is (= '("  ") (parser "  " :start :ws)))
  (is (= '("\"") (parser "\"" :start :dquote)))
  (is (not= '("//") (parser "//" :start :sym)))
  )
