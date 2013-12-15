(ns scijors.engine.loader-test
  (:use [scijors.engine loader]
        [clojure.test])
  (:require [clojure.java.io :as io]))




(deftest get-root-test
  (is (= (get-root "/a/b/c/") "/a/b/"))
  (is (= (get-root "/a/b/c") "/a/b/"))
  (is (= (get-root "/a/b/") "/a/"))
  (is (= (get-root "/abc") "/"))
  (is (= (get-root "/") nil)))

(deftest relative-filename-test
  (is (= (relative-filename "/a/b/c/" "../d.txt") "/a/b/d.txt"))
  (is (= (relative-filename "/a/b/c/" "d.txt") "/a/b/c/d.txt"))
  (is (= (relative-filename "/a/b/c/" "./d.txt") "/a/b/c/d.txt"))
  (is (= (relative-filename "/a/b/c/" "/d.txt") "/d.txt"))
  (is (= (relative-filename "/a/b/c/" "a1/d.txt") "/a/b/c/a1/d.txt"))
  )


(defn test-template-loader
  ([s]
     (let [file (io/as-file (str "/Users/cosmi/projects/scijors/test-resources/" s))]
       (when (.exists file)
         file))))

(deftest template-test
  (let [template (load-template "content.html"
                                test-template-loader)]
    (is (= (slurp (test-template-loader "outputs/content1.html")) (template {:v 2 :a 1}))))
  (let [template (load-template "subcontent.html"
                                test-template-loader)]
    (is (= (slurp (test-template-loader "outputs/subcontent1.html")) (template {:v 2 :a 1}))))
  (let [template (load-template "wrapper.html"
                                test-template-loader)]
    (is (= (slurp (test-template-loader "outputs/wrapper1.html"))
           (template {:v 2 :a 2 :b :a})))))




(deftest multi-test
  (let [template (load-template "multi.html"
                                test-template-loader)]
    (is (= (slurp (test-template-loader "outputs/multi1.html")) (template {})))))

