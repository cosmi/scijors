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
     (let [file (io/as-file (str "test-resources/" s))]
       (when (.exists file)
         (slurp file)))))

(deftest template-test
  (let [template (load-template "content.html"
                                test-template-loader)]
    (is (= "<html><head></head><body>\nTest value: v=2\nThis val val: v=3\n</body></html>\n" (template {:v 2})))

    )
  (let [template (load-template "wrapper.html"
                                test-template-loader)]
    (is (= "<html><head></head><body>\nSome other val: x=\nSome another val: v=3\n</body></html>\n" (template {:v 2})))

    )
  )
