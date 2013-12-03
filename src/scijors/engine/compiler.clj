(ns scijors.engine.compiler
  (:use [ scijors.engine grammar text filters variables tags])
  (:require [instaparse.core :as insta])
  )





(defn compile-template [template]
  (binding [*template-params* (atom {})]
    (let [ast ((get-parser) template :start :Content)]
      (if (insta/failure? ast)
        (throw (Exception. (str "Parse error\n" (prn-str (insta/get-failure ast)))))
        (let [template (compile-tags ast)
              params @*template-params*]
          (assoc-in params [:blocks :root] template))))))





