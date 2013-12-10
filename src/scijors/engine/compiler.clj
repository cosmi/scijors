(ns scijors.engine.compiler
  (:use [ scijors.engine grammar text filters variables tags markers])
  (:require [instaparse.core :as insta])
  )





(defn compile-template [template-string]
  (binding [*template-params* (atom {})]
    (let [ast ((get-parser) template-string :start :Content)]
      (if (insta/failure? ast)
        (throw (Exception. (str "Parse error\n" (prn-str (insta/get-failure ast)))))
        (let [template (in-block *filename*
                                 (-> ast (assoc-source
                                          *filename*
                                          template-string)
                                     compile-tags))
              params @*template-params*]
          (-> params
              (assoc-in [:blocks *filename*] template)
              (assoc-in [:blocks :root] template)))))))





