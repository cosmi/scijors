(ns scijors.debug
  (:use [scijors.core]
        [scijors.engine loader markers]))


(def error-template (create-template "scijors/templates/error.html" :loader classpath-loader))

(defn wrap-scijors-error [fun]
  (fn [data]
    (try (fun data)
         (catch clojure.lang.ExceptionInfo e
           (when-not (-> e ex-data :type :scijors-tree-exception)
             (throw e))
           (error-template (assoc (ex-data e) :ex-msg (.getMessage e)))
           ))))
