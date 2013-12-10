(ns scijors.core
  (:require [scijors.engine.loader :as loader]
            [clojure.string :as strings]
            [scijors.debug :as debug]))



(defn wrap-prefix [fun prefix]
  (fn [path]
    (fun (str prefix path))))

(def ^:private default-loader
  (->
   loader/classpath-loader
   (wrap-prefix "templates/")))


(defn set-default-loader! [new-loader]
  (alter-var-root #'default-loader (constantly new-loader))
  new-loader)

(defn prod-create-template [path loader]
  (loader/load-template path (or loader default-loader)))

(defn dev-create-template [path loader]
  (let [cache (atom (prod-create-template path loader))]
    (fn [data]
      (let [requested-deps
            (->> @cache meta :dependencies
                 (map (fn [[file ts]]
                        [file (.lastModified file)]))
                 (into {}))]
        ((swap! cache
                (fn [old]
                  (if (not= requested-deps (-> old meta :dependencies))
                    (prod-create-template path loader)
                    old)))
         data))
      )))




(defn create-template [path & {:keys [devmode loader] }]
  (if devmode
    (dev-create-template path loader)
    (prod-create-template path loader)))
