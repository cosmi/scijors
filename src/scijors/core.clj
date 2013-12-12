(ns scijors.core
  (:require [scijors.engine.loader :as loader]
            [clojure.string :as strings]))



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
    (fn [data & [block]]
      (let [requested-deps
            (->> @cache meta :dependencies
                 (map (fn [[file ts]]
                        [file (.lastModified file)]))
                 (into {}))]
        (let [fun (swap! cache
                         (fn [old]
                           (if (not= requested-deps (-> old meta :dependencies))
                             (prod-create-template path loader)
                             old)))]
          (if block
            (fun data block)
            (fun data)))))))



(defn create-template [path & {:keys [devmode loader] }]
  (if devmode
    (dev-create-template path loader)
    (prod-create-template path loader)))
