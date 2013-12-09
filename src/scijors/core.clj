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

(def ^:private templates-cache (atom {}))

(defn get-cached-template [path]
  (get @templates-cache path))

(defn reload-cache "Reloads all cached templates using current default-loader."
  [cache-data]
  (->>
   (for [[path template] cache-data]
     [path
      (loader/load-template path default-loader)])
   (into {})))

(defn set-default-loader! [new-loader & {:keys [reload-cache keep-cache]}]
  (alter-var-root #'default-loader (constantly new-loader))
  (when-not keep-cache
    (if reload-cache
      (swap! templates-cache reload-cache)
      (reset! templates-cache {})
      ))
  new-loader)

(defn devmode-get-template [path loader]
  (-> templates-cache
      (swap!
       (fn [cache]
         (let [cached (get cache path)
               requested-deps
               (->> cached meta :dependencies
                    (map (fn [[file ts]]
                           [file (.lastModified file)]))
                    (into {}))]
           (if (-> cached meta :dependencies (= requested-deps))
             cache
             (let [template (loader/load-template path loader)]
               (cond-> cache template
                       (assoc path template )))))))
      (get path)))

(defn get-template [path & {:keys [devmode loader no-cache] :or {loader default-loader}}]
  (cond no-cache
        (loader/load-template path loader)
        devmode
        (devmode-get-template path loader)
        :else
        (if-let [cached (when-not no-cache (get-cached-template path))]
          cached
          (-> templates-cache
              (swap!
               (fn [cache]
                 (if (get cache path)
                   cache
                   (let [template (loader/load-template path loader)]
                     (cond-> cache
                             template
                             (assoc path template))))))
              (get path)))))
        
