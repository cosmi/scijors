(ns scijors.core
  (:require [scijors.engine.loader :as loader]
            [clojure.string :as strings]))



(defn wrap-prefix [fun prefix]
  (fn [path]
    (fun (str prefix path))))


(defn- prod-create-template [path loader]
  (loader/load-template path loader))


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



(def ^:private reload-on-change true )

(defn set-reload-on-change! [mode]
  (alter-var-root #'reload-on-change (constantly (boolean mode)))
  mode)

(defn auto-create-template [path loader]
  (let [cache (atom (prod-create-template path loader))]
    (fn [data & [block]]
      (if-not reload-on-change
        (if block
              (@cache data block)
              (@cache data))
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
              (fun data))))))))

(defn- make-lazy [template-generator]
  (let [a (atom nil)]
    (fn [& args]
      (when @a (reset! a (template-generator)))
      (apply @a args)
      )))


(def ^:private default-loader
  (->
   loader/classpath-loader
   (wrap-prefix "templates/")))


(defn set-default-loader! [new-loader]
  (alter-var-root #'default-loader (constantly new-loader))
  new-loader)

(defn load-template [path & {:keys [mode devmode loader lazy]
                               :or {loader default-loader}}]
  (let [t
        (case (or mode (and devmode :dev) :auto)
          :dev
          #(dev-create-template path loader)
          :auto
          #(dev-create-template path loader)
          :prod
          #(prod-create-template path loader))]
    (if lazy
      (make-lazy t)
      (t))))

(def ^:deprecated create-template load-template)
