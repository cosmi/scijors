(ns scijors.engine.loader
  (:use [scijors.engine compiler variables]
        [clojure.java.io :as io]))





(defn classpath-loader 
  [^String s]
  (io/resource
   (if (.startsWith s "/")
     (subs s 1) s)))

(defn file-loader 
  [^String s]
  (io/as-file
   (if (.startsWith s "/")
     (subs s 1) s)))


(defn is-url-file? [url]
  (when (or
         (and (= java.net.URL (class url))
              (= "file" (.getProtocol url)))
         (= java.io.File (class url)))
    (let [file (io/as-file url)]
      [file (.lastModified file)])))


(defn load-template
  ([filename]
     (load-template filename classpath-loader))
  ([filename resource-provider]
     (let [dependencies (atom #{})]
       (binding [*loader* (memoize
                           (fn [fname]
                             (if-let [res (resource-provider fname)]
                               (let [res
                                     (if (string? res)
                                       res
                                       (do
                                         (swap! dependencies conj res)
                                         (slurp res)))]
                                 res)
                               (throw (Exception. (str "No such file: " fname))))))]
         (let [prepared-template (compile-template filename)
               dependencies (->> @dependencies
                                 vec
                                 (map is-url-file?)
                                 doall
                                 (into {}))]
           (-> prepared-template
               (vary-meta assoc :dependencies dependencies)))))))

