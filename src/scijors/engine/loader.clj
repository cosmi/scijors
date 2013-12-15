(ns scijors.engine.loader
  (:use [scijors.engine compiler variables]
        [clojure.java.io :as io]))


(defn get-root
  "Returns path minus the last segment (includes the trailing slash)."
  [^String path]
  (let [idx (.lastIndexOf path "/" (- (count path) 2))]
    (if (== idx -1)
      nil
      (subs path 0 (inc idx)))))

(defn relative-filename [^String root ^String subpath]
  {:pre [(.startsWith root "/") (.endsWith root "/")]}
  (cond
   (.startsWith subpath "/")
   subpath
   (.startsWith subpath "../")
   (if-let [root1 (get-root root)]
     (relative-filename root1 (subs subpath 3))
     (throw (ex-info "Cannot go up in path" {:root root :subpath subpath})))
   (.startsWith subpath "./")
   (relative-filename root (subs subpath 2))
   :else
   (str root subpath)))



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
                                 (map (fn [url]
                                        (when (or
                                               (and (= java.net.URL (class url))
                                                    (= "file" (.getProtocol url)))
                                               (= java.io.File (class url)))
                                          (let [file (io/as-file url)]
                                            [file (.lastModified file)]))))
                                 doall
                                 (into {}))]
           (-> prepared-template
               (vary-meta assoc :dependencies dependencies)))))))

