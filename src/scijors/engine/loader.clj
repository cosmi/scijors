(ns scijors.engine.loader
  (:use [scijors.engine compiler variables errors]
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

(defn- mixin-template [template mixin]
  (-> template
      (assoc :blocks (merge (dissoc (mixin :blocks) :root) (template :blocks)))))

(defn- extend-template [parent-template child-template]
   (-> child-template
       (assoc-in [:blocks :root] (get-in parent-template [:blocks :root]))))


(defn- prepare-template [filename template-provider]
  (let [template (template-provider filename)
        _ (prn :TMPL template)
        root (get-root filename)
        mixins (->> (template :mixins)
                    (map #(relative-filename root %))
                    (map template-provider )
                    reverse)
        template
        (if-not (template :extends)
          template
          (->
           (->> template :extends (relative-filename root) template-provider)
           (extend-template template)))
        template (reduce mixin-template template mixins)]
    template

    )
  )
  

(defn load-template
  ([filename]
     (load-template filename (fn [^String s]
                               {:pre [(.startsWith s "/")]}
                               (when-let [res (io/resource (subs s 1))]
                                 (slurp res)))))
  ([filename resource-provider]
     (let [get-template (memoize
                         (fn [fname]
                           (if-let [res (resource-provider fname)]
                             (compile-template res)
                             (throw (scijors-exception "No such file: " fname)))))
           prepared-template
           (prepare-template (relative-filename "/" filename) get-template)
           blocks (prepared-template :blocks)]
       (fn template
         ([input block]
            (binding [*block-scope* blocks
                      *input-scope* input]
              (prn *block-scope*)
              ((get-block block))
                          ))
         ([input] (template input :root))))))
     
