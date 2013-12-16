(ns scijors.engine.compiler
  (:use [ scijors.engine grammar text filters variables tags markers commons elements])
  (:require [instaparse.core :as insta])
  )


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


(defn- wrap-filename [fun filename]
  (fn []
    (binding [*current-filename* filename]
      (fun))))


(defn prepare-template
  ([template-path]
     (let [template-path (cond->> template-path
                          (not (.startsWith template-path "/"))
                          (str "/"))]
       (if-let [ret (@*dependencies* template-path)]
         ret
         (let [ret (binding [*root* (atom template-path)
                                 *current-filename* template-path]
                         (let [template-string (*loader* template-path)
                               ast ((get-parser) template-string :start :Content)
                               _ (when (insta/failure? ast)
                                   (throw (Exception. (prn-str ast))))
                               fun (-> ast (assoc-source
                                            template-path
                                            template-string)
                                       compile-tags)]
                           {:blocks  (assoc @*blocks* template-path fun)
                            :root @*root*
                            :path template-path}))]
           (swap! *dependencies* assoc template-path ret)
           ret))))
  ([context path]
     (prepare-template (relative-filename (get-root context) path))))

(defn wrap-template [{:keys [root blocks path] :as template}]
  (fn
    ([input]
      (binding [*input-scope* input
                *block-scope* blocks]
        ((get-block root))))
    ([input block]
      (binding [*input-scope* input
                *block-scope* blocks]
        ((get-block block))))))
    

(defn compile-template
  ([template-path]
     (binding [*blocks* (atom {})
               *dependencies* (atom {})]
       (-> (prepare-template template-path)
           wrap-template)))
  ([context path]
     (compile-template (relative-filename (get-root context) path))))



(deftag :TagExtends "
TagExtends = <tag-open> <'extends'> <ws> string <tag-close>;
"
  [_ filename :as tree]
  (let [filename (unescape-string filename)]
    ;; check for double extends
    (when (not= @*root* *current-filename*)
      (throw  (scijors-tree-exception tree (str "Extend called twice or cannot extend here"))))
    (let [{:keys [root blocks]} (prepare-template *current-filename* filename)]
      (swap! *blocks* merge blocks)
      (reset! *root* root))
    nil))



(deftag :TagLoad "
TagLoad = <tag-open> (<'mixin'> | <'load'>) <ws> string <tag-close>;
"
  [_ filename]
  (let [filename (unescape-string filename)]
    (let [{:keys [root blocks]} (prepare-template *current-filename* filename)]
      (swap! *blocks* merge blocks))
    nil))


(deftag :TagRender "
TagRender = <tag-open> (<'include'> | <'render'>) <ws> string <tag-close>;
"
  [_ filename]
  (let [filename (unescape-string filename)]
    (let [template (compile-template *current-filename* filename)]
      (fn []
        (template (get-scope))))))
