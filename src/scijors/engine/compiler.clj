(ns scijors.engine.compiler
  (:use [ scijors.engine grammar text filters variables tags markers commons elements])
  (:require [instaparse.core :as insta])
  )


(defn- wrap-filename [fun filename]
  (fn []
    (binding [*current-filename* filename]
      (fun))))

(defn wrap-block-source-filename [block-fn filename]
  (cond-> block-fn
          (-> block-fn meta :filename not)
          (->
           (wrap-filename filename)
           (vary-meta assoc :filename filename))))

(defn wrap-block-filenames [blocks filename]
  (into {} (for [[k,v] blocks] [k (wrap-block-source-filename v filename)])))

(defn prepare-template [template-path]
  (binding [*root* (atom template-path)
            *current-filename* template-path]
    (let [template-string (*loader* template-path)
          ast ((get-parser) template-string :start :Content)
          fun (-> ast (assoc-source
                       template-path
                       template-string)
                  compile-tags)]
      {:blocks  (wrap-block-filenames (assoc @*blocks* template-path fun) template-path)
       :root @*root*
       :path template-path})))

(defn wrap-template [{:keys [root blocks path] :as template}]
  (fn [input]
      (binding [*input-scope* input
                *block-scope* blocks
                *current-filename* path]
        ((get-block root)))))
    

(defn compile-template [template-path]
  (binding [*blocks* (atom {})]
    (-> (prepare-template template-path)
        wrap-template)))


(deftag :TagExtends "
TagExtends = <tag-open> <'extends'> <ws> string <tag-close>;
"
  [_ filename :as tree]
  (let [filename (unescape-string filename)]
    ;; check for double extends
    (when (not= @*root* *current-filename*)
      (throw  (scijors-tree-exception tree (str "Extend called twice or cannot extend here"))))
    (let [{:keys [root blocks]} (prepare-template filename)]
      (swap! *blocks* merge blocks)
      (reset! *root* root))
    nil))



(deftag :TagLoad "
TagLoad = <tag-open> (<'mixin'> | <'load'>) <ws> string <tag-close>;
"
  [_ filename]
  (let [filename (unescape-string filename)]
    (let [{:keys [root blocks]} (prepare-template filename)]
      (swap! *blocks* merge blocks))
    nil))


(deftag :TagRender "
TagRender = <tag-open> (<'include'> | <'render'>) <ws> string <tag-close>;
"
  [_ filename]
  (let [filename (unescape-string filename)]
    (let [template (compile-template)]
      (fn []
        (template (get-scope))))))
