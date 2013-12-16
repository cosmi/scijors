(ns scijors.engine.text
  (:use [scijors.engine
         grammar
         [elements]
         variables
         expr
         markers
         escape])
  (:require [instaparse.core :as insta]
            [clojure.string :as strings]))


(defonce tags (atom {}))

(defgrammar content-grammar
  "Content = (Tag+ | '');")

(defgrammar tag-grammar
  (fn tag-grammar []
    (when-let [tags (not-empty @tags)]
      (str "<Tag> = "
           (->> tags vals (map :name) (map name) (interpose " | ") (apply str))
           ";\n"
           ))))


(defn create-tag! [nom grammar fun]
  (assert (-> grammar strings/trim (.endsWith ";")))
  (swap! tags assoc nom {:name nom :grammar grammar :fun fun}))

(defmacro deftag [nom grammar args & body]
  `(let [grammar# (strings/trim ~grammar)]
     (assert (.startsWith grammar# ~(name nom)) "Tag grammar does not start with tag rule")
     (create-tag! ~nom grammar# (fn ~(-> nom name symbol) [~args] ~@body))
     (defgrammar ~nom grammar#)))


(deftag :Text
             "Text = #'[^{]+' | #'\\{[^#%{]';"
             [_ txt]
             (const txt))

(deftag :ExprTag
             "ExprTag = <dbl-lbrace> SuperExpr <dbl-rbrace>;"
             [_ expr]
             (let [expr (compile-expr expr)]
               (wrap-escape expr)))


(defn compile-tag [tree]
  (when-let [tag-fn (let [tree (cond-> tree (seq? tree) first )
                          [tagname & rst] tree
                          tag (@tags tagname)]
                      (assert tag (str "No such tag: " tagname))
                      ( (tag :fun) tree))]
    (-> tag-fn
        (mark-source-tree tree))))



(defn compile-tags [tags-list]
  (let [tags (-> tags-list
                 (cond->> (= :Content (first tags-list))
                          (drop 1))
                 (->>
                  (map compile-tag)
                  (remove nil?)
                  doall
                  vec))]
    (fn template []
      (->> tags
           (map #(%))
           (apply str)))))
             



