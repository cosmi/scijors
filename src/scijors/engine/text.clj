(ns scijors.engine.text
  (:use [scijors.engine
         [elements :only [elements-grammar]]
         variables
         expr
         markers
         escape])
  (:require [instaparse.core :as insta]
            [clojure.string :as strings]))


(defonce tags (atom {}))

(def ^:private get-grammar
  (memoize (fn get-grammar [tags]
             (apply str
                    "<Content> = Tag*;"
                    (get-expr-grammar)
                    "<Tag> = "
                    (concat
                     (->> tags vals (map :name) (map name) (interpose " | "))
                     [";"]
                     (->> tags vals (map :grammar)))))))


(defn create-tag! [nom grammar fun]
  (assert (-> grammar strings/trim (.endsWith ";")))
  (swap! tags assoc nom {:name nom :grammar grammar :fun fun}))

(defmacro deftag [nom grammar args & body]
  `(create-tag! ~nom ~grammar (fn ~(-> nom name symbol) [~args] ~@body)))


(create-tag! :Text
             "Text = #'[^{]+' | #'\\{[^#%{]';"
             (fn text [[_ txt]]
               (const txt)))

(create-tag! :ExprTag
             "ExprTag = <dbl-lbrace> SuperExpr <dbl-rbrace>;"
             (fn expr-tag [[_ expr]]
               (let [expr (compile-expr expr)]
                 (wrap-escape expr))))


(defn compile-tag [tree]
  (let [tree (cond-> tree (seq? tree) first )
        [tag & rst] tree
        tag (@tags tag)]
    (assert tag (str "No such tag: " tag))
    ( (tag :fun) tree)))

(def ^:private get-parser-memo
  (memoize (fn get-parser-memo [tags]
             (insta/parser (get-grammar tags) :start :Content))))


(defn get-parser []
  (get-parser-memo @tags))




(defn compile-tags [tags-list]
  (let [tags (->> tags-list
                  (map compile-tag)
                  (remove nil?)
                  vec)]
    (fn template []
      (->> tags
           (map #(%))
           (apply str)))))
             



