(ns scijors.engine.grammar
  (:require [instaparse.core :as insta]))



(defonce ^:private main-grammar (atom ()
                             ))

(defonce ^:private main-parser (atom nil))


(defn replace-or-append [l k v]
  (if (empty? l)
    [[k v]]
    (if (= k (ffirst l))
      (cons [k v] (rest l))
      (cons (first l) (replace-or-append (rest l) k v)))))



(defn extend-grammar [keyword s]
  (reset! main-parser nil)
  (swap! main-grammar
         replace-or-append keyword s))


(defmacro defgrammar [sym grammar]
  (let [sym (cond (symbol? sym)
                  (symbol (str *ns*) (name sym))
                  (keyword? sym)
                  (keyword (str *ns*) (name sym)))
        grammar-sym `grammar#]
    `(let [~grammar-sym ~grammar]
       ~@(when (symbol? sym)
           [`(def ~sym ~grammar-sym)])
       (extend-grammar '~sym ~grammar-sym))))

(defn- get-grammar []
  (->> @main-grammar
       (map second)
       (map #(if (fn? %) (%) %))
       (apply str)))


(defn get-parser
  ([& {:keys [start]}]
     (let [parser
           (swap! main-parser
            (fn [parser]
              (or parser
                  (insta/parser (get-grammar)))))]
       (if start
         (fn [i & {start1 :start}] (parser i :start (or start1 start)))
         parser))))
  
