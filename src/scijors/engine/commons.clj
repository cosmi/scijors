(ns scijors.engine.commons
  (:use [scijors.engine
         grammar
         [elements :only [elements-grammar]]
         variables
         expr
         markers
         escape])
  (:require [instaparse.core :as insta]
            [clojure.string :as strings]))


(defgrammar assoc-list-grammar "
AssocVar = sym (<ws>? <'.'> <ws>? sym)*;
AssocExpr = AssocVar <ws>? <'='> <ws>? Expr;
<AssocList> = AssocExpr (<comma> AssocExpr)*
WithAssocList = (('only' <ws>)?  AssocList);")




(defn parse-assoc-list [lst]
  (if (empty? lst)
    (constantly nil)
    (let [[[AssocExpr [AssocVar & syms] expr] & lst] lst
          syms (mapv keyword syms)
          expr (compile-expr expr)]
      (assert (= :AssocVar AssocVar))
      (assert (= :AssocExpr AssocExpr))
      
      (let [subfn (parse-assoc-list lst)]
        (fn assoc-list []
          (let [v (expr)
                sublist
                (with-scope
                  (assoc-in (get-scope) syms v)
                  (subfn))]
            (cons [syms v] sublist)))))))


(defn apply-assoc-list-resp [input lst]
  (reduce (fn [m [ks v]]
            (assoc-in m ks v)) input lst))



(defn wrap-assoc-list [fun WithAssocList]
  (let [[WithAssocList only? & assoc-list] WithAssocList
        assoc-list (cond->> assoc-list
                            (and only? (not= only? "only"))
                            (cons only?))
        only? (when (and only? (= only? "only")))
        assoc-list (when assoc-list (parse-assoc-list assoc-list))]
    (assert (= WithAssocList :WithAssocList))
    (if only?
      (fn assoc-list-wrapper []
        (let [assc (assoc-list)]
          (with-scope (apply-assoc-list-resp {} assc)
            (fun))))
      (fn assoc-list-wrapper []
        (let [assc (assoc-list)]
          (with-scope (apply-assoc-list-resp (get-scope) assc)
            (fun)))))))
