(ns scijors.engine.expr
  (:use [scijors.engine grammar elements variables markers])
  (:require [instaparse.core :as insta]
            [clojure.string :as strings]))


(defgrammar const-grammar "
Nil = <'nil'> | <'null'>;
False = <'false'>;
True = <'true'>;
<Const> = Nil | False | True;
Integer = #'-?[0-9]+';
Ratio = #'-?[0-9]+' <'/'> #'[0-9]+';
Double = #'-?([0-9]+\\.[0-9]*|\\.[0-9]+)';
String = string;
Keyword = <':'> (sym | ns-sym);
<ConstItem> = Const | Integer | Ratio | Double | String | Keyword;")


(defgrammar var-grammar "
CljNSSymbol = ns-sym;
CljCoreSymbol = <'/'> sym ;
<CljSymbol> = CljNSSymbol | CljCoreSymbol;
Variable = sym;
Block = <'$'> sym;
<Symbol> = CljSymbol | Variable;
")

(defgrammar struct-grammar "
Set = <hashlbrace> ( (Expr (<comma> Expr)*) | (ConstItem (<clj-ws> ConstItem)*))?<rbrace>;
Vector = <lsqparen> ( (Expr (<comma> Expr)*) | (ConstItem (<clj-ws> ConstItem)*))?<rsqparen>;
Map = <lbrace> ((Expr <colon> Expr) (<comma> Expr <colon> Expr)* |
(ConstItem <clj-ws> ConstItem) (<clj-ws> ConstItem <clj-ws> ConstItem)* )? <rbrace>;
<Item> = ConstItem / Symbol / Map / Vector / Set;
<Expr> = Item | OpExpr;"
  )

(defgrammar op-grammar "
<OpExpr> = OpExpr00;
<OpExpr00> = OpExpr05;

<OpExpr05> = Or | OpExpr10;
Or = OpExpr10 ((<ws>? <'||'> <ws>? | <ws> <'or'> <ws>) OpExpr10) +;
<OpExpr10> = And | OpExpr20;
And = OpExpr20 ((<ws>? <'&&'> <ws>? | <ws> <'and'> <ws>) OpExpr20) +;

<OpExpr20> = Equal | EqualNum | NotEqual | NotEqualNum | GTE | GT | LTE | LT | OpExpr30;
Equal = OpExpr30 <ws>? <'=='> <ws>? OpExpr30;
NotEqual = OpExpr30 <ws>? <'!='> <ws>? OpExpr30;
GTE = OpExpr30 <ws>? <'>='> <ws>? OpExpr30;
GT = OpExpr30 <ws>? <'>'> <ws>? OpExpr30;
LTE = OpExpr30 <ws>? <'<='> <ws>? OpExpr30;
LT = OpExpr30 <ws>? <'<'> <ws>? OpExpr30;
EqualNum = OpExpr30 <ws>? <'==='> <ws>? OpExpr30;
NotEqualNum = OpExpr30 <ws>? <'!=='> <ws>? OpExpr30;

<OpExpr30> = Concat | OpExpr35;
Concat = OpExpr35 (<ws>? <'&'> <ws>? OpExpr35) +;

<OpExpr35> = Plus | OpExpr40;
Plus = OpExpr40 (<ws>? <'+'> <ws>? OpExpr40) +;

<OpExpr40> = Minus | OpExpr50;
Minus = OpExpr50 (<ws> <'-'> <ws>? OpExpr50) +;

<OpExpr50> = Mult | OpExpr60;
Mult = OpExpr55 (<ws>? <'*'> <ws>? OpExpr55) +;
<OpExpr55> = Modulo | OpExpr60;
Modulo = OpExpr60 (<ws>? <'%'> <ws>? OpExpr60) +;

<OpExpr60> = Div | DivInt | OpExpr70;
Div = OpExpr60 (<ws> <'/'> <ws> OpExpr70) +;
DivInt = OpExpr70 (<ws> <'//'> <ws> OpExpr70) +;

<OpExpr70> = UnaryMinus | Not | Deref | OpExpr80;
UnaryMinus = <'-'> !#'[0-9]' <ws>? OpExpr70;
Not = (<'!'> | <'not'> <ws>) <ws>? OpExpr70;
Deref = <'@'> <ws>? OpExpr70;

<OpExpr80> = Index | Call | DotIndex | OpExpr100;
Index = OpExpr80 <lsqparen> Expr (<comma> Expr)* <rsqparen>;
Call = OpExpr80 <lparen> (Expr (<comma> Expr)*)? <rparen>;
DotIndex = OpExpr80  (<ws>? <'.'> <ws>? sym )+;

<OpExpr100> = <'('> <ws>? SuperExpr <ws>? <')'> | Item;

FilteredExpr = SuperExpr <pipe> Filter;
<SuperExpr> = Expr | FilteredExpr;
Filter = #'(?!a)b';
")

;; (def expr-grammar
;;   (str elements-grammar
;;        const-grammar
;;        var-grammar
;;        struct-grammar
;;        op-grammar))


(def compile-expr)




(defmulti compile-expr-impl first)

(defmethod compile-expr-impl :default [input]
  (throw (Exception. (prn-str input))))

(defmethod compile-expr-impl :Integer [[_ val]]
  (const (Long/parseLong val)))

(defmethod compile-expr-impl :Double [[_ val]]
  (const (Double/parseDouble val)))

(defmethod compile-expr-impl :Nil [[_]]
  (const nil))
(defmethod compile-expr-impl :True [[_]]
  (const true))
(defmethod compile-expr-impl :False [[_]]
  (const false))
(defmethod compile-expr-impl :String [[_ val]]
  (const (unescape-string val)))
(defmethod compile-expr-impl :Keyword [[_ val]]
  (const (keyword val)))
(defmethod compile-expr-impl :Ratio [[_ a b]]
  (const (/ (Long/parseLong a) (Long/parseLong b))))

(defmacro def-op [type arg & body]
  `(defmethod compile-expr-impl ~type [[root# & ~arg]]
     (let [~arg (map compile-expr ~arg)]
       (if (every? const? ~arg)
         (const (do ~@body))
         (fn ~(symbol (str "make-" (strings/lower-case (name type))))
           [] ~@body)))))

(def-op :Vector els
  (mapv #(%) els))
(def-op :Set els
  (set (map #(%) els)))
(def-op :Concat els
  (apply str (map #(%) els)))
(def-op :Plus els
  (apply + (map #(%) els)))
(def-op :Minus els
  (apply - (map #(%) els)))
(def-op :Mult els
  (apply * (map #(%) els)))
(def-op :Modulo els
  (apply mod (map #(%) els)))
(def-op :Div els
  (apply / (map #(%) els)))
(def-op :DivInt els
  (reduce quot (map #(%) els)))

(def-op :Or els
  (loop [els els]
    (if (empty? els) false
        (or ((first els))
            (recur (rest els))))))
(def-op :And els
  (loop [els els]
    (if (= 1 (count els)) ((first els))
        (and ((first els))
             (recur (rest els))))))

(def-op :UnaryMinus els
  (- ((first els))))

(def-op :Not els
  (not ((first els))))


(def-op :Equal els
  (reduce = (map #(%) els)))
(def-op :EqualNum els
  (reduce == (map #(%) els)))
(def-op :NotEqual els
  (reduce not= (map #(%) els)))
(def-op :NotEqualNum els
  (reduce #(not (== %1 %2)) (map #(%) els)))
(def-op :GTE els
  (reduce >= (map #(%) els)))
(def-op :GT els
  (reduce > (map #(%) els)))
(def-op :LTE els
  (reduce <= (map #(%) els)))
(def-op :LT els
  (reduce < (map #(%) els)))


(defmethod compile-expr-impl :Map [[_ & els :as k]]
  (let [els (->> els
                (map compile-expr)
                (partition-all 2 ))
        {const-els true var-els false}
        (group-by (fn [[k,v]] (and (const? k) (const? v))) els)

        const-map (->> const-els
                       (map (fn [[k,v]] [(k) (v)]))
                       (into {}))]
    (if (empty? var-els)
      (const const-map)
      (fn make-map []
        (reduce (fn [m [k, v]] (assoc m (k) (v))) const-map var-els)
        ))))


(defmethod compile-expr-impl :CljCoreSymbol [[_ s]]
  (->> s (str "clojure.core/") symbol find-var deref const))

(defmethod compile-expr-impl :CljNSSymbol [[_ s]]
  (->> s symbol find-var deref const))

(defmethod compile-expr-impl :Variable [[_ s]]
  (let [kword (keyword s)]
    (case kword
      :_global
      (fn global []
        *input-scope*)
      (fn variable []
        (get-scope-variable kword)))))

(defmethod compile-expr-impl :Block [[_ s]]
  (let [kword (keyword s)]
    (fn variable []
      (get-block kword))))


(defmethod compile-expr-impl :Deref [[_ expr ]]
  (let [expr (compile-expr expr)]
    (fn deref-op []
      (deref (expr)))))


(defmethod compile-expr-impl :DotIndex [[_ expr & s]]
  (let [kword (mapv keyword s)
        expr (compile-expr expr)
        fun (fn dot-index []
              (get-in (expr) kword))]
    (if (const? expr)
      (const (fun))
      fun)))

(def-op :Index els
  (let [[v & i] (mapv #(%) els)]
    (get-in v i)))


(defmethod compile-expr-impl :Call [[_ expr & args]]
  (let [expr (compile-expr expr)
        args (mapv compile-expr args)]
    (fn call []
      (apply (expr) (map #(%) args)))))
  
(defn compile-expr [tree]
  (-> (compile-expr-impl (cond-> tree (seq? tree) first))
      (mark-source-tree tree)))





(defonce filters (atom {}))

(defn create-filter! [nom grammar fun]
  (let [grammar (-> grammar strings/trim)]
    (assert (.endsWith grammar ";"))
    (assert (or
             (.startsWith grammar (str (name nom) " "))
             (.startsWith grammar (str (name nom) "="))))
    (swap! filters assoc nom {:name nom :grammar grammar :fun fun})))

(defmacro deffilter [nom grammar args & body]
  `(let [grammar# ~grammar]
     (create-filter! ~nom grammar# (fn ~(-> nom name symbol) ~args ~@body))
     (defgrammar ~nom grammar#)
     ))

(defmethod compile-expr-impl :FilteredExpr [[_ expr [name & rst :as tree]]]
  (let [filter (@filters name)]
    (assert filter (str "No such filter: " name))
    ((filter :fun) expr tree)))


(defgrammar filter-list-grammar
  (fn filter-list-grammar []
    (when-let [filters (not-empty @filters)]
      (str "<Filter> = "
           (apply str
                  (concat
                   (interpose "|"
                              (for [[k,f] filters]
                                (-> f :name name)))))
           ";\n"
           ))))


