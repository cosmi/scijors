(ns scijors.engine.tags
  (:use [scijors.engine expr markers text variables errors elements commons]))




(deftag :TagBlock "
TagBlock = TagBlockBegin Content (<end> | <tag-open> <'endblock'> (<ws> <sym>)? <tag-close>) ;
TagBlockBegin = <tag-open> <'block'> <ws> sym
                 (<ws> <'with'> <ws> WithAssocList)? <tag-close>;"
  [_ [TagBlockBegin sym with-assoc-list]  content]
  (assert (= TagBlockBegin :TagBlockBegin))
  (let [content (compile-tags content)]
    (when-not (nil? (get-in @*template-params* [:blocks sym]))
      (throw (scijors-exception "Block redefined: " sym {:block sym})))
    (swap! *template-params* assoc-in [:blocks sym] content)
    ;; TODO: if extends, the top level block should throw exception if defined with 'with'

    (cond->
     (fn block-emitter []
       ((get-block sym)))
     with-assoc-list
     (wrap-assoc-list with-assoc-list))))


(deftag :TagCallBlock "
TagCallBlock = <tag-open> <'callblock'> <ws> sym (<'with'> <ws> WithAssocList)? <tag-close>;"
  [_ sym with-assoc-list]
  (cond->
     (fn block-emitter []
       (try
         ((get-block sym))
         (catch NullPointerException e
           (throw (Exception. (format "No such block: '%s'" (name sym)) )))))
     with-assoc-list
     (wrap-assoc-list with-assoc-list)))


(deftag :TagDefBlock "
TagDefBlock = <tag-open> <'defblock'> <ws> sym <tag-close> Content (<end> | <tag-open> <'enddefblock'> (<ws> <sym>)? <tag-close>);"
  [_ sym content]
  (when-not (nil? (get-in @*template-params* [:blocks sym]))
    (throw (scijors-exception "Block redefined: " sym {:block sym})))
  (swap! *template-params* assoc-in [:blocks sym] content)
  nil)




(deftag :TagExtends "
TagExtends = <tag-open> <'extends'> <ws> string <tag-close>;
"
  [_ filename]
  (let [filename (unescape-string filename)]
    (when-not (nil? (get @*template-params* :extends))
      (throw (scijors-exception "Extend declared second time: " filename {:extend filename})))
    (swap! *template-params* update-in [:mixins] conj filename)
    (swap! *template-params* assoc :extends filename)
    nil))



(deftag :TagLoad "
TagLoad = <tag-open> (<'mixin'> | <'load'>) <ws> string <tag-close>;
"
  [_ filename]
  (let [filename (unescape-string filename)]
    (swap! *template-params* update-in [:mixins] conj filename)
    nil))




(deftag :TagWith "
TagWith = <tag-open> (<'with'> | <'let'>) <ws> WithAssocList <tag-close> Content <BT> (<'end'> | <'endwith'> | <'endlet'>) <ET>;
"
  [_ assoc-list content]
  (-> content compile-tags (wrap-assoc-list assoc-list)))



(deftag :TagIf "
TagIf =  TagIfIf TagIfElsif* TagIfElse? <tag-open> (<'end'> | <'endif'>) <tag-close>;
<TagIfIf>= <tag-open> <'if'> <ws> Expr <tag-close> Content;
<TagIfElsif> = <tag-open> (<'elsif'> | <'elif'> | <'else'> <ws> <'if'>) <ws> Expr <tag-close> Content;
<TagIfElse> = <tag-open> 'else' <tag-close> Content ;"
  [_ & cases]
  (let [cases (partition-all 2 cases)
        cases (map (fn [[expr,content]]
                     [(if (= expr "else") (constantly true) (compile-expr expr))
                      (compile-tags content)]) cases)]
    (reduce (fn [last-fn [expr content]]
              (fn []
                (if (expr) (content)
                    (last-fn)))) (constantly nil) (reverse cases))))



(deftag :TagSwitch "
TagSwitch =  <BT> <'switch'> <ws> Expr (<ET> <ws> <BT> | <ws>) TagSwitchCase (<BT> TagSwitchCase)* TagSwitchElse? <BT> (<'end'>|<'endswitch'>) <ET>;
TagSwitchCase = <'case'> <ws> Expr (<comma> Expr)* <ET> Content;
TagSwitchElse = <BT> <'else'> <ET> Content ;"
  [_ expr & cases]
  (let [[cases else] (if (-> cases last first (= :TagSwitchElse))
                       [(butlast cases) (last cases)] [cases])
        cases (->>
               (for [[TagSwitchCase expr & rst] cases]
                (let [exprs (cons expr (butlast rst))
                      content (last rst)
                      exprs (map compile-expr exprs)
                      content (compile-tags content)]
                  (when-let [expr (some (complement const?) exprs)]
                    (throw (scijors-exception (get-source-tree expr) "Not a constant")))
                  (mapv #(vector (%) content) exprs)))
               (apply concat)
               (into {}))
        expr (compile-expr expr)
        else (when else
               (compile-tags (second else)))]
    (fn []
      (let [val (expr)
            fun (or (get cases val)
                    else
                    (throw (scijors-tree-exception (get-source-tree expr) (str "No matching clause: " (prn-str val)))))]
        (fun)))))



(deftag :TagComment "
TagComment = <'{#'> (<#'#[^}]'> | <#'[^#]+'>)*   <'#}'>;"
  [_]
  nil)


(deftag :TagVerbatim "
TagVerbatim = <'{%%'> (#'%%[^}]' | #'%[^%]' | #'[^%]+')* <'%%}'>;"
  [_ & content]
  (constantly (apply str content)))

(deftag :TagFor "
TagFor = TagForStart Content (TagForElse | TagForInterpose TagForElse?)? <BT> (<'end'> | <'endfor'>) <ET>;
TagForStart = <BT> <'for'> <ws> (TagForSym (<ws> TagForIndex)? | TagForIndex) <ws> <'in'> <ws> Expr <ET>;
TagForSym = sym;
TagForIndex = <'index'> <ws> sym;
TagForElse = <BT> (<'else'> | <'empty'>) <ET> Content;
TagForInterpose = <BT> <'interpose'> <ET> Content;
"
  [_ [_ & start] content & extras]
  (let [content (compile-tags content)
        extras (into {} extras)
        interp (extras :TagForInterpose)
        interp (when interp (compile-tags interp))
        else (extras :TagForElse)
        else (when else (compile-tags else))
        [expr & assign] (reverse start)
        expr (compile-expr expr)
        assign (into {} assign)
        variable (assign :TagForSym)
        index (assign :TagForIndex)
        variable (when variable (keyword variable))
        index (when index (keyword index))]
    (fn for-loop []
      (if-let [v (not-empty (expr))]
        (let [fun (fn [idx v]
                    (with-scope
                      (cond-> (get-scope)
                          variable (assoc variable v)
                          index (assoc index idx))
                      (content)))
              interp (when interp (interp))]
          (->>
           (cond->>
            (if (map? v)
             (map #(fun (key %) (val %)) v)
             (map-indexed fun v))
            interp (interpose interp))
           (apply str)))
        (when else (else))))))
           

;; TODO:
;; - gen-id
