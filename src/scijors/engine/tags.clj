(ns scijors.engine.tags
  (:use [scijors.engine expr markers text variables elements commons]))


(defn- create-block-emitter [sym]
  (fn block-emitter []
    ((get-block sym))))

(deftag :TagBlock "
TagBlock = TagBlockBegin Content (<end> | <tag-open> <'endblock'> (<ws> <sym>)? <tag-close>) ;
TagBlockBegin = <tag-open> <'block'> <ws> sym
                 (<ws> <'with'> <ws> WithAssocList)? <tag-close>;"
  [_ [TagBlockBegin sym with-assoc-list]  content :as tree]
  (assert (= TagBlockBegin :TagBlockBegin))
  (in-block sym
            (let [content (compile-tags content)]
              (register-block! sym tree content)
              ;; TODO: if extends, the top level block should throw exception if defined with 'with'
              (cond->
               (create-block-emitter sym)
               with-assoc-list
               (wrap-assoc-list with-assoc-list)))))


(deftag :TagCallBlock "
TagCallBlock = <tag-open> <'callblock'> <ws> sym (<ws> <'with'> <ws> WithAssocList)? <tag-close>;"
  [_ sym with-assoc-list :as tree]

  ;; Mark that block
  (mark-block! sym)
  (cond->
     (fn block-emitter []
       (if-let [block-fn (get-block sym)]
         (in-block sym (block-fn))
         (throw (scijors-tree-exception tree (format "No such block: '%s'!" sym)))))
     with-assoc-list
     (wrap-assoc-list with-assoc-list)))


(deftag :TagDefBlock "
TagDefBlock = <tag-open> <'defblock'> <ws> sym <tag-close> Content (<end> | <tag-open> <'enddefblock'> (<ws> <sym>)? <tag-close>);"
  [_ sym content :as tree]
  (in-block sym
            (let [content (compile-tags content)]
              (register-block! sym tree content))
            nil))


(defn define-block-multi [[_ [_ sym expr with-assoc-list] default-content :as tree]]
  (in-block sym
            (let [default-content (compile-tags default-content)
                  expr (compile-expr expr)
                  dispatch-map (atom {})
                  block (-> (fn []
                              (let [dispatch-val (expr)
                                    dispatched-fun (@dispatch-map dispatch-val default-content)]
                                (dispatched-fun)))
                            (vary-meta assoc :dispatch-map dispatch-map))
                            
                           ]
              (register-block! sym tree block))))


(deftag :TagBlockMulti "
TagBlockMulti = TagBlockMultiBegin Content (<end> | <tag-open> <'endmultiblock'> (<ws> <sym>)? <tag-close>) ;
TagBlockMultiBegin = <tag-open> <'multiblock'> <ws> sym <ws><'on'><ws> Expr
                 (<ws> <'with'> <ws> WithAssocList)? <tag-close>;"
  [_ [TagBlockMultiBegin sym expr with-assoc-list :as begin-node] default-content :as tree]
  (assert (= TagBlockMultiBegin :TagBlockMultiBegin))
  (define-block-multi tree)
  (in-block sym
            ;; TODO: if extends, the top level block should throw exception if defined with 'with'
            (cond->
             (create-block-emitter sym)
             with-assoc-list
             (wrap-assoc-list with-assoc-list))))

(deftag :TagBlockMultiDef "
TagBlockMultiDef = TagBlockMultiDefBegin Content (<end> | <tag-open> <'enddefmultiblock'> (<ws> <sym>)? <tag-close>) ;
TagBlockMultiDefBegin = <tag-open> <'defmultiblock'> <ws> sym <ws><'on'><ws> Expr
                  <tag-close>;"
  [_ [TagBlockMultiBegin sym expr  :as begin-node] default-content :as tree]
  (assert (= TagBlockMultiBegin :TagBlockMultiDefBegin))
  (define-block-multi tree)
  nil)


(defn define-block-multi-extend [[_ [TagBlockMultiExtendBegin sym expr] content :as tree]]
  (in-block sym
            (let [content (compile-tags content)
                  expr (compile-expr expr)]
              (when (not (const? expr))
                (throw (scijors-tree-exception tree "Dispatch value has to be constant")))
              (extend-block! sym (expr) tree content))))

(deftag :TagBlockMultiExtend "
TagBlockMultiExtend = TagBlockMultiExtendBegin Content (<end> | <tag-open> <'endmultiblock'> (<ws> <sym>)? <tag-close>) ;
TagBlockMultiExtendBegin = <tag-open> <'multiblock'> <ws> sym <ws> <'extend'> <ws> Expr (<ws> <'with'> <ws> WithAssocList)? <tag-close>;"
  [_ [TagBlockMultiExtendBegin sym expr with-assoc-list] content :as tree]
  (assert (= TagBlockMultiExtendBegin :TagBlockMultiExtendBegin))
  (define-block-multi-extend tree)
  (in-block sym
            (cond->
             (create-block-emitter sym)
             with-assoc-list
             (wrap-assoc-list with-assoc-list))))

(deftag :TagBlockMultiDefExtend "
TagBlockMultiDefExtend = TagBlockMultiDefExtendBegin Content (<end> | <tag-open> <'enddefmultiblock'> (<ws> <sym>)? <tag-close>) ;
TagBlockMultiDefExtendBegin = <tag-open> <'defmultiblock'> <ws> sym <ws><'extend'><ws> Expr  <tag-close>;"
  [_ [TagBlockMultiDefExtendBegin sym expr with-assoc-list] content :as tree]
  (assert (= TagBlockMultiDefExtendBegin :TagBlockMultiDefExtendBegin))
  (define-block-multi-extend tree)
  nil)



(deftag :TagWith "
TagWith = <tag-open> (<'with'> | <'let'>) <ws> WithAssocList <tag-close> Content <BT> (<'end'> | <'endwith'> | <'endlet'>) <ET>;
"
  [_ assoc-list content]
  (-> content compile-tags (wrap-assoc-list assoc-list)))



(deftag :TagIf "
TagIf =  TagIfIf TagIfElsif* TagIfElse? <tag-open> (<'end'> | <'endif'>) <tag-close>;
<TagIfIf>= <tag-open> <'if'> <ws> Expr <tag-close> Content;
<TagIfElsif> = <tag-open> (<'elseif'> |<'elsif'> | <'elif'> | <'else'> <ws> <'if'>) <ws> Expr <tag-close> Content;
<TagIfElse> = <tag-open> 'else' <tag-close> Content ;"
  [_ & cases]
  (let [cases (partition-all 2 cases)
        cases (mapv (fn [[expr,content]]
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
                      exprs (mapv compile-expr exprs)
                      content (compile-tags content)]
                  (when-let [expr (some (complement const?) exprs)]
                    (throw (scijors-tree-exception expr "Not a constant")))
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

(deftag :TagId "
TagId = <BT> <'id'> <ET>;"
  [_]
  (constantly (hash *current-filename*)))

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
        expr (last start)
        assign (butlast start)
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
