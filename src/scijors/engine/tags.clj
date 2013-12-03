(ns scijors.engine.tags
  (:use [scijors.engine text variables errors elements commons]))


(deftag :TagBlock "
TagBlock = TagBlockBegin Content (<end> | <tag-open> <'endblock'> (<ws> <sym>)? <tag-close>) ;
TagBlockBegin = <tag-open> <'block'> <ws> sym
                  (<ws> <'with'> <ws> ('only' <ws>)?  AssocList)? <tag-close>;"
  [_ [TagBlockBegin sym only? assoc-list] & content]
  (assert (= TagBlockBegin :TagBlockBegin))
  (let [assoc-list (cond->> assoc-list
                            (and only? (not= only? "only"))
                            (cons only?))
        only? (when (and only? (= only? "only")))
        assoc-list (when assoc-list (parse-assoc-list assoc-list))
        content (compile-tags content)]
    (when-not (nil? (get-in @*template-params* [:blocks sym]))
      (throw (scijors-exception "Block redefined: " sym {:block sym})))
    (swap! *template-params* assoc-in [:blocks sym] content)
    ;; TODO: if extends, the top level block should throw exception if defined with 'with'

    (if assoc-list
      (fn block-emitter []
        (let [assc (assoc-list)]
          (with-scope (apply-assoc-list-resp
                       (if only? {} (get-scope)) assc)
            ((get-block sym)))))
      (fn block-emitter []
        ((get-block sym))))
    ))


(deftag :TagCallBlock "
TagCallBlock = <tag-open> <'callblock'> <ws> sym <tag-close>;"
  [_ sym & content]
  (let [content (compile-tags content)]
    (when-not (nil? (get-in @*template-params* [:blocks sym]))
      (throw (scijors-exception "Block redefined: " sym {:block sym})))
    (swap! *template-params* assoc-in [:blocks sym] content)
    (fn block-emitter []
      ((get-block sym)))
    ))



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



