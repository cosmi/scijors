(ns scijors.engine.tags
  (:use [scijors.engine text variables errors elements]))


(deftag :TagBlock "
TagBlock = TagBlockBegin Content (<end> | <tag-open> <'endblock'> (<ws> <sym>)? <tag-close>) ;
<TagBlockBegin> = <tag-open> <'block'> <ws> sym <tag-close>;"
  [_ sym & content]
  (let [content (compile-tags content)]
    (when-not (nil? (get-in @*template-params* [:blocks sym]))
      (throw (scijors-exception "Block redefined: " sym {:block sym})))
    (swap! *template-params* assoc-in [:blocks sym] content)
    (fn block-emitter []
      ((get-block sym)))
    ))


(deftag :TagCallBlock "
TagCallBlock = <tag-open> <'block'> <ws> sym <tag-close>;"
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



