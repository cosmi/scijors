(ns scijors.engine.filters
  (:use [scijors.engine expr markers escape]))


(deffilter :FilterSafe "FilterSafe = <'safe'>;" [expr tree]
  (mark-safe (compile-expr expr)))


(deffilter :FilterUnsafe "FilterUnsafe = <'unsafe'>;" [expr tree]
  (unmark-safe (compile-expr expr)))


(deffilter :FilterEscape "FilterEscape = <'esc'> | <'escape'>;" [expr tree]
  (let [expr (compile-expr expr)
        escape (get-current-escape-fun)]
   (let [ret (mark-safe
              (fn []
                (escape (str (expr)))))]
     (if (const? expr)
       (mark-safe (const (ret)))
       ret))))


(deffilter :FilterEscapeCustom "FilterEscapeCustom = (<'esc'> | <'escape'>) <ws> sym;"
  [expr [_ mode]]
  (let [mode (keyword mode)
        expr (compile-expr expr)
        escape (get-escape-fun mode)]
    (assert escape (str "No such mode: " mode))
    (let [ret (mark-safe
               (fn []
                 (escape (str (expr)))))]

      (if (const? expr)
        (mark-safe (const (ret)))
        ret))))
  

(deffilter :FilterMode "FilterMode = <'mode'> <ws> sym;" [expr [_ mode]]
  (let [mode (keyword mode)
        expr (with-escape-mode mode
               (compile-expr expr))]
      expr))


(deffilter :FilterFormat "FilterFormat = <'format'> <ws> String;" [expr [_ s]]
  (let [expr (compile-expr expr)
        s (compile-expr s)]
    (assert (const? s))
    (let [fmt (s)
          fun #(format fmt (expr))]
      (if (const? expr)
        (const (fun))
        fun))))

