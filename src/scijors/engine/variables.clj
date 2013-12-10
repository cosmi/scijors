(ns scijors.engine.variables
  (:use [scijors.engine markers]))


(def ^:dynamic *input-scope*)

(def ^:dynamic *block-scope*)

(def ^:dynamic *template-params*)


(def ^:dynamic *filename* nil)
(def ^:dynamic *block*)


(defmacro with-input [input & body]
  `(binding [*input-scope* ~input]
     ~@body
    )
  )

(defn get-block [block-name]
  (get *block-scope* block-name))

(defmacro with-scope [input & body]
  `(binding [*input-scope* ~input]
     ~@body
    )
  )

(defn get-scope-variable [kword]
  (get *input-scope* kword)
  )
(defn get-scope []
  *input-scope*)

(defn get-block [kword]
  (get *block-scope* kword))


(defmacro in-block [blockname & body]
  `(binding [*block* ~blockname]
     ~@body
     ))

(defmacro in-file [filename & body]
 `(binding [*filename* ~filename]
     ~@body
     ))

(defn register-block! [block-name tree content]
  (when-not (nil? (get-in @*template-params* [:blocks block-name]))
    (throw (scijors-tree-exception  tree (str "Block redefined: " block-name))))
  (swap! *template-params* assoc-in [:blocks block-name] content))


