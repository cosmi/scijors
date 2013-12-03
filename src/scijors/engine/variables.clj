(ns scijors.engine.variables)


(def ^:dynamic *input-scope*)

(def ^:dynamic *block-scope*)

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


(def ^:dynamic *template-params*)


