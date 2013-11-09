(ns scijors.engine.variables)


(def ^:dynamic *input-scope*)


(defmacro with-input [input & body]
  `(binding [*input-scope* ~input]
     ~@body
    )
  )

(defmacro with-scope [input & body]
  `(binding [*input-scope* ~input]
     ~@body
    )
  )

(defn get-scope-variable [kword]
  (get *input-scope* kword)
  )
