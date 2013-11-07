(ns scijors.engine.variables)


(def ^:dynamic *input-scope*)


(defmacro with-input [input & body]
  `(binding [*input-scope* input]
     ~@body
    )
  )
