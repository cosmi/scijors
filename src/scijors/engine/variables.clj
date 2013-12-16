(ns scijors.engine.variables
  (:use [scijors.engine markers]))

;; At runtime:
(def ^:dynamic *input-scope*) ;; is {}
(def ^:dynamic *block-scope*) ;; is {}

;; At runtime & compile-time
(def ^:dynamic *current-filename* nil) ;; is string
(def ^:dynamic *current-block* nil) ;; is string/keyword

;; At compile-time:
(def ^:dynamic *loader* nil) ;; is fn
(def ^:dynamic *blocks* nil) ;; is (atom #{})
(def ^:dynamic *root*) ;; is (atom "")

(def ^:dynamic *dependencies* (atom {}))


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
(defn get-scope []
  *input-scope*)

(defn get-block [kword]
  (get *block-scope* kword))


(defmacro in-block [blockname & body]
  `(binding [*current-block* ~blockname]
     ~@body
     ))

(defmacro in-file [filename & body]
 `(binding [*current-filename* ~filename]
     ~@body
     ))







(defn register-block! [block-name tree content]
  (let [old-block (@*blocks* block-name)]
    (when (and old-block (let [filename (-> old-block meta :filename)]
                           (or (= filename *current-filename*) (not filename))))
      (throw (scijors-tree-exception  tree (str "Block redefined: " block-name)))))
  (let [filename *current-filename*
        new-content (->
                     (fn []
                       (binding [*current-filename* filename
                                 *current-block* block-name]
                         (content)))
                     (with-meta (meta content))
                     (vary-meta assoc :filename filename))]
    (swap! *blocks* assoc block-name new-content)))

(defn mark-block! [block-name]
  (swap! *blocks* update-in [block-name] identity))

