(ns scijors.engine.markers

  )
;; TODO: rename to scijors.engine.metadata?

(defn mark-const [f]
  (vary-meta f assoc ::const true))

(defn const? [f]
  (-> f meta ::const boolean))

(defn const [f]
  (-> f constantly mark-const))


(defn safe? [f]
  (-> f meta ::safe boolean))

(defn mark-safe [f]
  (vary-meta f assoc ::safe true))

(defn unmark-safe [f]
  (vary-meta f dissoc ::safe))


(defn mark-source-tree [f tree]
  (vary-meta f assoc ::source tree))

(defn get-source-tree [f]
  (-> f meta ::source))




(defn scijors-tree-exception [tree msg]
  (ex-info (format "Error: '%s' caused by: %s" msg tree) {:tree tree :msg msg
                                                          :type :scijors-tree-exception}))
