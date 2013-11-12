(ns scijors.engine.markers

  )

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
