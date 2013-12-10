(ns scijors.engine.markers
  (:require [clojure.string :as strings])
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
  (vary-meta f assoc ::source-tree tree))

(defn get-source-tree [f]
  (-> f meta ::source-tree))


(defn assoc-source [tree filename string]
  (if-not (vector? tree) tree
          (let [metadata (meta tree)]
            (->
             (mapv #(assoc-source % filename string) tree)
             (with-meta (assoc metadata
                          ::source string
                          ::filename filename))))))
  


(defn get-source-string-data [tree]
  (let [m (meta tree)
        start (:instaparse.gll/start-index m)
        end (:instaparse.gll/end-index m)
        text (::source m)
        pres (strings/split-lines (subs text 0 (min (count text) (inc start))))
        ]
    {:col (count (last pres))
     :row (count pres)
     :text (subs text start end)
     :source text
     :filename (::filename m)}
    ))

(defn scijors-tree-exception [tree msg]
  (let [{:keys [col row text filename] :as data} (get-source-string-data tree)] 
    (ex-info (format "Error at file: \"%s\" row:%d, col:%d, starting with \"%s\":\n%s"
                     filename col row (subs text 0 (min (count text) 50))
                     msg)
             (merge data
                    {:tree tree :msg msg
                     :type :scijors-tree-exception}))))
