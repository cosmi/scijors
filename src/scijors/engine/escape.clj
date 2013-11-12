(ns scijors.engine.escape
  (:use [scijors.engine markers])
  )



(defn escape-html
  "Change special characters into HTML character entities."
  [text]
  (.. #^String text
    (replace "&" "&amp;")
    (replace "<" "&lt;")
    (replace ">" "&gt;")
    (replace "\"" "&quot;")
    (replace "'"  "&#39;")
    (replace "`"  "&#96;")))



(def escape-funs (atom {}))

(defn add-escape-fun! [mode fun]
  (swap! escape-funs assoc mode fun))


(add-escape-fun! :html escape-html)


(def ^:dynamic *escape-mode* :html)


(defn get-current-escape-fun []
  (@escape-funs *escape-mode*))
(defn get-escape-fun [mode]
  (@escape-funs mode))

;; (defn mark-escaped [f]
;;   (-> f (vary-meta assoc :escaped *escape-mode*)))

(defmacro with-escape-mode [mode & body]
  `(binding [*escape-mode* ~mode]
     (assert (@escape-funs ~mode) (str "No such mode: " ~mode))
     ~@body))


(defn wrap-escape
  ([expr mode]
     (if (-> expr safe?)
       expr
       (let [escape (get-escape-fun mode)
             fun (fn wrap-escape []
                   (escape (str (expr))))]
         (if (const? expr)
           (mark-safe (const (fun)))
           (mark-safe fun)))))
  ([expr]
     (wrap-escape expr *escape-mode*)))

