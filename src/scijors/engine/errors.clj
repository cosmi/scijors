(ns scijors.engine.errors)


(defn scijors-exception [& content]
  (if (-> content last map?)
    (ex-info (->> content butlast (apply str)) (last content))
    (ex-info (apply str content) {})))
