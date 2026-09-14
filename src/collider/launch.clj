(ns collider.launch
  (:require [clojure.edn :as edn]
            [collider.data :as data]
            [collider.tables :as tables])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main [& args]
  (when-not (data/dir)
    (let [{:keys [jar]} (apply merge {} (map edn/read-string args))]
      (tables/generate! {:out "data" :jar jar})))
  (apply (requiring-resolve 'collider.core/-main) args))
