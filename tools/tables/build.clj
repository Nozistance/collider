(ns build
  (:require [clojure.tools.build.api :as b]))

(def jar-file "target/collider-tables.jar")

(def classes "target/classes")

(defn uber [_]
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/delete {:path "target"})
    (b/compile-clj {:basis      basis
                    :src-dirs   ["src"]
                    :class-dir  classes
                    :ns-compile '[collider.tables.core]})
    (b/uber {:class-dir classes
             :uber-file jar-file
             :basis     basis
             :main      'collider.tables.core})))
