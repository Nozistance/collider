(ns build
  (:require [clojure.java.io :as io]
            [clojure.tools.build.api :as b]))

(def jar-file "target/collider-tables.jar")

(def classes "target/classes")

(defn uber [_]
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/delete {:path "target"})
    (b/compile-clj {:basis      basis
                    :src-dirs   ["src"]
                    :class-dir  classes
                    :ns-compile '[collider.tables.core]})
    (let [f (io/file classes "collider" "tables" "sources.edn")]
      (load-file "src/collider/tables/stamp.clj")
      (spit f (pr-str ((resolve 'collider.tables.stamp/fingerprint)
                       "src"))))
    (b/uber {:class-dir classes
             :uber-file jar-file
             :basis     basis
             :main      'collider.tables.core})))
