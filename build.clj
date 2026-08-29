(ns build
  (:require [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def prim-dir  "target/classes")
(def jar-file  "target/collider.jar")
(def basis     (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [_]
  (b/javac {:src-dirs   ["src"]
            :class-dir  prim-dir
            :basis      basis
            :javac-opts ["-proc:none" "--release" "21"]}))

(defn uber [_]
  (clean nil)
  (javac nil)
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis      basis
                  :src-dirs   ["src"]
                  :class-dir  class-dir
                  :ns-compile '[collider.server]
                  :java-opts  ["-Dclojure.compiler.direct-linking=true"]})
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           :basis     basis
           :main      'collider.server}))
