(ns build
  (:require [clojure.tools.build.api :as b]))

(defn javac [_]
  (b/javac {:src-dirs   ["src/main/java"]
            :class-dir  "target/classes"
            :basis      (b/create-basis {:project "deps.edn"})
            :javac-opts ["-proc:none" "--release" "25"]}))
