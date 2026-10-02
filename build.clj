(ns build
  (:require [clojure.java.io :as io]
            [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def jar-file "target/collider.jar")
(def basis (delay (b/create-basis {:project "deps.edn"})))
(defn clean [_]
  (b/delete {:path class-dir})
  (b/delete {:path jar-file}))

(defn javac [_]
  (b/process {:command-args ["clojure" "-T:build" "javac"]
              :dir          "libs/data.long-map"})
  (b/javac {:src-dirs   ["src/main/java"]
            :class-dir  class-dir
            :basis      @basis
            :javac-opts ["-proc:none" "--release" "25"]}))

(defn- compile-clj []
  (b/compile-clj {:basis      @basis
                  :src-dirs   ["src/main/clojure"]
                  :class-dir  class-dir
                  :ns-compile '[collider.cli]
                  :java-opts  ["-Dclojure.compiler.direct-linking=true"]}))

(defn- uber []
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           :basis     @basis
           :main      'collider.cli
           :exclude   [".*\\.java$" ".*\\.cljs$"]}))

(defn- commit []
  (b/git-process {:git-args "rev-parse --short=11 HEAD"}))

(defn- write-build-info []
  (let [f (io/file class-dir "collider" "build.edn")]
    (io/make-parents f)
    (spit f (pr-str {:commit (commit)}))))

(defn release [_]
  (clean nil)
  (write-build-info)
  (javac nil)
  (compile-clj)
  (uber))

(defn- path [s] (.getCanonicalPath (io/file (str s))))

(defn tables [{:keys [out cache jar]}]
  (let [game @(requiring-resolve 'collider.data/game)
        args (cond-> ["clojure" "-M" "-m" "collider.tables.core"
                      "generate"
                      "--out" (path (or out "target/data"))
                      "--cache" (path (or cache (str "data/" game)))]
               jar (conj "--jar" (path jar)))
        {:keys [exit]} (b/process {:dir "tools/tables" :command-args args})]
    (when-not (zero? exit)
      (throw (ex-info "table generation failed" {:exit exit})))))
