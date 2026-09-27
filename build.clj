(ns build
  (:require [clojure.java.io :as io]
            [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def prim-dir "target/classes")
(def jar-file "target/collider.jar")
(def basis (b/create-basis {:project "deps.edn"}))
(defn clean [_]
  (b/delete {:path class-dir})
  (b/delete {:path jar-file}))

(defn javac [_]
  (b/javac {:src-dirs   ["src"]
            :class-dir  prim-dir
            :basis      basis
            :javac-opts ["-proc:none" "--release" "25"]}))

(defn- log-bytes [n] ((requiring-resolve 'collider.log/human-bytes) n))
(defn- info [& args] (apply (requiring-resolve 'collider.log/info) args))
(defn- step [doing done f] ((requiring-resolve 'collider.log/step) doing done f))

(defn- compile-clj []
  (b/compile-clj {:basis      basis
                  :src-dirs   ["src"]
                  :class-dir  class-dir
                  :ns-compile '[collider.core]
                  :java-opts  ["-Dclojure.compiler.direct-linking=true"]}))

(defn- uber []
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           :basis     basis
           :main      'collider.core
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
  (step "Compiling java" "Compiled java" #(javac nil))
  (step "Compiling clojure" "Compiled clojure" #(compile-clj))
  (step "Packing the jar" "Packed the jar" #(uber))
  (info jar-file (log-bytes (.length (io/file jar-file)))))
