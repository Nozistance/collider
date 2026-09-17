(ns collider.launch
  "Game data generation before the first server start."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.cli :as cli]
            [collider.data :as data]
            [collider.tables :as tables])
  (:import (java.io File)
           (clojure.lang ExceptionInfo))
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- child-command ^String/1 [opts]
  (into-array String [(str (System/getProperty "java.home") "/bin/java")
                      "--sun-misc-unsafe-memory-access=allow"
                      "-cp" (System/getProperty "java.class.path")
                      "clojure.main" "-m" "collider.tables" (pr-str opts)]))

(defn- event [^String line]
  (when (str/starts-with? line "{")
    (try (edn/read-string line)
         (catch Exception _ nil))))

(defn- relay! [^Process p]
  (with-open [r (io/reader (.getInputStream p))]
    (reduce (fn [noise line]
              (if-let [m (event line)]
                (do (cli/render! m)
                    (when (= :error (:event m)) (System/exit 1))
                    noise)
                (conj noise line)))
            [] (line-seq r))))

(defn- child-died [noise]
  (ex-info "generation failed"
           {:what "data generator failed"
            :why  (str "Its last words: " (str/join " / " (take-last 3 noise)))}))

(defn- generate! [opts]
  (let [p (.start (doto (ProcessBuilder. (child-command opts))
                    (.redirectErrorStream true)))
        noise (relay! p)]
    (when-not (zero? (.waitFor p))
      (throw (child-died noise)))))

(defn- first-run! [args]
  (let [{:keys [jar]} (apply merge {} (map edn/read-string args))]
    (cli/render! {:event :intro :version tables/version
                  :stale? (.isDirectory (io/file "data"))})
    (generate! {:out "data" :jar jar})
    (cli/render! {:event :data-ready})))

(defn -main [& args]
  (when-not (data/dir)
    (try (first-run! args)
         (catch ExceptionInfo e
           (cli/render! (assoc (ex-data e) :event :error))
           (System/exit 1))))
  (apply (requiring-resolve 'collider.core/-main) args))
