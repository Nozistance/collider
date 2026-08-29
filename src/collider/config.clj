(ns collider.config
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.log :as log]))

(set! *warn-on-reflection* true)

(def defaults
  {:port 25565
   :motd "Powered by Collider"
   :max-players 20
   :view-distance 4
   :simulation-distance 2
   :chunk-send-rate 20
   :save-file "world.snapshot"
   :save-period-ms 120000})

(defn load-config
  ([] (load-config "config.edn"))
  ([path]
   (merge defaults
          (when (.exists (io/file (str path)))
            (edn/read-string (slurp (str path)))))))

(defn- render ^String [m]
  (str "{" (str/join "\n " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) m)) "}\n"))

(defn write-default!
  ([] (write-default! "config.edn"))
  ([path]
   (let [f (io/file (str path))]
     (when-not (.exists f)
       (try
         (spit f (render defaults))
         (log/info "wrote default config to" (.getPath f))
         (catch Exception e
           (log/info "could not write" (.getPath f) "-" (.getMessage e))))))))
