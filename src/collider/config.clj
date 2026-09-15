(ns collider.config
  "Server settings, read from config.edn."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.log :as log]))

(set! *warn-on-reflection* true)

(def defaults
  {:port                  25565
   :motd                  "Powered by Collider"
   :max-players           20
   :max-connections       256
   :view-distance         4
   :simulation-distance   2
   :compression-threshold 256
   :save-dir              "world"
   :save-period-ms        300000})

(defn load-config
  "Returns the settings at path, over the defaults for whatever it leaves out."
  ([] (load-config "config.edn"))
  ([path]
   (merge defaults
          (when (.exists (io/file (str path)))
            (edn/read-string (slurp (str path)))))))

(defn- render
  "Returns the settings as text, one setting per line."
  ^String [m]
  (str "{" (str/join "\n " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) m)) "}\n"))

(defn write-default!
  "Writes the default settings to path, unless a file is already there.
   Returns true when it wrote them."
  ([] (write-default! "config.edn"))
  ([path]
   (let [f (io/file (str path))]
     (when-not (.exists f)
       (try
         (spit f (render defaults))
         true
         (catch Exception e
           (log/warn "could not write" (.getPath f) "-" (.getMessage e))
           false))))))
