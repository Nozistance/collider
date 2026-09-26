(ns collider.config
  "Server settings, read from config.edn."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.log :as log]
            [malli.core :as m]
            [malli.error :as me]))

(set! *warn-on-reflection* true)

(def defaults
  {:port                     25565
   :motd                     "Powered by Collider"
   :max-players              20
   :max-connections          256
   :view-distance            4
   :simulation-distance      2
   :compression-threshold    256
   :save-dir                 "world"
   :save-period-ms           300000
   :pause-when-empty-seconds 60})

(def ^:private Settings
  [:map {:closed true}
   [:port {:optional true} [:int {:min 1 :max 65535}]]
   [:motd {:optional true} :string]
   [:max-players {:optional true} [:int {:min 1}]]
   [:max-connections {:optional true} [:int {:min 1}]]
   [:view-distance {:optional true} [:int {:min 2 :max 32}]]
   [:simulation-distance {:optional true} [:int {:min 2 :max 32}]]
   [:compression-threshold {:optional true} [:int {:min -1}]]
   [:save-dir {:optional true} :string]
   [:save-period-ms {:optional true} [:int {:min 0}]]
   [:pause-when-empty-seconds {:optional true}
    [:int {:min 0}]]])

(defn- complaint [settings [k msgs]]
  (str k " " (str/join ", " msgs)
       (when (contains? settings k)
         (str ", got " (pr-str (get settings k))))))

(defn- checked
  "Returns the settings that fit the schema, or throws.
  The error holds a line per bad key."
  [path settings]
  (if-let [errors (me/humanize (m/explain Settings settings))]
    (throw (ex-info (str "invalid " path)
                    {:what (str path " has bad settings")
                     :why  (map #(complaint settings %) errors)}))
    settings))

(defn load-config
  ([] (load-config "config.edn"))
  ([path]
   (merge defaults
          (when (.exists (io/file (str path)))
            (checked path (edn/read-string (slurp (str path))))))))

(def world-keys
  "The settings the world value holds, read by the tick."
  [:view-distance :simulation-distance :max-players :motd])

(def ^:private fixed-keys [:port :save-dir])

(defn reload
  "Returns the settings of path laid under overlay, as :settings.
  The keys a running server cannot change keep their current
  values; the ones the file changes are named as :restart. Throws
  as load-config does when the file is bad."
  [current path overlay]
  (let [fresh (merge (load-config path) overlay)]
    {:settings (merge fresh (select-keys current fixed-keys))
     :restart  (filterv #(not= (current %) (fresh %)) fixed-keys)}))

(defn- entry [[k v]] (str (pr-str k) " " (pr-str v)))

(defn- render ^String [m]
  (str "{" (str/join "\n " (map entry m)) "}\n"))

(defn write-default!
  "Writes the default settings to path.
  Does nothing when a file is already there. Returns true
  when it wrote them."
  ([] (write-default! "config.edn"))
  ([path]
   (let [f (io/file (str path))]
     (when-not (.exists f)
       (try
         (spit f (render defaults))
         true
         (catch Exception e
           (log/warn "could not write" (.getPath f)
                     "-" (.getMessage e))
           false))))))
