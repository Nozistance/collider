(ns collider.config
  "Server settings and their reload."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.log :as log]
            [malli.core :as m]
            [malli.error :as me])
  (:import (java.util Random)))

(set! *warn-on-reflection* true)

(def file
  "config.edn")

(def defaults
  {:port                     25565
   :motd                     "Powered by Collider"
   :max-players              20
   :max-connections          256
   :view-distance            4
   :simulation-distance      2
   :compression-threshold    256
   :encryption               false
   :save-dir                 "world"
   :commit-period-ms         10000
   :pause-when-empty-seconds 60
   :game-mode                :creative
   :force-game-mode          false
   :level-seed               ""})

(def ^:private Settings
  [:map {:closed true}
   [:port {:optional true} [:int {:min 1 :max 65535}]]
   [:motd {:optional true} :string]
   [:max-players {:optional true} [:int {:min 1}]]
   [:max-connections {:optional true} [:int {:min 1}]]
   [:view-distance {:optional true} [:int {:min 2 :max 32}]]
   [:simulation-distance {:optional true} [:int {:min 2 :max 32}]]
   [:compression-threshold {:optional true} [:int {:min -1}]]
   [:encryption {:optional true} :boolean]
   [:save-dir {:optional true} :string]
   [:commit-period-ms {:optional true} [:int {:min 0}]]
   [:pause-when-empty-seconds {:optional true}
    [:int {:min 0}]]
   [:game-mode {:optional true}
    [:enum :survival :creative :adventure :spectator]]
   [:force-game-mode {:optional true} :boolean]
   [:level-seed {:optional true} [:or :int :string]]
   [:plugins {:optional true} [:map-of :keyword :any]]])

(defn- complaint [v [k msgs]]
  (str k " " (str/join ", " msgs)
       (when (contains? v k) (str ", got " (pr-str (get v k))))))

(defn complaints
  "Returns the lines that say where v breaks schema, or nil when
  v fits it."
  [schema v]
  (when-let [errors (me/humanize (m/explain schema v))]
    (map #(complaint v %) errors)))

(defn- checked [path settings]
  (if-let [why (complaints Settings settings)]
    (throw (ex-info (str "invalid " path)
                    {:what (str path " has bad settings") :why why}))
    settings))

(defn load-config
  "Returns the settings in path over the defaults. Throws when
  they are bad."
  ([] (load-config file))
  ([path]
   (merge defaults
          (when (.exists (io/file (str path)))
            (checked path (edn/read-string (slurp (str path))))))))

(defn parsed-seed
  "Returns the seed that level-seed v names, a whole number as it is
  and text as the game reads it, or nil when v is blank."
  [v]
  (if (int? v)
    v
    (let [s (.trim ^String v)]
      (when-not (.isEmpty s)
        (try (Long/parseLong s)
             (catch NumberFormatException _ (long (.hashCode s))))))))

(defn level-seed
  "Returns the seed of a new world under settings cfg, a random one
  when its level-seed is blank."
  ^long [cfg]
  (or (parsed-seed (:level-seed cfg ""))
      (.nextLong (Random.))))

(def world-keys
  "The settings that the world holds for the tick."
  [:view-distance :simulation-distance :max-players :motd
   :game-mode :force-game-mode])

(def ^:private fixed-keys [:port :save-dir :plugins])

(defn reload
  "Returns the settings of path with overlay on top as :settings.
  Keys that a running server cannot change keep their values and are
  named in :restart when they differ. Throws when the config is bad."
  [current path overlay]
  (let [fresh (merge (load-config path) overlay)]
    {:settings (merge fresh (select-keys current fixed-keys))
     :restart  (filterv #(not= (current %) (fresh %)) fixed-keys)}))

(defn- entry [[k v]] (str (pr-str k) " " (pr-str v)))

(defn- render ^String [m]
  (str "{" (str/join "\n " (map entry m)) "}\n"))

(defn write-default!
  "Writes the default settings to path unless path exists.
  Returns true when it writes them."
  ([] (write-default! file))
  ([path]
   (let [f (io/file (str path))]
     (when-not (.exists f)
       (try
         (spit f (render defaults))
         true
         (catch Exception e
           (log/warn "could not write" (str f) "-" (ex-message e))
           false))))))

(def settable
  "The settings a command may write, the plugins aside."
  (into [] (comp (map first) (remove #{:plugins}))
        (rest (rest Settings))))

(def ^:private text-keys #{:motd :save-dir :level-seed})

(defn value-of
  "Returns the value of setting k that text spells."
  [k ^String text]
  (if (text-keys k)
    text
    (let [v (edn/read-string text)]
      (if (symbol? v) (keyword v) v))))

(defn- ordered [m]
  (let [rank (zipmap (keys defaults) (range))]
    (sort-by #(rank (key %) (count rank)) m)))

(def ^:private value-re "(\"(?:[^\"\\\\]|\\\\.)*\"|[^\\s,}\\]]+)")

(defn- key-re [k]
  (re-pattern (str "(?m)^([^;\\n]*?[\\s{,])"
                   (java.util.regex.Pattern/quote (str k))
                   "(\\s+)" value-re)))

(defn- in-place
  "Returns config text with the value of k set to v, its comments and
  layout kept, or with the pair added before the last brace."
  ^String [^String text k v]
  (let [m (re-matcher (key-re k) text)]
    (if (.find m)
      (str (subs text 0 (.start m 3)) (pr-str v)
           (subs text (.end m 3)))
      (let [i (str/last-index-of text "}")]
        (str (subs text 0 i) "\n " (pr-str k) " " (pr-str v)
             (subs text i))))))

(defn written!
  "Writes setting k as v into config path and returns whether the
  running server takes a restart for it. Comments in the file stay.
  Throws, writing nothing, when the settings would be bad."
  [path k v]
  (when-not (some #{k} settable)
    (throw (ex-info (str k " is not a setting a command writes")
                    {:why [(str k " is not a setting a command writes")]})))
  (let [f (io/file (str path))
        text (if (.exists f) (slurp f) "{}")
        m' (checked path (assoc (edn/read-string text) k v))
        kept (in-place text k v)]
    (spit f (if (= m' (edn/read-string kept))
              kept
              (render (ordered m'))))
    {:restart? (boolean (some #{k} fixed-keys))}))

(defn- restart-warning [old ks]
  (log/warn (str file ":") (str/join ", " (map pr-str ks))
            "take a restart, keeping"
            (pr-str (select-keys old ks))))

(defn- reload-failure [e]
  (let [{:keys [what why]} (ex-data e)]
    (log/warn "reload failed:" (or what (ex-message e)))
    (doseq [line why] (log/warn " " line))))

(defn- applied! [{:keys [settings on-change]} r eid]
  (let [old @settings
        s (:settings r)]
    (when-let [ks (seq (:restart r))] (restart-warning old ks))
    (reset! settings s)
    (on-change old s)
    [:config-loaded eid (select-keys s world-keys)]))

(defn reload!
  "Reads the config of a running server again for player eid.
  Returns the event that tells the tick how it went."
  [{:keys [settings path overlay] :as edge} eid]
  (try (applied! edge (reload @settings path overlay) eid)
       (catch Exception e
         (reload-failure e)
         [:config-failed eid])))
