(ns collider.game.gamerules
  "Game rules, their defaults and their values."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(def ^:private booleans-on
  [:advance-time :advance-weather :allow-entering-nether-using-portals
   :block-drops :block-explosion-drop-decay :command-blocks-work
   :command-block-output :drowning-damage :elytra-movement-check
   :ender-pearls-vanish-on-death :entity-drops :fall-damage
   :fire-damage :forgive-dead-players :freeze-damage
   :global-sound-events :locator-bar :log-admin-commands :mob-drops
   :mob-explosion-drop-decay :mob-griefing
   :natural-health-regeneration :player-movement-check
   :projectiles-can-break-blocks :pvp :raids :send-command-feedback
   :show-advancement-messages :show-death-messages
   :spawner-blocks-work :spawn-mobs :spawn-monsters :spawn-patrols
   :spawn-phantoms :spawn-wandering-traders :spawn-wardens
   :spectators-generate-chunks :spread-vines :tnt-explodes
   :water-source-conversion])

(def ^:private booleans-off
  [:immediate-respawn :keep-inventory :lava-source-conversion
   :limited-crafting :reduced-debug-info :tnt-explosion-drop-decay
   :universal-anger])

(def ^:private integers
  {:fire-spread-radius-around-player     [128 -1 Integer/MAX_VALUE]
   :max-block-modifications              [32768 1 Integer/MAX_VALUE]
   :max-command-forks                    [65536 0 Integer/MAX_VALUE]
   :max-command-sequence-length          [65536 0 Integer/MAX_VALUE]
   :max-entity-cramming                  [24 0 Integer/MAX_VALUE]
   :max-minecart-speed                   [8 1 1000]
   :max-snow-accumulation-height         [1 0 8]
   :players-nether-portal-creative-delay [0 0 Integer/MAX_VALUE]
   :players-nether-portal-default-delay  [80 0 Integer/MAX_VALUE]
   :players-sleeping-percentage          [100 0 Integer/MAX_VALUE]
   :random-tick-speed                    [3 0 Integer/MAX_VALUE]
   :respawn-radius                       [10 0 Integer/MAX_VALUE]})

(defn- bool-rules [ks default]
  (map (fn [k] [k {:type :bool :default default}]) ks))

(defn- int-rule [[k [d lo hi]]]
  [k {:type :int :default d :min lo :max hi}])

(def table
  "Every game rule with its type, its default and its bounds."
  (into (sorted-map)
        (concat (bool-rules booleans-on true)
                (bool-rules booleans-off false)
                (map int-rule integers))))

(def defaults
  "The value of each game rule in a new world."
  (into {} (map (fn [[k v]] [k (:default v)])) table))

(defn wire-name
  "Returns the name of rule as the client knows it."
  ^String [rule]
  (data/wire rule))

(defn rule-of
  "Returns the rule named s, or nil when there is none."
  [^String s]
  (let [k (data/kebab s)]
    (when (contains? table k) k)))

(defn serialize
  "Returns value of rule as text."
  ^String [rule value]
  (if (= :bool (:type (table rule)))
    (if value "true" "false")
    (str value)))

(defn- long-of [^String s]
  (try (Long/parseLong s) (catch NumberFormatException _ nil)))

(defn parse
  "Returns the value of rule that text s names, or nil when s names
  none in its bounds."
  [rule ^String s]
  (let [{:keys [type min max]} (table rule)]
    (case type
      :bool (case s "true" true "false" false nil)
      :int (when-let [n (long-of s)]
             (when (<= (long min) n (long max)) n)))))
