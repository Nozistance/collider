(ns collider.game.rules
  "Game rules of 26.2: the table with types and defaults, their wire names,
   parsing and printing of values. The values of a world live in :rules."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private booleans-on
  [:advance-time :advance-weather :allow-entering-nether-using-portals :block-drops
   :block-explosion-drop-decay :command-blocks-work :command-block-output :drowning-damage
   :elytra-movement-check :ender-pearls-vanish-on-death :entity-drops :fall-damage :fire-damage
   :forgive-dead-players :freeze-damage :global-sound-events :locator-bar :log-admin-commands
   :mob-drops :mob-explosion-drop-decay :mob-griefing :natural-health-regeneration
   :player-movement-check :projectiles-can-break-blocks :pvp :raids :send-command-feedback
   :show-advancement-messages :show-death-messages :spawner-blocks-work :spawn-mobs
   :spawn-monsters :spawn-patrols :spawn-phantoms :spawn-wandering-traders :spawn-wardens
   :spectators-generate-chunks :spread-vines :tnt-explodes :water-source-conversion])

(def ^:private booleans-off
  [:immediate-respawn :keep-inventory :lava-source-conversion :limited-crafting
   :reduced-debug-info :tnt-explosion-drop-decay :universal-anger])

(def ^:private integers
  {:fire-spread-radius-around-player [128 -1 Integer/MAX_VALUE]
   :max-block-modifications          [32768 1 Integer/MAX_VALUE]
   :max-command-forks                [65536 0 Integer/MAX_VALUE]
   :max-command-sequence-length      [65536 0 Integer/MAX_VALUE]
   :max-entity-cramming              [24 0 Integer/MAX_VALUE]
   :max-minecart-speed               [8 1 1000]
   :max-snow-accumulation-height     [1 0 8]
   :players-nether-portal-creative-delay [0 0 Integer/MAX_VALUE]
   :players-nether-portal-default-delay  [80 0 Integer/MAX_VALUE]
   :players-sleeping-percentage      [100 0 Integer/MAX_VALUE]
   :random-tick-speed                [3 0 Integer/MAX_VALUE]
   :respawn-radius                   [10 0 Integer/MAX_VALUE]})

(def table
  "Rule to {:type :bool or :int, :default, and for ints :min :max}, as vanilla
   GameRules registers them."
  (into (sorted-map)
        (concat (map (fn [k] [k {:type :bool :default true}]) booleans-on)
                (map (fn [k] [k {:type :bool :default false}]) booleans-off)
                (map (fn [[k [d lo hi]]] [k {:type :int :default d :min lo :max hi}]) integers))))

(def defaults (into {} (map (fn [[k v]] [k (:default v)])) table))

(defn wire-name
  "Identifier of the rule on the wire and in commands: minecraft:advance_time."
  ^String [rule]
  (str "minecraft:" (str/replace (name rule) "-" "_")))

(defn rule-of
  "Rule keyword for a name as typed or sent: advance_time, minecraft:advance_time."
  [^String s]
  (let [k (keyword (str/replace (str/replace (str/lower-case s) #"^minecraft:" "") "_" "-"))]
    (when (contains? table k) k)))

(defn serialize ^String [rule value]
  (if (= :bool (:type (table rule))) (if value "true" "false") (str value)))

(defn parse
  "Value of the rule from its text, nil if the text does not fit the type
   or the range."
  [rule ^String s]
  (let [{:keys [type min max]} (table rule)]
    (case type
      :bool (case s "true" true "false" false nil)
      :int (when-let [n (try (Long/parseLong s) (catch NumberFormatException _ nil))]
             (when (<= (long min) n (long max)) n)))))
