(ns collider.game.gamerules
  (:require [collider.data :as data]))

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
  (into (sorted-map)
        (concat (map (fn [k] [k {:type :bool :default true}]) booleans-on)
                (map (fn [k] [k {:type :bool :default false}]) booleans-off)
                (map (fn [[k [d lo hi]]] [k {:type :int :default d :min lo :max hi}]) integers))))

(def defaults (into {} (map (fn [[k v]] [k (:default v)])) table))
(defn wire-name ^String [rule]
  (data/wire rule))

(defn rule-of [^String s]
  (let [k (data/kebab s)]
    (when (contains? table k) k)))

(defn serialize ^String [rule value]
  (if (= :bool (:type (table rule))) (if value "true" "false") (str value)))

(defn parse [rule ^String s]
  (let [{:keys [type min max]} (table rule)]
    (case type
      :bool (case s "true" true "false" false nil)
      :int (when-let [n (try (Long/parseLong s) (catch NumberFormatException _ nil))]
             (when (<= (long min) n (long max)) n)))))
