(ns collider.game.systems.sleep
  "Sleeping players and night skipping."
  (:require [collider.game.changes :as changes]
            [collider.game.clock :as clock]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.sleep :as sleeping]
            [collider.game.systems.daynight :as daynight]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.attribute :as attribute]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(def ^:private deep-sleep 100)

(def ^:private wake-marker "minecraft:wake_up_from_sleep")

(defn- block-at [world pos]
  (chunk/chunks-get-block (:chunks world) pos))

(defn bed-rule
  "Returns the bed rule of the level with what it allows now."
  [world]
  (let [r (attribute/bed-rule (:dim world))
        dark? (daynight/dark-outside? world)]
    (assoc r
           :sleep? (attribute/allows? (:can-sleep r) dark?)
           :spawn? (attribute/allows? (:can-set-spawn r) dark?))))

(defn- deep-count ^long [world asleep]
  (let [now (long (:tick world))
        deep? (fn [[_ e]]
                (>= (- now (long (get-in e [:sleeping :since])))
                    deep-sleep))]
    (count (filter deep? asleep))))

(defn- morning-deltas [world]
  (when-let [k (and (clock/advancing? world)
                    (clock/default-of (:dim world)))]
    (let [c0 (clock/state world k)
          c (or (clock/moved-to c0 k wake-marker) c0)
          sent {k (clock/network-state c true)}]
      [[:set-clock k c]
       (out/all (out/time (long (:tick world)) sent))])))

(defn- skip-night-deltas [world asleep]
  (concat (morning-deltas world)
          (when (and (get-in world [:rules :advance-weather] true)
                     (weather/raining? world))
            [[:set-weather weather/reset-cycle]])
          (mapcat (fn [[eid _]] (sleeping/wake-deltas world eid))
                  asleep)))

(defn- waking-deltas [world asleep waking]
  (let [left (count (sleeping/counted waking))]
    (concat (mapcat (fn [[eid _]] (sleeping/wake-deltas world eid))
                    waking)
            (when (pos? left)
              (let [n (- (count asleep) left)]
                [(sleeping/announcement world n)])))))

(defn- night-passes? [world all asleep]
  (let [needed (sleeping/sleepers-needed world)]
    (and (>= (count asleep) needed)
         (>= (deep-count world all) needed))))

(defn vacated-deltas
  "Returns the deltas of the bed that player eid, e, leaves between
  ticks. Its head is set free."
  [world eid e]
  (let [head (get-in e [:sleeping :pos])
        st (when head (block-at world head))
        base (dec (long (:tick world)))]
    (when (and st (= :bed (block/type-of st)))
      (let [cs [[head (sleeping/vacated st)]]]
        (delta/authored (changes/flagged-deltas world cs 3 nil base)
                        eid :player)))))

(defn- sleep-deltas [world]
  (when-let [all (seq (sleeping/in-bed world))]
    (let [sleep? (:sleep? (bed-rule world))
          asleep (sleeping/counted all)
          up? (fn [[_ e]] (or (:leave-bed? e) (not sleep?)))
          waking (filter up? all)]
      (cond
        (night-passes? world all asleep) (skip-night-deltas world all)
        (seq waking) (waking-deltas world asleep waking)))))

(defn- quit-deltas [world]
  (mapcat #(vacated-deltas world (:eid %) %)
          (get-in world [:input :quits])))

(defn sleep
  "Returns the deltas of sleeping and waking in the level."
  {:wake {:types #{:player} :keys [[:input :quits]]}}
  [world _d]
  (deltas/of-vec (concat (quit-deltas world) (sleep-deltas world))))
