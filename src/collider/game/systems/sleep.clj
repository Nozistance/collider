(ns collider.game.systems.sleep
  "Sleeping players and night skipping."
  (:require [collider.game.clock :as clock]
            [collider.game.game-mode :as game-mode]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.daynight :as daynight]
            [collider.vec :as v]
            [collider.world.blocks.bed :as bed]
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

(defn- counted [entries]
  (remove (comp game-mode/spectator? val) entries))

(defn sleepers-needed
  "Returns how many sleeping players the night needs.
  Spectators do not count."
  ^long [world]
  (let [players (count (counted (state/player-entries world)))
        k [:rules :players-sleeping-percentage]
        share (long (get-in world k 100))]
    (max 1 (long (Math/ceil (/ (* players share) 100.0))))))

(defn announcement
  "Returns the message that counts the sleeping players."
  [world ^long asleep]
  (let [needed (sleepers-needed world)]
    (out/all (out/overlay
               (if (>= asleep needed)
                 {:translate "sleep.skipping_night"}
                 {:translate "sleep.players_sleeping"
                  :with [asleep needed]})))))

(defn- stand-up [world e head bed?]
  (let [p (:pos e)]
    (if bed?
      (bed/stand-up-position (:chunks world) head (:yaw e 0.0))
      [(v/x p) (v/y p) (v/z p)])))

(defn- vacated [st]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :occupied :false)))

(defn- woken-deltas [eid up yaw]
  [[:merge-entity eid
    {:sleeping nil :leave-bed? nil :yaw yaw :pitch 0.0}]
   [:teleport eid up]
   (out/all (out/animation eid :wake-up))
   (out/to eid (out/animation eid :wake-up))
   (out/to eid (out/teleport up yaw 0.0))])

(defn wake-deltas
  "Returns the deltas of player eid waking up."
  [world eid]
  (let [e (get-in world [:entities eid])
        head (get-in e [:sleeping :pos])
        st (block-at world head)
        bed? (= :bed (block/type-of st))
        up (stand-up world e head bed?)
        yaw (if bed? (bed/look-yaw head up) (:yaw e 0.0))]
    (concat
      (when bed? (edit/set-deltas world [[head (vacated st)]]))
      (woken-deltas eid up yaw))))

(defn- in-bed [world]
  (filter (fn [[_ e]] (:sleeping e)) (state/player-entries world)))

(defn sleepers
  "Returns the sleeping players that count, which are no spectators."
  [world]
  (counted (in-bed world)))

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
          (mapcat (fn [[eid _]] (wake-deltas world eid)) asleep)))

(defn- waking-deltas [world asleep waking]
  (let [left (count (counted waking))]
    (concat (mapcat (fn [[eid _]] (wake-deltas world eid)) waking)
            (when (pos? left)
              [(announcement world (- (count asleep) left))]))))

(defn- night-passes? [world all asleep]
  (let [needed (sleepers-needed world)]
    (and (>= (count asleep) needed)
         (>= (deep-count world all) needed))))

(defn vacated-deltas
  "Returns the deltas of the bed that player e leaves between ticks.
  Its head is set free."
  [world e]
  (let [head (get-in e [:sleeping :pos])
        st (when head (block-at world head))
        base (dec (long (:tick world)))]
    (when (and st (= :bed (block/type-of st)))
      (edit/flagged-deltas world [[head (vacated st)]] 3 nil base))))

(defn- sleep-deltas [world]
  (when-let [all (seq (in-bed world))]
    (let [sleep? (:sleep? (bed-rule world))
          asleep (counted all)
          up? (fn [[_ e]] (or (:leave-bed? e) (not sleep?)))
          waking (filter up? all)]
      (cond
        (night-passes? world all asleep) (skip-night-deltas world all)
        (seq waking) (waking-deltas world asleep waking)))))

(defn- quit-deltas [world]
  (mapcat #(vacated-deltas world %) (:quits world)))

(defn sleep
  "Returns a step that runs sleeping and waking in the level."
  [world _d]
  [#(concat (quit-deltas world) (sleep-deltas world))])
