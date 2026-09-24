(ns collider.game.systems.sleep
  "Sleeping players and night skipping."
  (:require [collider.game.out :as out]
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

(def ^:private day-length 24000)

(defn- block-at [world pos]
  (chunk/chunks-get-block (:chunks world) pos))

(defn bed-rule [world]
  (let [r (attribute/bed-rule (:dim world))
        dark? (daynight/dark-outside? world)]
    (assoc r
           :sleep? (attribute/allows? (:can-sleep r) dark?)
           :spawn? (attribute/allows? (:can-set-spawn r) dark?))))

(defn sleepers-needed ^long [world]
  (let [players (count (state/player-entries world))
        k [:rules :players-sleeping-percentage]
        share (long (get-in world k 100))]
    (max 1 (long (Math/ceil (/ (* players share) 100.0))))))

(defn announcement [world ^long asleep]
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

(defn wake-deltas [world eid]
  (let [e (get-in world [:entities eid])
        head (get-in e [:sleeping :pos])
        st (block-at world head)
        bed? (= :bed (block/type-of st))
        up (stand-up world e head bed?)
        yaw (if bed? (bed/look-yaw head up) (:yaw e 0.0))]
    (concat
      (when bed? [[:set-blocks [[head (vacated st)]]]])
      [[:merge-entity eid
        {:sleeping nil :leave-bed? nil :yaw yaw :pitch 0.0}]
       [:teleport eid up]
       (out/all (out/animation eid :wake-up))
       (out/to eid (out/animation eid :wake-up))
       (out/to eid (out/teleport up yaw 0.0))])))

(defn sleepers [world]
  (filter (fn [[_ e]] (and (= :player (:type e)) (:sleeping e)))
          (:entities world)))

(defn- deep-count ^long [world asleep]
  (let [now (long (:tick world))
        deep? (fn [[_ e]]
                (>= (- now (long (get-in e [:sleeping :since])))
                    deep-sleep))]
    (count (filter deep? asleep))))

(defn- skip-night-deltas [world asleep]
  (let [day (quot (long (:time-of-day world 0)) day-length)
        t (* day-length (inc day))]
    (concat [[:set-time t]
             (out/all (out/time (long (:tick world)) t))]
            (when (and (get-in world [:rules :advance-weather] true)
                       (weather/raining? world))
              [[:set-weather weather/reset-cycle]])
            (mapcat (fn [[eid _]] (wake-deltas world eid)) asleep))))

(defn- waking-deltas [world asleep waking]
  (concat (mapcat (fn [[eid _]] (wake-deltas world eid)) waking)
          [(announcement world (- (count asleep) (count waking)))]))

(defn- sleep-deltas [world]
  (let [sleep? (:sleep? (bed-rule world))
        asleep (sleepers world)
        needed (sleepers-needed world)
        up? (fn [[_ e]] (or (:leave-bed? e) (not sleep?)))
        waking (filter up? asleep)]
    (cond
      (and (>= (count asleep) needed)
           (>= (deep-count world asleep) needed))
      (skip-night-deltas world asleep)
      (seq waking) (waking-deltas world asleep waking))))

(defn sleep [world _d]
  [#(sleep-deltas world)])
