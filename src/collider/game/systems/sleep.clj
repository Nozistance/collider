(ns collider.game.systems.sleep
  "Sleeping players: waking up by choice or at dawn, and skipping the night
   when enough of them have slept long enough: the share is the world rule
   :players-sleeping-percentage, vanilla playersSleepingPercentage."
  (:require [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.daynight :as daynight]
            [collider.vec :as v]
            [collider.world.bed :as bed]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:private deep-sleep 100)
(def ^:private day-length 24000)

(defn- block-at [world pos] (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos))

(defn sleepers-needed
  "How many sleepers skip the night, by the world rule :players-sleeping-percentage."
  ^long [world]
  (let [players (count (state/player-entries world))
        share   (long (get-in world [:rules :players-sleeping-percentage] 100))]
    (max 1 (long (Math/ceil (/ (* players share) 100.0))))))

(defn announcement
  "Overlay for everyone when the number of sleepers changes to asleep
   (vanilla announceSleepStatus)."
  [world ^long asleep]
  (let [needed (sleepers-needed world)]
    (out/all (out/overlay [(if (>= asleep needed)
                             {:translate "sleep.skipping_night"}
                             {:translate "sleep.players_sleeping" :with [asleep needed]})]))))

(defn wake-deltas
  "The player stands up next to the bed, everyone sees them wake."
  [world eid]
  (let [e    (get-in world [:entities eid])
        head (get-in e [:sleeping :pos])
        st   (block-at world head)
        bed? (= :bed (block/type-of st))
        up   (if bed? (bed/stand-up-position (:chunks world) head (:yaw e 0.0)) [(v/x (:pos e)) (v/y (:pos e)) (v/z (:pos e))])
        yaw  (if bed? (bed/look-yaw head up) (:yaw e 0.0))]
    (concat
     (when bed?
       [[:set-blocks [[head (block/state (block/block-of st) (assoc (block/props-of st) :occupied :false))]]]])
     [[:merge-entity eid {:sleeping nil :leave-bed? nil :yaw yaw :pitch 0.0}]
      [:teleport eid up]
      (out/all (out/animation eid :wake-up))
      (out/to eid (out/animation eid :wake-up))
      (out/to eid (out/teleport up yaw 0.0))])))

(defn sleepers [world]
  (filter (fn [[_ e]] (and (= :player (:type e)) (:sleeping e))) (:entities world)))

(defn- sleep-deltas [world]
  (let [dark?   (daynight/dark? (:time-of-day world 0))
        asleep  (sleepers world)
        needed  (sleepers-needed world)
        deep    (count (filter (fn [[_ e]] (>= (- (long (:tick world)) (long (get-in e [:sleeping :since]))) deep-sleep)) asleep))
        waking  (remove (fn [[_ e]] (not (or (:leave-bed? e) (not dark?)))) asleep)]
    (cond
      (and (>= (count asleep) needed) (>= deep needed))
      (let [t (* day-length (inc (quot (long (:time-of-day world 0)) day-length)))]
        (concat [[:set-time t] (out/all (out/time (long (:tick world)) t))]
                (mapcat (fn [[eid _]] (wake-deltas world eid)) asleep)))
      (seq waking)
      (concat (mapcat (fn [[eid _]] (wake-deltas world eid)) waking)
              [(announcement world (- (count asleep) (count waking)))]))))

(defn sleep [world _events]
  [#(sleep-deltas world)])
