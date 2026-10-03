(ns collider.game.sleep
  "Players in bed, waking up and the count of sleepers."
  (:require [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.level :as level]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.bed :as bed]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- block-at [world pos]
  (chunk/at (:chunks world) pos))

(defn counted
  "Returns the player entries that count for sleep, which are no
  spectators."
  [entries]
  (remove (comp game-mode/spectator? val) entries))

(defn in-bed
  [world]
  (filter (fn [[_ e]] (:sleeping e)) (level/player-entries world)))

(defn sleepers
  "Returns the sleeping players that count, which are no spectators."
  [world]
  (counted (in-bed world)))

(defn sleepers-needed
  "Returns how many sleeping players the night needs.
  Spectators do not count."
  ^long [world]
  (let [players (count (counted (level/player-entries world)))
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

(defn vacated
  "Returns bed state st with no one in it."
  [st]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :occupied :false)))

(defn- stand-up [world e head bed?]
  (let [p (:pos e)]
    (if bed?
      (bed/stand-up-position (:chunks world) head (:yaw e 0.0))
      [(v/x p) (v/y p) (v/z p)])))

(defn- woken-deltas [eid up yaw]
  [[:merge-entity eid
    {:sleeping nil :leave-bed? nil :pose :standing :yaw yaw
     :pitch 0.0}]
   [:teleport eid up]
   (out/all (out/animation eid :wake-up))
   (out/to eid (out/animation eid :wake-up))
   (out/to eid (out/teleport up yaw 0.0))])

(defn- freed-deltas [world eid head st]
  (delta/authored (changes/set-deltas world [[head (vacated st)]])
                  eid :player))

(defn wake
  "Returns where player eid stands up, its yaw and the deltas of its
  waking up."
  [world eid]
  (let [e (get-in world [:entities eid])
        head (get-in e [:sleeping :pos])
        st (block-at world head)
        bed? (= :bed (block/type-of st))
        up (stand-up world e head bed?)
        yaw (if bed? (bed/look-yaw head up) (:yaw e 0.0))]
    {:pos up :yaw yaw
     :deltas (concat (when bed? (freed-deltas world eid head st))
                     (woken-deltas eid up yaw))}))

(defn wake-deltas
  [world eid]
  (:deltas (wake world eid)))
