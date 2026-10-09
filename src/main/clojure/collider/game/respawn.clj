(ns collider.game.respawn
  "Where a dead player comes back, and the deltas that bring it."
  (:require [collider.game.block.menu :as menu]
            [collider.game.block.screen :as screen]
            [collider.game.food :as food]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.blocks.bed :as bed]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const player-health 20.0)

(defn respawn-config
  "Returns the respawn point player e set, or nil when it set none."
  [e]
  (if-let [{:keys [pos yaw pitch]} (:forced-spawn e)]
    {:pos     pos :yaw (double (or yaw 0.0))
     :pitch   (double (or pitch 0.0)) :forced? true}
    (when-let [pos (:spawn e)]
      {:pos pos :yaw (:yaw e 0.0) :pitch 0.0 :forced? false})))

(def ^:private ^:const stand-up-reach 3)

(defn- reach-chunks [c]
  (let [c (long c)]
    (range (bit-shift-right (- c stand-up-reach) 4)
           (inc (bit-shift-right (+ c stand-up-reach) 4)))))

(defn respawn-chunk-ids
  "Returns the ids of the chunks the check of the respawn point of
  player e reads."
  [e]
  (when-let [{[x _ z] :pos} (respawn-config e)]
    (for [cx (reach-chunks x) cz (reach-chunks z)]
      (chunk/pos->id cx cz))))

(defn- respawnable? [chunks pos]
  (block/possible-to-respawn-in?
    (chunk/at chunks pos)))

(defn- free-to-stand? [chunks [x y z]]
  (and (respawnable? chunks [x y z])
       (respawnable? chunks [x (inc (long y)) z])))

(defn- stand-on [pos yaw pitch]
  [[(+ (double (nth pos 0)) 0.5)
    (+ (double (nth pos 1)) 0.1)
    (+ (double (nth pos 2)) 0.5)]
   yaw pitch])

(defn- found-respawn [chunks {:keys [pos yaw pitch forced?]}]
  (if (bed/head-pos chunks pos)
    (let [up (bed/stand-up-position chunks pos yaw)]
      [up (bed/look-yaw pos up) 0.0])
    (when (and forced? (free-to-stand? chunks pos))
      (stand-on pos yaw pitch))))

(defn bed-respawn
  "Returns [pos yaw pitch] at the respawn point of player e.
  Returns nil when it has none or the point cannot be used."
  [chunks e]
  (when-let [cfg (respawn-config e)]
    (found-respawn chunks cfg)))

(defn- reshow-deltas [world eid]
  (for [[oid o] (:entities world)
        :when (contains? (:tracking o) eid)]
    [:tracking oid [] [eid]]))

(def ^:private no-experience
  {:xp-level 0 :xp-progress 0.0 :xp-total 0 :score 0})

(defn- fresh-marks [e tick keep?]
  (cond-> {:health player-health :health-sent player-health
           :hud-sent nil
           :hurt-resist 0 :last-damage 0.0 :death-time 0
           :born tick :ambience nil :xp-sent -1 :level-up-at 0
           :xp-ready-at nil :client-vel [0.0 0.0 0.0]
           :fire 0 :burning? false :ticks-frozen 0 :fall 0.0
           :landed nil}
    true (merge food/fresh)
    (not keep?) (merge no-experience)
    (seq (:effects e)) (assoc :effects {})
    (:absorption e) (assoc :absorption nil)))

(defn- shown-experience [e keep?]
  (let [e (if keep? e (merge e no-experience))]
    (out/experience (:xp-progress e 0.0) (:xp-level e 0)
                    (:xp-total e 0))))

(defn- revived [eid e pos yaw pitch tick keep?]
  [[:teleport eid pos]
   [:merge-entity eid (fresh-marks e tick keep?)]
   (out/to eid (out/respawn))
   (out/to eid (out/teleport pos yaw pitch))
   (out/to eid (shown-experience e keep?))
   (out/to eid (out/held-slot (long (or (:held-slot e) 0))))])

(defn- own-slots [inv]
  (out/inventory (mapv inv (range menu/slot-count)) nil))

(def ^:private not-valid
  (out/overlay {:translate "block.minecraft.spawn.not_valid"}))

(defn- keeps? [world] (get-in world [:rules :keep-inventory] false))

(defn- kept-xp? [world e]
  (or (keeps? world) (game-mode/spectator? e)))

(defn respawn-deltas
  "Returns the deltas that bring dead player eid back at pos.
  When lost? is true they tell it the respawn point it set was lost."
  [world eid [pos yaw pitch lost?]]
  (let [e (get-in world [:entities eid])
        inv (apply dissoc (:inventory e) (range 5))
        t (:tick world)
        back (revived eid e pos yaw pitch t (kept-xp? world e))]
    (cond-> (into (vec (screen/removed-deltas world eid e)) back)
      (seq inv) (conj (out/to eid (own-slots inv)))
      lost? (conj (out/to eid not-valid))
      true (into (reshow-deltas world eid)))))
