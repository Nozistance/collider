(ns collider.game.turn.living
  "The parts of a living entity's turn around its step: the base tick
  before it and the touch of blocks after it."
  (:require [collider.game.effect.account :as account]
            [collider.game.entity.hurt :as hurt]))

(set! *warn-on-reflection* true)

(defn- shown [world eid e ds]
  (let [rs (hurt/report-deltas world eid e)]
    (if (seq rs)
      [(hurt/hurt-now world eid e rs) (into (vec ds) rs)]
      [e ds])))

(defn- base-of [world eid e]
  (cond-> []
    (hurt/based? world e) (into (hurt/base-deltas world eid e))
    (not (pos? (double (:health e))))
    (into (hurt/timer-deltas eid e))))

(defn based
  "Returns living entity eid after LivingEntity.baseTick and its
  deltas: fire, the void and the countdown of its hurt resistance,
  its death timer, then its effects (:509). A hurt shows at once."
  [world eid e]
  (let [ds (base-of world eid e)
        e1 (if (seq ds) (hurt/hurt-now world eid e ds) e)
        fx (account/tick-deltas world eid e1)]
    (cond (seq fx) (let [e2 (hurt/hurt-now world eid e1 fx)]
                     (shown world eid e2 (into ds fx)))
          (seq ds) (shown world eid e1 ds)
          :else (shown world eid e nil))))

(defn touched
  "Returns living entity eid after the hurts ls of its landing in its
  move (Entity.checkFallDamage:1589), the blocks it touches at the
  end of its move (LivingEntity.aiStep, applyEffectsFromBlocks
  :3154), then the hurts cs of its push (pushEntities :3183), and ds
  with their deltas and the show of each hurt of its turn. It is wet
  as it was when its turn began."
  [world eid e wet? ds ls cs]
  (let [fd (hurt/fire-deltas world eid e wet?)
        fs (if (or ls cs) (-> (vec ls) (into fd) (into cs)) fd)]
    (if (seq fs)
      (shown world eid (hurt/hurt-now world eid e fs)
             (into (vec ds) fs))
      (shown world eid e ds))))

(defn ended
  "Returns the deltas of dead entity eid whose death ends before its
  step, as LivingEntity.tickDeath removes it."
  [eid e]
  (hurt/timer-deltas eid e))
