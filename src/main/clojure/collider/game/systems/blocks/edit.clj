(ns collider.game.systems.blocks.edit
  "Block edit checks."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.changes :as changes]
            [collider.game.entity :as entity]
            [collider.game.mode :as game-mode]
            [collider.game.command.args.block :as block-args]
            [collider.game.command.forms :as forms]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.stack :as stack]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- builder-box [e]
  (case (:type e)
    :player (when-not (game-mode/spectator? e) (entity/box e))
    (:tnt :falling-block) (entity/box e)
    :item nil
    (when (mobs/mob-type? (:type e)) (mobs/box-of e))))

(defn- placed-box [[x y z] [a b c d e f]]
  (let [at (fn [o v] (+ (long o) (/ (double v) 16.0)))]
    [(at x a) (at y b) (at z c) (at x d) (at y e) (at z f)]))

(defn- body-box [e [half h]]
  (let [p (:pos e) half (double half)]
    [(- (v/x p) half) (v/y p) (- (v/z p) half)
     (+ (v/x p) half) (+ (v/y p) (double h)) (+ (v/z p) half)]))

(defn obstructed?
  "Returns true when a block of that state would overlap an entity.
  Only an entity that stops building at the cell counts."
  [world pos state]
  (let [bs (mapv #(placed-box pos %) (block/collision-boxes state))
        hits? (fn [e]
                (when-let [d (builder-box e)]
                  (let [b (body-box e d)]
                    (some #(phys/joined? % b) bs))))]
    (boolean (and (seq bs) (some hits? (vals (:entities world)))))))

(defn own-change
  "Returns the effect that shows one player the true block at pos."
  [world eid pos]
  (let [at (chunk/block-chunk pos)
        changed [[pos (changes/block-at world pos)]]]
    (out/to eid (out/blocks-changed at changed))))

(defn- saved [world pos]
  (when-let [e (be/at world pos)]
    (be/full-nbt e pos (:tick world))))

(defn- same-block? [{:keys [state entity? nbt]} st full]
  (and (= state st)
       (or (not entity?) (= nbt (some-> full (dissoc :x :y :z))))))

(defn- tested
  "Returns [hit? v'] for the predicates v of an adventure component
  on the block at pos. The last block tested stays in the metadata
  of v, and the same block again gets the same answer."
  [world v pos]
  (let [st (changes/block-at world pos)
        full (saved world pos)
        seen (meta v)]
    (if (and seen (same-block? seen st full))
      [(:hit? seen) v]
      (let [p (some #(when (block-args/component-matches? % st full) %)
                    (:predicates v))]
        [(some? p)
         (with-meta v {:state st :hit? (some? p)
                       :entity? (some? (:nbt p))
                       :nbt (some-> full (dissoc :x :y :z))})]))))

(defn- allows [world eid e hand k pos]
  (let [stack (player/hand-stack e hand)]
    (if-let [v (stack/component stack k)]
      (let [[hit? v'] (tested world v pos)]
        [hit? (when-not (= (meta v) (meta v'))
                [[:set-slot eid (player/hand-slot e hand)
                  (assoc-in stack [:components k] v')]])])
      [false nil])))

(defn break-check
  "Returns [may? deltas] for player eid, e, breaking the block at pos.
  Out of the modes that build only a main hand stack that can break
  the block lets it. The deltas keep the last block it tested."
  [world eid e pos]
  (cond (game-mode/may-build? e) [true nil]
        (game-mode/spectator? e) [false nil]
        :else (allows world eid e :main :can-break pos)))

(defn may-break?
  "Returns true when player e may break the block at pos."
  [world e pos]
  (first (break-check world nil e pos)))

(defn use-check
  "Returns [may? deltas] for player eid, e, using the stack in its
  hand on the block at pos. Out of the modes that build only a stack
  that can be placed on the block lets it."
  [world eid e pos]
  (if (game-mode/may-build? e)
    [true nil]
    (allows world eid e (:use-hand e :main) :can-place-on pos)))

(defn may-use-at?
  "Returns true when player e may use the stack in its hand on the
  block at pos."
  [world e pos]
  (first (use-check world nil e pos)))

(defn build-limit
  "Returns the red line above the hotbar that names a height limit.
  It names the top one when high? is true."
  [eid high? ^long y]
  (let [k (if high? "build.tooHigh" "build.tooLow")
        text {:translate k :with [y] :color "red"}]
    (out/to eid (out/overlay text))))

(def ^:private game-master-types
  #{:command :structure :jigsaw :test :test-instance})

(defn game-master-block?
  "Returns true when only game masters may place, use and break st."
  [^long st]
  (contains? game-master-types (block/type-of st)))

(defn game-master?
  "Returns true when player e may place, use and break game
  master blocks."
  [e]
  (and (game-mode/creative? e)
       (<= (long forms/gamemaster) (player/permission-level e))))

(defn- hit-uv [face [cx cy cz]]
  (let [x (/ (double cx) 16.0)
        y (/ (double cy) 16.0)
        z (/ (double cz) 16.0)]
    (case (dir/from-index (long face))
      :north [(- 1.0 x) y]
      :south [x y]
      :west [z y]
      :east [(- 1.0 z) y]
      nil)))

(defn- section ^long [^double rel ^long n]
  (min (dec n) (max 0 (num/floor (* rel n)))))

(defn hit-slot
  "Returns which slot of a grid drawn on the block face was clicked."
  [st face cursor rows cols]
  (when (= (block/facing-of st) (dir/from-index (long face)))
    (when-let [[u v] (hit-uv face cursor)]
      (let [cols (long cols)
            col (section (double u) cols)
            row (section (- 1.0 (double v)) (long rows))]
        (+ col (* cols row))))))

(defn waterloggable?
  "Returns true when the state can take water and holds none."
  [st]
  (= :false (:waterlogged (block/props-of st))))

(def ^:private flowing-water-types
  #{:conduit :coral-plant :coral-fan :coral-wall-fan
    :base-coral-plant :base-coral-fan :base-coral-wall-fan})

(defn- placed-wet? [st state]
  (if (contains? flowing-water-types (block/type-of state))
    (block/full-water? st)
    (block/water-source? st)))

(defn waterlogged
  "Returns the state to place at pos'.
  It holds water when it takes the water that stands there."
  [world pos' state]
  (if (and (placed-wet? (changes/block-at world pos') state)
           (contains? (block/props-of state) :waterlogged)
           (not= :light (block/type-of state)))
    (block/with-water state)
    state))
