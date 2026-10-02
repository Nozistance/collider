(ns collider.game.systems.blocks.edit
  "Block edit checks."
  (:require [collider.game.changes :as changes]
            [collider.game.entity :as entity]
            [collider.game.mode :as game-mode]
            [collider.game.command.forms :as forms]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn builder-box
  "Returns the half width and height an entity blocks with.
  Returns nil when it never blocks placement, as a spectator."
  [e]
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
  "Whether st is a block only game masters place, use and break."
  [^long st]
  (contains? game-master-types (block/type-of st)))

(defn game-master?
  "Whether player e may place, use and break game master blocks: a
  creative player of the game master permission level."
  [e]
  (and (game-mode/creative? e)
       (<= (long forms/gamemaster) (player/permission-level e))))

(defn hit-uv
  "Returns where a click landed on a face, across and up.
  Both run from zero to one."
  [face [cx cy cz]]
  (let [x (/ (double cx) 16.0)
        y (/ (double cy) 16.0)
        z (/ (double cz) 16.0)]
    (case (long face)
      2 [(- 1.0 x) y]
      3 [x y]
      4 [z y]
      5 [(- 1.0 z) y]
      nil)))

(defn section
  "Returns which of n equal parts a fraction falls in."
  ^long [^double rel ^long n]
  (min (dec n) (max 0 (long (Math/floor (* rel n))))))

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

(defn with-water
  "Returns the state with water, or without when logged? is false."
  [st logged?]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :waterlogged
                      (if logged? :true :false))))

(def ^:private full-water-types
  "The block classes that hold falling water as well as a source.
  Everywhere else a placed block holds only source water."
  #{:conduit :coral-plant :coral-fan :coral-wall-fan
    :base-coral-plant :base-coral-fan :base-coral-wall-fan})

(defn- placed-wet? [st state]
  (if (contains? full-water-types (block/type-of state))
    (block/full-water? st)
    (block/water-source? st)))

(defn waterlogged
  "Returns the state to place at pos'.
  It holds water when it takes the water that stands there."
  [world pos' state]
  (if (and (placed-wet? (changes/block-at world pos') state)
           (contains? (block/props-of state) :waterlogged)
           (not= :light (block/type-of state)))
    (with-water state true)
    state))
