(ns collider.game.mob.placement
  "Where a mob of a type may spawn: the placement types and rules of
  SpawnPlacements and the position checks of NaturalSpawner."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.light :as light]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(defn- state-set ^booleans [runs]
  (let [a (boolean-array (data/block-state-count))]
    (doseq [[lo hi] runs
            id (range lo (inc (min (long hi) (dec (alength a)))))]
      (aset a (int id) true))
    a))

(def ^:private ^:table sets
  (delay (let [s (data/spawns)]
           {:floors (mapv state-set (:floors s))
            :dangers (mapv state-set (:dangers s))
            :conductors (state-set (:conductors s))})))

(defn- in? [^booleans a ^long st] (aget a st))

(defn facts
  "Returns the spawn facts of entity type t, nil for a misc type."
  [t]
  (get-in (data/spawns) [:types t]))

(defn categories
  "Returns the mob categories in their order, each a pair of its name
  and its facts."
  []
  (:categories (data/spawns)))

(defn conductor?
  "Returns true when st conducts redstone."
  [^long st]
  (in? (:conductors @sets) st))

(defn- floor? [f ^long st]
  (in? (nth (:floors @sets) (:floor f)) st))

(defn- dangerous? [f ^long st]
  (in? (nth (:dangers @sets) (:danger f)) st))

(defn empty-spawn-block?
  "Returns true when a mob with facts f may stand in st, as
  NaturalSpawner.isValidEmptySpawnBlock."
  [f ^long st]
  (not (or (block/full-cube? st) (block/signal-source? st)
           (block/liquid-class st)
           (block/tagged? st "prevent_mob_spawning_inside")
           (dangerous? f st))))

(def ^:private ^:const border 29999984)

(defn- in-border? [^long x ^long z]
  (and (<= (- border) x) (< x border) (<= (- border) z) (< z border)))

(defn- at ^long [chunks x y z] (chunk/chunks-get-block chunks x y z))

(defn- on-ground? [chunks f x y z]
  (let [x (long x) y (long y) z (long z)]
    (and (in-border? x z) (floor? f (at chunks x (dec y) z))
         (empty-spawn-block? f (at chunks x y z))
         (empty-spawn-block? f (at chunks x (inc y) z)))))

(defn position-ok?
  "Returns true when the placement type of kind f allows a spawn at
  block x y z, as SpawnPlacements.isSpawnPositionOk."
  [chunks f x y z]
  (case (:placement f)
    :on-ground (on-ground? chunks f x y z)
    :no-restrictions true
    false))

(defn- bright? [chunks x y z] (> (light/light-at chunks x y z) 8))

(defn rules-ok?
  "Returns true when a mob of kind k may spawn at block x y z, as
  SpawnPlacements.checkSpawnRules with Animal.checkAnimalSpawnRules
  and its kin: the block below is of the ground of k, and lit."
  [chunks k peaceful? x y z]
  (and (or (:peaceful k) (not peaceful?))
       (some? (:ground k))
       (block/tagged? (at chunks x (dec (long y)) z) (:ground k))
       (bright? chunks x y z)))

(defn spawn-box
  "Returns the box [x0 y0 z0 x1 y1 z1] of a spawn of kind k at x y z,
  as EntityType.getSpawnAABB."
  [k x y z]
  (let [s (float (:scale k 1.0))
        half (double (/ (float (* s (float (:width k)))) (float 2.0)))
        height (double (float (* s (float (:height k)))))]
    [(- (double x) half) (double y) (- (double z) half)
     (+ (double x) half) (+ (double y) height) (+ (double z) half)]))

(defn box-free?
  "Returns true when box meets no block, as noCollision of
  CollisionGetter for no entity: every block sees it above."
  [chunks box]
  (phys/box-free? chunks box Double/MAX_VALUE))

(defn kind
  "Returns the spawn facts of entity type t with its size and ground,
  the tag of the blocks it spawns on; nil for a misc type."
  [t ground]
  (when-let [f (facts t)]
    (let [{:keys [width height]} (get (data/entities) t)]
      (assoc f :type t :ground ground :width width :height height))))
