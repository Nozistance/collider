(ns collider.world.space.habitat
  "Where a mob kind may spawn. It reads the floor, the blocks the mob
  stands in, the light and the room its box needs."
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
            :dangers (mapv state-set (:dangers s))})))

(defn- in? [^booleans a ^long st] (aget a st))

(defn facts
  "Returns the spawn facts of entity type t, nil for a misc type."
  [t]
  (get-in (data/spawns) [:types t]))

(defn categories
  "Returns the mob categories in order, each with its cap per chunk,
  its despawn distances and whether it is friendly and persistent."
  []
  (:categories (data/spawns)))

(defn spawn-floor?
  "Returns true when a mob with facts f may spawn on st."
  [f ^long st]
  (in? (nth (:floors @sets) (:floor f)) st))

(defn- dangerous? [f ^long st]
  (in? (nth (:dangers @sets) (:danger f)) st))

(defn empty-spawn-block?
  "Returns true when a mob with facts f may stand in st."
  [f ^long st]
  (not (or (block/full-cube? st) (block/signal-source? st)
           (block/liquid-class st)
           (block/tagged? st "prevent_mob_spawning_inside")
           (dangerous? f st))))

(defn- in-border? [^long x ^long z]
  (let [b chunk/world-border]
    (and (<= (- b) x) (< x b) (<= (- b) z) (< z b))))

(defn- at ^long [chunks x y z] (chunk/block-state chunks x y z))

(defn- on-ground? [chunks f x y z]
  (let [x (long x) y (long y) z (long z)]
    (and (in-border? x z) (spawn-floor? f (at chunks x (dec y) z))
         (empty-spawn-block? f (at chunks x y z))
         (empty-spawn-block? f (at chunks x (inc y) z)))))

(defn position-ok?
  "Returns true when the placement type of kind f allows a spawn at
  block x y z."
  [chunks f x y z]
  (case (:placement f)
    :on-ground (on-ground? chunks f x y z)
    :no-restrictions true
    false))

(defn- bright? [chunks x y z] (> (light/light-at chunks x y z) 8))

(defn rules-ok?
  "Returns true when a mob of kind k may spawn at block x y z. The
  block below must be of the ground of k and the cell must be lit."
  [chunks k peaceful? x y z]
  (and (or (:peaceful k) (not peaceful?))
       (some? (:ground k))
       (block/tagged? (at chunks x (dec (long y)) z) (:ground k))
       (bright? chunks x y z)))

(defn spawn-box
  [k x y z]
  (let [s (float (:scale k 1.0))
        half (double (/ (float (* s (float (:width k)))) (float 2.0)))
        height (double (float (* s (float (:height k)))))]
    [(- (double x) half) (double y) (- (double z) half)
     (+ (double x) half) (+ (double y) height) (+ (double z) half)]))

(defn mob-box-free?
  "Returns true when box meets no block. Every block sees the box
  above it."
  [chunks box]
  (phys/box-free? chunks box Double/MAX_VALUE))

(defn kind
  "Returns the spawn facts of entity type t with its size and ground,
  or nil for a misc type. The ground is the block tag it spawns on."
  [t ground]
  (when-let [f (facts t)]
    (let [{:keys [width height]} (get (data/entities) t)]
      (assoc f :type t :ground ground :width width :height height))))
