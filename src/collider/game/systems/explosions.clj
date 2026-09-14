(ns collider.game.systems.explosions
  "Explosions: what each one breaks, and what it throws."
  (:require [clojure.data.int-map :as i]
            [collider.game.block.tnt :as tnt]
            [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.blocks.fire :as fire]
            [collider.world.block :as block]
            [collider.world.gen :as gen]
            [collider.world.space.explosion :as explosion]))

(set! *warn-on-reflection* true)

(defn- entity-box
  "Returns the width and height a blast sees an entity as."
  [e]
  (case (:type e)
    :player [0.3 1.8]
    :tnt [0.49 0.98]
    :item [0.125 0.25]
    (let [{:keys [half height]} (mobs/types (:type e))]
      [(or half 0.45) (or height 1.3)])))

(defn- blast-impulse
  "Returns the shove and the damage a blast gives at a point."
  [center [px _ pz] ey d12 density power]
  (let [ey (double ey) d12 (double d12) density (double density) power (double power)
        dx (- (double px) (double (center 0)))
        dy (- ey (double (center 1)))
        dz (- (double pz) (double (center 2)))
        d13 (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
    (when (pos? d13)
      (let [k (* (- 1.0 d12) density)
            dmg (Math/floor (+ 1.0 (* (/ (+ (* k k) k) 2.0) 7.0 2.0 power)))]
        [[(* (/ dx d13) k) (* (/ dy d13) k) (* (/ dz d13) k)] dmg]))))

(defn- knockback
  "Returns the shove and damage an entity takes from a blast, or nothing when it
   is out of reach or sheltered."
  [read [cx cy cz :as center] e p power]
  (let [px (v/x p) py (v/y p) pz (v/z p)
        dx (- (double px) (double cx))
        dy (- (double py) (double cy))
        dz (- (double pz) (double cz))
        d12 (/ (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) (* 2.0 (double power)))]
    (when (<= d12 1.0)
      (let [[half height] (entity-box e)
            density (explosion/block-density read center p half height)]
        (when (pos? density)
          (blast-impulse center [px py pz] (+ (double py) (entity/eye-height e)) d12 density power))))))

(def ^:private ^:const kb-cell 8)
(defn- kb-cell-key
  "Returns the key of the neighbourhood a position belongs to."
  ^long [^long x ^long y ^long z]
  (bit-or (bit-shift-left (+ (bit-shift-right x 3) 524288) 26)
          (bit-shift-left (+ (bit-shift-right z 3) 524288) 6)
          (bit-and (bit-shift-right y 3) 63)))

(defn- kb-index
  "Returns the entities gathered by the neighbourhood they stand in."
  [entries]
  (persistent!
    (reduce (fn [m [_ e :as entry]]
              (let [p (:pos e) x (v/x p) y (v/y p) z (v/z p)
                    k (kb-cell-key (long (Math/floor (double x))) (long (Math/floor (double y))) (long (Math/floor (double z))))]
                (assoc! m k (conj (get m k []) entry))))
            (transient (i/int-map))
            entries)))

(defn- kb-candidates
  "Returns the entities near enough to a point to be hit."
  [index [cx cy cz]]
  (let [x (long (Math/floor (double cx))) y (long (Math/floor (double cy))) z (long (Math/floor (double cz)))]
    (->> (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]]
           (get index (kb-cell-key (+ x (* dx kb-cell)) (+ y (* dy kb-cell)) (+ z (* dz kb-cell)))))
         (apply concat)
         (sort-by first))))

(defn- blast-one
  "Returns the running deltas with one entity's share of a blast added."
  [[motions ds] [oid o] kb dmg]
  (if (= :player (:type o))
    [(assoc motions oid kb) ds]
    [motions (cond-> (conj ds [:push oid kb])
                     (some? (:health o)) (conj [:damage oid dmg]))]))

(defn- blast-deltas
  "Returns the players' knockback and the deltas for everything else the blast throws."
  [read index center power later]
  (reduce (fn [acc [oid o :as entry]]
            (if-let [[kb dmg] (knockback read center o (get later oid (:pos o)) power)]
              (blast-one acc entry kb dmg)
              acc))
          [{} []]
          (kb-candidates index center)))

(defn- explosion-pitch
  "Returns the pitch of a blast's sound."
  ^double [seed]
  (* 0.7 (+ 1.0 (* 0.2 (- (random/of-key [seed :p1]) (random/of-key [seed :p2]))))))

(def ^:private decay-rules
  {:tnt :tnt-explosion-drop-decay
   :block :block-explosion-drop-decay
   :mob :mob-explosion-drop-decay})

(defn- decay-radius
  "Returns the radius that thins a blast's drops, or nil when they are not thinned."
  [world source power]
  (when (get-in world [:rules (get decay-rules source :block-explosion-drop-decay)]
                (not= :tnt source))
    power))

(defn- interacts?
  "Returns true when a blast is allowed to change the world."
  [world source]
  (or (not= :mob source) (get-in world [:rules :mob-griefing] true)))

(defn- drop-deltas [world rg destroy seed source power]
  (when (get-in world [:rules :block-drops] true)
    (map-indexed (fn [i [pos stack]]
                   [:spawn-entity (items/popped world pos stack i)])
                 (explosion/stacks rg destroy seed (decay-radius world source power)))))

(defn- fire-cells
  "Returns the fires a blast leaves behind."
  [rg affected gone seed]
  (into []
        (keep (fn [[x y z :as p]]
                (let [below (explosion/read-block rg (long x) (dec (long y)) (long z))]
                  (when (and (< (double (random/of-key [seed p :fire])) (/ 1.0 3.0))
                             (or (contains? gone p) (zero? (explosion/read-block rg (long x) (long y) (long z))))
                             (not (contains? gone [(long x) (dec (long y)) (long z)]))
                             (block/solid-render? below))
                    [p (fire/fire-state 0)]))))
        affected))

(defn- chain-cells
  "Returns the TNT a blast sets off."
  [rg affected gone primed tnt?]
  (when tnt?
    (into []
          (comp (filter (fn [[x y z]] (tnt/tnt-state? (explosion/read-block rg (long x) (long y) (long z)))))
                (remove primed)
                (remove gone))
          affected)))

(defn- explosion-reader [world [cx cy cz]]
  (explosion/block-reader (:chunks world) (gen/flat-chunk)
                          [(long (double cx)) (long (double cy)) (long (double cz))]))

(defn- break-cells
  "Returns the TNT a blast primes and the blocks it destroys."
  [world rg affected gone primed source]
  (if (interacts? world source)
    [(chain-cells rg affected gone primed (get-in world [:rules :tnt-explodes] true))
     (into [] (comp (remove primed) (remove gone)) affected)]
    [[] []]))

(defn- blast-acc
  "Returns the running deltas with one whole blast added."
  [world rg acc {:keys [center power seed source affected destroy chains fires motions pushes]}]
  (-> acc
      (cond-> (seq destroy) (conj [:set-blocks (mapv (fn [p] [p 0]) destroy)]))
      (conj (out/all (out/explosion center power (count affected) motions (explosion-pitch seed))))
      (into pushes)
      (into (map (fn [p] [:spawn-entity (assoc (tnt/chain-primed p seed) :origin nil)])) chains)
      (into (drop-deltas world rg destroy seed source power))
      (cond-> (seq fires) (conj [:set-blocks fires]))))

(defn- request-deltas
  "Returns the deltas for one blast, and what the blasts after it must know."
  [world index [gone primed acc] {:keys [center power source fire? by later]}]
  (let [power (double (or power tnt/power))
        seed [(:tick world) by]
        rg (explosion-reader world center)
        affected (explosion/affected-blocks rg center power seed)
        [chains destroy] (break-cells world rg affected gone primed source)
        gone' (into gone destroy)
        [motions pushes] (blast-deltas rg index center power later)
        fires (if fire? (fire-cells rg affected gone' seed) [])]
    [gone' (into primed chains)
     (blast-acc world rg acc {:center center :power power :seed seed :source source
                              :affected affected :destroy destroy :chains chains
                              :fires fires :motions motions :pushes pushes})]))

(defn- requests
  "Returns the blasts asked for this tick."
  [d]
  (into [] (comp (filter (fn [delta] (= :explode (nth delta 0)))) (map second)) (:world d)))

(defn explosions
  "Returns the deltas for every blast requested this tick, in the order they
   were asked for."
  [world d]
  (let [reqs (requests d)]
    (when (seq reqs)
      (let [index (kb-index (:entities world))]
        (nth (reduce (partial request-deltas world index)
                     [#{} (tnt/primed-origins world) []]
                     reqs)
             2)))))
