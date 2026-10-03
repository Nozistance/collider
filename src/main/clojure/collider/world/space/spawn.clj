(ns collider.world.space.spawn
  "Places where players and mobs may spawn."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.light :as light]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-attempts 1024)

(def ^:private ^:const eps 1.0E-7)

(def ^:private ^:const none (dec chunk/min-y))

(defn- state-at [chunks x y z]
  (if (chunk/in-range? (long y))
    (chunk/block-state chunks x y z)
    0))

(defn- fluid? [^long st] (some? (block/liquid-class st)))

(defn- air? [^long st] (block/air-type? st))

(defn- motion-blocking? [^long st]
  (or (block/blocks-motion? st) (fluid? st)))

(defn- own-height ^double [^long st]
  (let [l (block/liquid-level st)]
    (/ (double (if (or (zero? l) (>= l 8)) 8 (- 8 l))) 9.0)))

(defn- fluid-height [chunks x y z st]
  (if (= (block/liquid-class (long st))
         (block/liquid-class
           (long (state-at chunks x (inc (long y)) z))))
    1.0
    (own-height (long st))))

(defn- blank-at? [chunks ^long x ^long y ^long z]
  (let [id (chunk/pos->id (bit-shift-right x 4) (bit-shift-right z 4))
        c (get chunks id)
        s (when c (chunk/chunk-section c (chunk/section-index y)))]
    (or (nil? s) (identical? s chunk/empty-section))))

(defn- column-heights [chunks x z]
  (loop [y (long chunk/max-y) surface none motion none floor none]
    (cond
      (or (< y (long chunk/min-y)) (not= floor none))
      [surface motion floor]
      (blank-at? chunks x y z)
      (recur (dec (bit-and y -16)) surface motion floor)
      :else
      (let [st (long (state-at chunks x y z))
            top? (and (= motion none) (motion-blocking? st))]
        (recur (dec y)
               (if (and (= surface none) (not (air? st))) y surface)
               (if top? y motion)
               (if (block/blocks-motion? st) y floor))))))

(defn surface-top
  "Returns the y of the highest block of the column at x z that is
  not air, one below the lowest y when there is none."
  ^long [chunks x z]
  (long (nth (column-heights chunks x z) 0)))

(defn motion-blocking-height
  "Returns the y just above the top of the column at x z.
  The top is its highest block or fluid."
  ^long [chunks x z]
  (let [[_ motion _] (column-heights chunks x z)]
    (if (= (long motion) (long none))
      (long chunk/min-y)
      (inc (long motion)))))

(defn- overlaps? [lo box]
  (let [[x0 y0 z0 x1 y1 z1] lo [a b c d e f] box]
    (and (< (double x0) (double d)) (> (double x1) (double a))
         (< (double y0) (double e)) (> (double y1) (double b))
         (< (double z0) (double f)) (> (double z1) (double c)))))

(defn- box-at [^long x ^long y ^long z [a b c d e f]]
  [(+ x (/ (double a) 16.0)) (+ y (/ (double b) 16.0))
   (+ z (/ (double c) 16.0)) (+ x (/ (double d) 16.0))
   (+ y (/ (double e) 16.0)) (+ z (/ (double f) 16.0))])

(defn- fluid-box [chunks x y z st]
  (let [x (long x) y (long y) z (long z)
        h (double (fluid-height chunks x y z st))]
    [(double x) (double y) (double z) (+ x 1.0) (+ y h) (+ z 1.0)]))

(defn- cell-boxes [chunks x y z]
  (let [x (long x) y (long y) z (long z)
        st (long (state-at chunks x y z))
        solid (mapv #(box-at x y z %) (block/collision-boxes st))]
    (if (fluid? st)
      (conj solid (fluid-box chunks x y z st))
      solid)))

(def ^:private ^:table player-size
  (delay (let [{:keys [width height]} (:player (data/entities))]
           [(* 0.5 (double (float width))) (double (float height))])))

(defn- player-box [^long px ^long py ^long pz]
  (let [cx (+ px 0.5) cz (+ pz 0.5)
        w (double (nth @player-size 0))
        h (double (nth @player-size 1))]
    [(- cx w) (double py) (- cz w) (+ cx w) (+ py h) (+ cz w)]))

(defn- cell-edge ^long [^double v ^long d]
  (+ d (long (Math/floor (+ v (* d (double eps)))))))

(defn- player-box-free? [chunks px py pz]
  (let [box (player-box (long px) (long py) (long pz))
        [x0 y0 z0 x1 y1 z1] box
        i0 (cell-edge x0 -1) i1 (cell-edge x1 1)
        j0 (cell-edge y0 -1) j1 (cell-edge y1 1)
        k0 (cell-edge z0 -1) k1 (cell-edge z1 1)]
    (loop [i i0 j j0 k k0]
      (cond
        (> i i1) true
        (> j j1) (recur (inc i) j0 k0)
        (> k k1) (recur i (inc j) k0)
        (some (partial overlaps? box) (cell-boxes chunks i j k)) false
        :else (recur i j (inc k))))))

(defn- bottom-center [[x y z]]
  [(+ (long x) 0.5) (double y) (+ (long z) 0.5)])

(defn- open-top? [surface top floor]
  (not (and (<= (long surface) (long top))
            (> (long surface) (long floor)))))

(defn- level-respawn-pos [chunks x z]
  (let [[surface top floor] (column-heights chunks x z)]
    (when (and (>= (long top) (long chunk/min-y))
               (open-top? surface top floor))
      (loop [y (inc (long top))]
        (when (>= y (long chunk/min-y))
          (let [st (long (state-at chunks x y z))]
            (cond
              (fluid? st) nil
              (block/collision-face-full-up? st) [x (inc y) z]
              :else (recur (dec y)))))))))

(defn- rise [chunks x y z]
  (loop [y (long y)]
    (if (or (player-box-free? chunks x y z)
            (>= y (long chunk/max-y)))
      y
      (recur (inc y)))))

(defn- sink [chunks x y z]
  (loop [y (long y)]
    (if (or (not (player-box-free? chunks x y z))
            (<= y (long chunk/min-y)))
      y
      (recur (dec y)))))

(defn- fixup-height [chunks [x y z]]
  (let [x (long x) z (long z)
        up (rise chunks x y z)
        down (sink chunks x (dec (long up)) z)]
    (bottom-center [x (inc (long down)) z])))

(defn- coprime
  ^long [^long n] (if (<= n 16) (dec n) 17))

(defn- scan-params [radius seed]
  (let [radius (max 0 (long radius))
        side (inc (* 2 radius))
        n (min (long max-attempts) (* side side))
        offset (long (Math/floor (* (double seed) n)))]
    [radius side n (coprime n) offset]))

(defn- candidate-cell [params ^long ox ^long oz ^long i]
  (let [[radius side n step offset] params
        value (rem (+ (long offset) (* (long step) i)) (long n))]
    [(+ ox (rem value (long side)) (- (long radius)))
     (+ oz (quot value (long side)) (- (long radius)))]))

(defn- free-spawn [chunks x z]
  (when-let [[px py pz :as pos] (level-respawn-pos chunks x z)]
    (when (player-box-free? chunks px py pz)
      (bottom-center pos))))

(defn search-chunk-ids
  "Returns the ids of the chunks a spawn search reads.
  The search covers radius around suggestion."
  [suggestion radius]
  (let [r (max 0 (long radius))
        x (long (nth suggestion 0))
        z (long (nth suggestion 2))
        span (fn [^long c]
               (range (bit-shift-right (- c r) 4)
                      (inc (bit-shift-right (+ c r) 4))))]
    (for [cx (span x) cz (span z)] (chunk/pos->id cx cz))))

(defn find-spawn
  "Returns a standing position with room for a player.
  It looks at the columns within radius of suggestion in an
  order decided by seed. When no column is free, it returns a
  position above or below suggestion instead."
  [chunks suggestion radius seed]
  (let [[_ _ n :as params] (scan-params radius seed)
        ox (long (nth suggestion 0)) oz (long (nth suggestion 2))]
    (loop [i 0]
      (if (>= i (long n))
        (fixup-height chunks suggestion)
        (let [[x z] (candidate-cell params ox oz i)]
          (or (free-spawn chunks x z) (recur (inc i))))))))

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
  "Returns the mob categories in their order, each a pair of its name
  and its facts."
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
  "Returns the box of a spawn of kind k at x y z."
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
  "Returns the spawn facts of entity type t with its size and ground.
  The ground is the tag of the blocks it spawns on. A misc type has
  none."
  [t ground]
  (when-let [f (facts t)]
    (let [{:keys [width height]} (get (data/entities) t)]
      (assoc f :type t :ground ground :width width :height height))))
