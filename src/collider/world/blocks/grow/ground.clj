(ns collider.world.blocks.grow.ground
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.gen :as gen]
            [collider.world.blocks.grass :as grass]
            [collider.world.blocks.grow.common :refer [air-at? chance? flag pick water? with]]
            [collider.world.light :as light]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.multiface :as multiface]
            [collider.world.blocks.support :as support]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(defn- snowy [chunks p] (flag (block/tagged? (gen/at chunks (dir/up p)) "snow")))

(defn- spread-target? [chunks fresh q]
  (and (= :dirt (block/block-of (gen/at chunks q)))
       (grass/can-stay-alive? chunks fresh q)
       (not (water? (gen/at chunks (dir/up q))))))

(defn- spread-cells [chunks p st roll]
  (let [self (block/block-of st) fresh (block/state self)]
    (into [] (keep (fn [i]
                     (let [q (mapv + p [(dec (pick roll [:x i] 3)) (- (pick roll [:y i] 5) 3) (dec (pick roll [:z i] 3))])]
                       (when (and (chunk/in-range? (q 1)) (spread-target? chunks fresh q))
                         [q (block/state self {:snowy (snowy chunks q)})]))))
          (range 4))))

(defn spread-tick [chunks p st roll time ctx]
  (if-not (grass/can-stay-alive? chunks st p)
    [[p (block/state :dirt)]]
    (when (>= (weather/brightness ctx chunks gen/flat-chunk (p 0) (inc (long (p 1))) (p 2) time) 9)
      (spread-cells chunks p st roll))))

(defn- near-water? [chunks [x y z]]
  (boolean (some (fn [[dx dy dz]] (water? (gen/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])))
                 (for [dx (range -4 5) dy [0 1] dz (range -4 5)] [dx dy dz]))))

(defn farmland-tick [chunks p st _roll _time _ctx]
  (let [m (block/prop-long st :moisture)]
    (cond
      (near-water? chunks p) (when (< m 7) [[p (with st :moisture 7)]])
      (pos? m) [[p (with st :moisture (dec m))]]
      (not (block/tagged? (gen/at chunks (dir/up p)) "maintains_farmland")) [[p (block/state :dirt)]])))

(defn- amethyst-next [^long target dir]
  (let [n (block/block-of target)]
    (cond
      (or (zero? target) (and (= :water (liquid/liquid-class target)) (liquid/source-state? target))) :small-amethyst-bud
      (not= dir (block/facing-of target)) nil
      (= :small-amethyst-bud n) :medium-amethyst-bud
      (= :medium-amethyst-bud n) :large-amethyst-bud
      (= :large-amethyst-bud n) :amethyst-cluster)))

(defn budding-tick [chunks p _st roll _time _ctx]
  (when (chance? roll :gate 5)
    (let [dir (dir/six (pick roll :dir 6))
          q (mapv + p (dir/offset dir))
          target (gen/at chunks q)]
      (when-let [b (amethyst-next target dir)]
        [[q (block/state b {:facing dir :waterlogged (if (water? target) :true :false)})]]))))

(defn ice-tick [chunks p st _roll _time _ctx]
  (when (> (long (light/block-light-at chunks gen/flat-chunk (p 0) (p 1) (p 2))) (- 11 (block/dampening st)))
    [[p (block/state :water)]]))

(defn snow-tick [chunks p _st _roll _time _ctx]
  (when (> (long (light/block-light-at chunks gen/flat-chunk (p 0) (p 1) (p 2))) 11)
    [[p 0]]))

(defn eyeblossom-tick [_chunks p st _roll time _ctx]
  (when-let [new (eyeblossom/switched (long st) (long time))]
    [[p new]]))

(def ^:private potted-eyeblossom {:potted-open-eyeblossom   :potted-closed-eyeblossom
                                  :potted-closed-eyeblossom :potted-open-eyeblossom})

(defn potted-tick [_chunks p st _roll time _ctx]
  (let [self (block/block-of (long st))]
    (when (contains? potted-eyeblossom self)
      (let [open? (= :potted-open-eyeblossom self)
            night? (<= 12600 (mod (long time) 24000) 23400)]
        (when (not= open? night?)
          [[p (block/state (potted-eyeblossom self))]])))))

(defn leaves-tick [_chunks p st _roll _time _ctx]
  (when (and (= :false (:persistent (block/props-of st))) (= 7 (block/prop-long st :distance)))
    [[p (block/emptied st)]]))

(defn chorus-tick [chunks p st roll _time _ctx]
  (chorus/flower-tick chunks p st (fn [salt ^long n] (pick roll salt n))))

(defn roots-meal [chunks [_ y _ :as p] _st _roll]
  (when (and (chunk/in-range? (dec (long y))) (zero? (gen/at chunks (dir/down p))))
    {:changes [[(dir/down p) (block/state :hanging-roots)]]}))

(defn lichen-meal [chunks p st roll]
  (when-let [changes (multiface/spread-random chunks p st roll)]
    {:changes changes}))

(defn carpet-meal [chunks p st _roll]
  (when (= :true (:bottom (block/props-of st)))
    (when-let [topper (moss/carpet-topper chunks p (constantly true))]
      {:changes [[(dir/up p) topper]]})))

(defn hanging-moss-meal [chunks p st _roll]
  (let [q (moss/hanging-end chunks p (block/block-of st))]
    (when (air-at? chunks q)
      {:changes [[q (with st :tip :true)]]})))

(defn- shuffled-dirs [roll]
  (loop [pool (vec dir/horizontal) i 0 acc []]
    (if (= i 3)
      (into acc pool)
      (let [j (pick roll [:shuffle i] (- 4 i))]
        (recur (into (subvec pool 0 j) (subvec pool (inc j))) (inc i) (conj acc (pool j)))))))

(defn spread-meal [chunks p roll target]
  (when-let [q (first (for [d (shuffled-dirs roll)
                            :let [q (mapv + p (dir/horizontal-offset d))]
                            :when (and (air-at? chunks q)
                                       (support/supported? chunks gen/flat-chunk q target))]
                        q))]
    {:changes [[q target]]}))

(defn bush-meal [chunks p st roll] (spread-meal chunks p roll (block/state (block/block-of st))))
(defn short-dry-grass-meal [_chunks p _st _roll] {:changes [[p (block/state :tall-dry-grass)]]})
(defn tall-dry-grass-meal [chunks p _st roll] (spread-meal chunks p roll (block/state :short-dry-grass)))

(defn- pickle-cells [[x y z]]
  (for [[i span] (map-indexed vector [1 3 5 3 1])
        :let [z-off ([0 1 2 1 0] i)]
        dz (range span)]
    [(+ (long x) -2 (long i)) y (+ (long z) (- (long z-off)) dz)]))

(defn- pickle-spots [chunks p roll]
  (into []
        (comp (map-indexed vector)
              (mapcat (fn [[i [qx qy qz]]]
                        (for [dy [-1 0]
                              :let [q [qx (+ (long qy) (long dy)) qz]]
                              :when (and (not= q p)
                                         (zero? (pick roll [:seed i dy] 6))
                                         (= :water (liquid/liquid-class (gen/at chunks q)))
                                         (block/tagged? (gen/at chunks (dir/down q)) "coral_blocks"))]
                          [q (block/state :sea-pickle {:pickles     (keyword (str (inc (pick roll [:n i dy] 4))))
                                                       :waterlogged :true})]))))
        (pickle-cells p)))

(defn pickle-meal [chunks p st roll]
  (when (and (= :true (:waterlogged (block/props-of st)))
             (block/tagged? (gen/at chunks (dir/down p)) "coral_blocks"))
    {:changes (conj (pickle-spots chunks p roll) [p (with st :pickles 4)])}))
