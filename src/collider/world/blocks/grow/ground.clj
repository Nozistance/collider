(ns collider.world.blocks.grow.ground
  "Growth of the ground blocks and of what stands on them."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.grass :as grass]
            [collider.world.blocks.grow.common :refer [air-at? chance? flag pick water? with]]
            [collider.world.light :as light]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.multiface :as multiface]
            [collider.world.blocks.support :as support]
            [collider.world.env.biome :as biome]
            [collider.world.env.weather :as weather]
            [collider.world.feature :as feature]))

(set! *warn-on-reflection* true)

(defn- snowy [chunks p] (flag (block/tagged? (chunk/at chunks (dir/up p)) "snow")))

(defn- spread-target? [chunks fresh q]
  (and (= :dirt (block/block-of (chunk/at chunks q)))
       (grass/can-stay-alive? chunks fresh q)
       (not (water? (chunk/at chunks (dir/up q))))))

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
    (when (>= (weather/brightness ctx chunks (p 0) (inc (long (p 1))) (p 2) time) 9)
      (spread-cells chunks p st roll))))

(defn- near-water? [chunks [x y z]]
  (boolean (some (fn [[dx dy dz]] (water? (chunk/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])))
                 (for [dx (range -4 5) dy [0 1] dz (range -4 5)] [dx dy dz]))))

(defn farmland-tick [chunks p st _roll _time _ctx]
  (let [m (block/prop-long st :moisture)]
    (cond
      (near-water? chunks p) (when (< m 7) [[p (with st :moisture 7)]])
      (pos? m) [[p (with st :moisture (dec m))]]
      (not (block/tagged? (chunk/at chunks (dir/up p)) "maintains_farmland")) [[p (block/state :dirt)]])))

(defn- amethyst-next [^long target dir]
  (let [n (block/block-of target)]
    (cond
      (or (zero? target) (and (= :water (block/liquid-class target)) (block/source-state? target))) :small-amethyst-bud
      (not= dir (block/facing-of target)) nil
      (= :small-amethyst-bud n) :medium-amethyst-bud
      (= :medium-amethyst-bud n) :large-amethyst-bud
      (= :large-amethyst-bud n) :amethyst-cluster)))

(defn budding-tick [chunks p _st roll _time _ctx]
  (when (chance? roll :gate 5)
    (let [dir (dir/six (pick roll :dir 6))
          q (mapv + p (dir/offset dir))
          target (chunk/at chunks q)]
      (when-let [b (amethyst-next target dir)]
        [[q (block/state b {:facing dir :waterlogged (if (water? target) :true :false)})]]))))

(defn ice-tick [chunks p st _roll _time _ctx]
  (when (> (long (light/block-light-at chunks (p 0) (p 1) (p 2))) (- 11 (block/dampening st)))
    [[p (block/state :water)]]))

(defn snow-tick [chunks p _st _roll _time _ctx]
  (when (> (long (light/block-light-at chunks (p 0) (p 1) (p 2))) 11)
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

(defn leaves-tick
  "Returns the change that removes leaves grown too far from their log, or nil
   when they stay."
  [_chunks p st _roll _time _ctx]
  (when (and (= :false (:persistent (block/props-of st))) (= 7 (block/prop-long st :distance)))
    [[p (block/emptied st)]]))

(defn chorus-tick [chunks p st roll _time _ctx]
  (chorus/flower-tick chunks p st (fn [salt ^long n] (pick roll salt n))))

(defn roots-meal [chunks [_ y _ :as p] _st _roll]
  (when (and (chunk/in-range? (dec (long y))) (zero? (chunk/at chunks (dir/down p))))
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
                                       (support/supported? chunks q target))]
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
                                         (= :water (block/liquid-class (chunk/at chunks q)))
                                         (block/tagged? (chunk/at chunks (dir/down q)) "coral_blocks"))]
                          [q (block/state :sea-pickle {:pickles     (keyword (str (inc (pick roll [:n i dy] 4))))
                                                       :waterlogged :true})]))))
        (pickle-cells p)))

(defn pickle-meal [chunks p st roll]
  (when (and (= :true (:waterlogged (block/props-of st)))
             (block/tagged? (chunk/at chunks (dir/down p)) "coral_blocks"))
    {:changes (conj (pickle-spots chunks p roll) [p (with st :pickles 4)])}))

(defn- grown-tall [acc q ^long st]
  (let [tall (block/state (if (= :fern (block/block-of st)) :large-fern :tall-grass))
        up (dir/up q)]
    (when (and (support/supported? (feature/chunks acc) q tall)
               (air-at? (feature/chunks acc) up))
      (-> acc
          (feature/set-state q tall)
          (feature/set-state up (block/state (block/block-of tall) {:half :upper}))))))

(defn- turf-short [acc q st j roll]
  (if (and (= :short-grass (block/block-of (long st))) (zero? (pick roll [:tall j] 10)))
    (or (grown-tall acc q st) acc)
    acc))

(defn- turf-plant [acc q st j roll biome]
  (let [salt [:grow j]]
    (cond
      (not (and (zero? (long st)) (chunk/in-range? (q 1)))) acc
      (pos? (pick roll [:kind j] 8)) (feature/placed acc :grass-bonemeal q roll salt)
      :else (let [fs (feature/bone-meal-features biome)]
              (if (empty? fs)
                acc
                (feature/configured acc (nth fs (pick roll [:which j] (count fs))) q roll salt))))))

(defn- turf-walk [acc self p j roll]
  (loop [q (dir/up p) i 0]
    (if (= i (quot (long j) 16))
      q
      (let [s [:walk j i]
            q' (mapv + q [(dec (pick roll (conj s :x) 3))
                          (quot (* (dec (pick roll (conj s :y) 3)) (pick roll (conj s :n) 3)) 2)
                          (dec (pick roll (conj s :z) 3))])]
        (when (and (= self (block/block-of (feature/state-at acc (dir/down q'))))
                   (not (block/full-cube? (feature/state-at acc q'))))
          (recur q' (inc i)))))))

(defn- turf-try [acc self p j roll biome]
  (if-let [q (turf-walk acc self p j roll)]
    (let [st (feature/state-at acc q)]
      (-> (turf-short acc q st j roll)
          (turf-plant q st j roll biome)))
    acc))

(defn turf-meal
  "Returns the bone meal result for a grass block at p. It puts grass, tall
   grass and the flowers of the biome around it."
  [chunks p st roll]
  (when (air-at? chunks (dir/up p))
    (let [self (block/block-of st)
          biome (:name (biome/at chunks p))
          acc (reduce (fn [acc j] (turf-try acc self p j roll biome))
                      (feature/start chunks) (range 128))]
      {:changes (feature/cells acc)})))

(defn placer-meal [chunks p st roll]
  (when (air-at? chunks (dir/up p))
    (let [f (feature/placer-feature (block/block-of st))
          acc (feature/configured (feature/start chunks) f (dir/up p) roll [:patch])]
      {:changes (feature/cells acc)})))
