(ns collider.world.blocks.grow.ground
  "Growth of the ground blocks and of what stands on them."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.blocks.grass :as grass]
            [collider.world.blocks.grow.common
             :refer [air-at? chance? flag pick water? with]]
            [collider.world.light :as light]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.multiface :as multiface]
            [collider.world.blocks.support :as support]
            [collider.world.env.weather :as weather]
            [collider.world.feature :as feature]))

(set! *warn-on-reflection* true)

(defn- snowy [chunks p]
  (flag (block/tagged? (chunk/at chunks (dir/up p)) "snow")))

(defn- spread-target? [chunks fresh q]
  (and (= :dirt (block/block-of (chunk/at chunks q)))
       (grass/can-stay-alive? chunks fresh q)
       (not (water? (chunk/at chunks (dir/up q))))))

(defn- spread-offset [roll i]
  [(dec (pick roll [:x i] 3))
   (- (pick roll [:y i] 5) 3)
   (dec (pick roll [:z i] 3))])

(defn- spread-cell [chunks p self fresh roll i]
  (let [q (mapv + p (spread-offset roll i))]
    (when (and (chunk/in-range? (q 1))
               (spread-target? chunks fresh q))
      [q (block/state self {:snowy (snowy chunks q)})])))

(defn- spread-cells [chunks p st roll]
  (let [self (block/block-of st) fresh (block/state self)]
    (into [] (keep #(spread-cell chunks p self fresh roll %))
          (range 4))))

(defn spread-tick
  "Returns the changes of a random tick of grass or mycelium at p."
  [chunks p st roll time ctx]
  (let [y (inc (long (p 1)))]
    (if-not (grass/can-stay-alive? chunks st p)
      [[p (block/state :dirt)]]
      (when (>= (weather/brightness ctx chunks (p 0) y (p 2) time) 9)
        (spread-cells chunks p st roll)))))

(def ^:private water-offsets
  (for [dx (range -4 5) dy [0 1] dz (range -4 5)] [dx dy dz]))

(defn- near-water? [chunks p]
  (boolean (some #(water? (chunk/at chunks (mapv + p %)))
                 water-offsets)))

(defn farmland-tick
  "Returns the changes of a random tick of farmland at p."
  [chunks p st _roll _time _ctx]
  (let [m (block/prop-long st :moisture)
        above (chunk/at chunks (dir/up p))]
    (cond
      (near-water? chunks p)
      (when (< m 7) [[p (with st :moisture 7)]])
      (pos? m) [[p (with st :moisture (dec m))]]
      (not (block/tagged? above "maintains_farmland"))
      [[p (block/state :dirt)]])))

(defn- amethyst-next [^long target dir]
  (let [n (block/block-of target)]
    (cond
      (or (zero? target) (block/water-source? target))
      :small-amethyst-bud
      (not= dir (block/facing-of target)) nil
      (= :small-amethyst-bud n) :medium-amethyst-bud
      (= :medium-amethyst-bud n) :large-amethyst-bud
      (= :large-amethyst-bud n) :amethyst-cluster)))

(defn budding-tick
  "Returns the bud a random tick of budding amethyst at p grows."
  [chunks p _st roll _time _ctx]
  (when (chance? roll :gate 5)
    (let [dir (dir/six (pick roll :dir 6))
          q (mapv + p (dir/offset dir))
          target (chunk/at chunks q)
          wet (if (water? target) :true :false)]
      (when-let [b (amethyst-next target dir)]
        [[q (block/state b {:facing dir :waterlogged wet})]]))))

(defn- block-light ^long [chunks p]
  (long (light/block-light-at chunks (p 0) (p 1) (p 2))))

(defn ice-tick
  "Returns the water that ice at p melts into in bright light."
  [chunks p st _roll _time _ctx]
  (when (> (block-light chunks p) (- 11 (block/dampening st)))
    [[p (block/state :water)]]))

(defn snow-tick
  "Returns the change that melts snow at p in bright light."
  [chunks p _st _roll _time _ctx]
  (when (> (block-light chunks p) 11)
    [[p 0]]))

(defn eyeblossom-tick
  "Returns the change that opens or closes an eyeblossom."
  [_chunks p st _roll time _ctx]
  (when-let [new (eyeblossom/switched (long st) (long time))]
    [[p new]]))

(def ^:private potted-eyeblossom
  {:potted-open-eyeblossom   :potted-closed-eyeblossom
   :potted-closed-eyeblossom :potted-open-eyeblossom})

(defn potted-tick
  "Returns the change that opens or closes a potted eyeblossom."
  [_chunks p st _roll time _ctx]
  (let [self (block/block-of (long st))]
    (when (contains? potted-eyeblossom self)
      (let [open? (= :potted-open-eyeblossom self)
            night? (<= 12600 (mod (long time) 24000) 23400)]
        (when (not= open? night?)
          [[p (block/state (potted-eyeblossom self))]])))))

(defn leaves-tick
  "Returns the change that removes leaves too far from their log.
  Returns nil when they stay."
  [_chunks p st _roll _time _ctx]
  (when (and (= :false (:persistent (block/props-of st)))
             (= 7 (block/prop-long st :distance)))
    [[p (block/emptied st)]]))

(defn chorus-tick
  "Returns the growth of a random tick of a chorus flower."
  [chunks p st roll _time _ctx]
  (chorus/flower-tick chunks p st
                      (fn [salt ^long n] (pick roll salt n))))

(defn roots-meal
  "Returns the hanging roots bone meal grows under rooted dirt."
  [chunks [_ y _ :as p] _st _roll]
  (when (and (chunk/in-range? (dec (long y)))
             (zero? (chunk/at chunks (dir/down p))))
    {:changes [[(dir/down p) (block/state :hanging-roots)]]}))

(defn lichen-meal
  "Returns the spread bone meal gives glow lichen."
  [chunks p st roll]
  (when-let [changes (multiface/spread-random chunks p st roll)]
    {:changes changes}))

(defn carpet-meal
  "Returns the moss bone meal grows on a pale moss carpet."
  [chunks p st _roll]
  (when (= :true (:bottom (block/props-of st)))
    (when-let [topper (moss/carpet-topper chunks p (constantly true))]
      {:changes [[(dir/up p) topper]]})))

(defn hanging-moss-meal
  "Returns the moss bone meal adds to the end of hanging moss."
  [chunks p st _roll]
  (let [q (moss/hanging-end chunks p (block/block-of st))]
    (when (air-at? chunks q)
      {:changes [[q (with st :tip :true)]]})))

(defn- shuffled-dirs [roll]
  (loop [pool (vec dir/horizontal) i 0 acc []]
    (if (= i 3)
      (into acc pool)
      (let [j (pick roll [:shuffle i] (- 4 i))
            left (into (subvec pool 0 j) (subvec pool (inc j)))]
        (recur left (inc i) (conj acc (pool j)))))))

(defn- meal-room? [chunks q target]
  (and (air-at? chunks q) (support/supported? chunks q target)))

(defn spread-meal
  "Returns target put beside p where bone meal finds room."
  [chunks p roll target]
  (let [beside (fn [d] (mapv + p (dir/horizontal-offset d)))
        q (->> (shuffled-dirs roll)
               (map beside)
               (filter #(meal-room? chunks % target))
               first)]
    (when q {:changes [[q target]]})))

(defn bush-meal
  "Returns the bush bone meal spreads beside a bush."
  [chunks p st roll]
  (spread-meal chunks p roll (block/state (block/block-of st))))

(defn short-dry-grass-meal
  "Returns short dry grass grown tall by bone meal."
  [_chunks p _st _roll]
  {:changes [[p (block/state :tall-dry-grass)]]})

(defn tall-dry-grass-meal
  "Returns the short dry grass bone meal spreads beside tall."
  [chunks p _st roll]
  (spread-meal chunks p roll (block/state :short-dry-grass)))

(defn- pickle-cells [[x y z]]
  (for [[i span] (map-indexed vector [1 3 5 3 1])
        :let [z-off ([0 1 2 1 0] i)]
        dz (range span)]
    [(+ (long x) -2 (long i)) y (+ (long z) (- (long z-off)) dz)]))

(defn- coral-below? [chunks p]
  (block/tagged? (chunk/at chunks (dir/down p)) "coral_blocks"))

(defn- pickle-spot? [chunks p roll i dy q]
  (and (not= q p)
       (zero? (pick roll [:seed i dy] 6))
       (block/water? (chunk/at chunks q))
       (coral-below? chunks q)))

(defn- pickle-state [roll i dy]
  (let [n (keyword (str (inc (pick roll [:n i dy] 4))))]
    (block/state :sea-pickle {:pickles n :waterlogged :true})))

(defn- column-spots [chunks p roll [i [qx qy qz]]]
  (for [dy [-1 0]
        :let [q [qx (+ (long qy) (long dy)) qz]]
        :when (pickle-spot? chunks p roll i dy q)]
    [q (pickle-state roll i dy)]))

(defn- pickle-spots [chunks p roll]
  (into []
        (comp (map-indexed vector)
              (mapcat #(column-spots chunks p roll %)))
        (pickle-cells p)))

(defn pickle-meal
  "Returns the sea pickles bone meal spreads over coral."
  [chunks p st roll]
  (when (and (= :true (:waterlogged (block/props-of st)))
             (coral-below? chunks p))
    {:changes (conj (pickle-spots chunks p roll)
                    [p (with st :pickles 4)])}))

(defn- grown-tall [acc q ^long st]
  (let [fern? (= :fern (block/block-of st))
        tall (block/state (if fern? :large-fern :tall-grass))
        upper (block/state (block/block-of tall) {:half :upper})
        up (dir/up q)]
    (when (and (support/supported? (feature/chunks acc) q tall)
               (air-at? (feature/chunks acc) up))
      (-> acc
          (feature/set-state q tall)
          (feature/set-state up upper)))))

(defn- turf-short [acc q st j roll]
  (if (and (= :short-grass (block/block-of (long st)))
           (zero? (pick roll [:tall j] 10)))
    (or (grown-tall acc q st) acc)
    acc))

(defn- turf-flower [acc q j roll salt biome]
  (let [fs (feature/bone-meal-features biome)]
    (if (empty? fs)
      acc
      (let [f (nth fs (pick roll [:which j] (count fs)))]
        (feature/configured acc f q roll salt)))))

(defn- turf-plant [acc q st j roll biome]
  (let [salt [:grow j]]
    (cond
      (not (and (zero? (long st)) (chunk/in-range? (q 1)))) acc
      (pos? (pick roll [:kind j] 8))
      (feature/placed acc :grass-bonemeal q roll salt)
      :else (turf-flower acc q j roll salt biome))))

(defn- walk-step [roll s]
  (let [dy (* (dec (pick roll (conj s :y) 3))
              (pick roll (conj s :n) 3))]
    [(dec (pick roll (conj s :x) 3))
     (quot dy 2)
     (dec (pick roll (conj s :z) 3))]))

(defn- walkable? [acc self q]
  (and (= self (block/block-of (feature/state-at acc (dir/down q))))
       (not (block/full-cube? (feature/state-at acc q)))))

(defn- turf-walk [acc self p j roll]
  (loop [q (dir/up p) i 0]
    (if (= i (quot (long j) 16))
      q
      (let [q' (mapv + q (walk-step roll [:walk j i]))]
        (when (walkable? acc self q')
          (recur q' (inc i)))))))

(defn- turf-try [acc self p j roll biome]
  (if-let [q (turf-walk acc self p j roll)]
    (let [st (feature/state-at acc q)]
      (-> (turf-short acc q st j roll)
          (turf-plant q st j roll biome)))
    acc))

(defn turf-meal
  "Returns the bone meal result for a grass block at p. It puts grass,
  tall grass and the flowers of biome around it."
  [chunks p st roll biome]
  (when (air-at? chunks (dir/up p))
    (let [self (block/block-of st)
          n (:name biome)
          acc (reduce (fn [acc j] (turf-try acc self p j roll n))
                      (feature/start chunks) (range 128))]
      {:changes (feature/cells acc)})))

(defn placer-meal
  "Returns the patch bone meal grows on a block that places one."
  [chunks p st roll]
  (when (air-at? chunks (dir/up p))
    (let [f (feature/placer-feature (block/block-of st))
          start (feature/start chunks)
          acc (feature/configured start f (dir/up p) roll [:patch])]
      {:changes (feature/cells acc)})))
