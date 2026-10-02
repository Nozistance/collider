(ns collider.world.blocks.grow.crop
  "Crops and the other plants that grow where they stand."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]
            [collider.world.blocks.grass :as grass]
            [collider.world.blocks.grow.common
             :refer [age aged air-at? chance? height-below lit?
                     older]]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(def max-age
  "The age at which each crop is ripe."
  {:crop 7 :carrot 7 :potato 7 :beetroot 3 :torchflower-crop 1
   :stem 7})

(defn- soil-speed ^double [chunks [x y z] ^long dx ^long dz]
  (let [q [(+ (long x) dx) (dec (long y)) (+ (long z) dz)]
        st (chunk/at chunks q)]
    (if (block/tagged? st "grows_crops")
      (if (pos? (block/prop-long st :moisture)) 3.0 1.0)
      0.0)))

(defn- crowded? [chunks [x y z] self]
  (let [same? (fn [^long dx ^long dz]
                (let [q [(+ (long x) dx) y (+ (long z) dz)]]
                  (= self (block/block-of (chunk/at chunks q)))))]
    (or (and (or (same? -1 0) (same? 1 0))
             (or (same? 0 -1) (same? 0 1)))
        (same? -1 -1) (same? 1 -1) (same? 1 1) (same? -1 1))))

(defn- soil-share ^double [chunks p ^long dx ^long dz]
  (let [s (soil-speed chunks p dx dz)]
    (if (and (zero? dx) (zero? dz)) s (/ s 4.0))))

(defn- growth-speed ^double [chunks p self]
  (let [shares (for [dx [-1 0 1] dz [-1 0 1]]
                 (soil-share chunks p dx dz))
        speed (reduce + 1.0 shares)]
    (if (crowded? chunks p self) (/ speed 2.0) speed)))

(defn- growth-roll? [chunks p st roll]
  (let [speed (growth-speed chunks p (block/block-of st))]
    (chance? roll :grow (inc (long (/ 25.0 speed))))))

(defn- grows-now? [chunks p st roll]
  (and (lit? chunks p 9) (growth-roll? chunks p st roll)))

(defn- gated? [t roll]
  (and (#{:beetroot :torchflower-crop} t) (chance? roll :gate 3)))

(defn tick
  "Returns the changes of a random tick of the crop st at p."
  [chunks p st roll _time _world]
  (let [t (block/type-of st)]
    (when (and (< (age st) (long (max-age t)))
               (not (gated? t roll))
               (grows-now? chunks p st roll))
      [[p (older st) nil update/clients]])))

(defn- room-above? [chunks p ^long a]
  (or (< a 3)
      (let [u (chunk/at chunks (dir/up p))]
        (or (zero? u) (= :pitcher-crop (block/type-of u))))))

(defn- pitcher-grown [chunks p st ^long a]
  (when (and (lit? chunks p 8)
             (chunk/in-range? (inc (long (p 1))))
             (room-above? chunks p a))
    (let [st' (aged st a)]
      (cond-> [[p st' nil update/clients]]
        (>= a 3) (conj [(dir/up p) (block/with st' :half :upper)])))))

(defn- lower? [st] (= :lower (:half (block/props-of st))))

(defn pitcher-tick
  "Returns the changes of a random tick of the pitcher crop st at p."
  [chunks p st roll _time _world]
  (when (and (lower? st)
             (< (age st) 4)
             (growth-roll? chunks p st roll))
    (pitcher-grown chunks p st (inc (age st)))))

(defn pitcher-meal
  "Returns the bone meal result for the pitcher crop st at p."
  [chunks p st _roll]
  (let [lp (if (lower? st) p (dir/down p))
        lst (if (lower? st) st (chunk/at chunks lp))]
    (when (and (= :pitcher-crop (block/type-of lst)) (< (age lst) 4))
      (when-let [cs (pitcher-grown chunks lp lst (inc (age lst)))]
        {:changes cs}))))

(defn- fruit-changes [chunks p st roll]
  (let [[fruit attached tag] (support/stem-fruits (block/block-of st))
        dir (dir/horizontal (random/below (roll :dir) 4))
        beside (dir/toward p dir)
        soil (chunk/at chunks (dir/down beside))]
    (when (and (air-at? chunks beside) (block/tagged? soil tag))
      [[beside (block/state fruit)]
       [p (block/state attached {:facing dir})]])))

(defn stem-tick
  "Returns the changes of a random tick of the stem st at p. A ripe
  stem grows a fruit beside it."
  [chunks p st roll _time _world]
  (when (grows-now? chunks p st roll)
    (if (< (age st) (long (max-age :stem)))
      [[p (older st) nil update/clients]]
      (fruit-changes chunks p st roll))))

(defn cane-tick
  "Returns the changes of a random tick of the sugar cane st at p."
  [chunks p st _roll _time _world]
  (let [h (height-below chunks p (block/block-of st) 3)]
    (when (and (air-at? chunks (dir/up p)) (< (inc h) 3))
      (if (= 15 (age st))
        [[(dir/up p) (block/state (block/block-of st))]
         [p (aged st 0) nil update/quiet]]
        [[p (older st) nil update/quiet]]))))

(defn- cactus-top [chunks p st roll a height]
  (let [a (long a) height (long height) up (dir/up p)
        cactus (block/state :cactus)]
    (cond
      (and (= a 8) (support/supported? chunks up cactus))
      (when (<= (double (roll :flower)) (if (>= height 3) 0.25 0.1))
        [[up (block/state :cactus-flower)]])
      (and (= a 15) (< height 3))
      (let [st' (aged st 0)]
        [[up cactus]
         [p st' [[:neighbor-changed up st']] update/quiet]]))))

(defn cactus-tick
  "Returns the changes of a random tick of the cactus st at p."
  [chunks p st roll _time _world]
  (when (air-at? chunks (dir/up p))
    (let [a (age st)
          h (inc (height-below chunks p :cactus 3))]
      (when-not (and (>= h 3) (= a 15))
        (into (vec (cactus-top chunks p st roll a h))
              (when (< a 15)
                [[p (aged st (inc a)) nil update/quiet]]))))))

(defn berry-tick
  "Returns the changes of a random tick of the berry bush st at p."
  [chunks p st roll _time _world]
  (when (and (< (age st) 3)
             (chance? roll :gate 5)
             (lit? chunks (dir/up p) 9))
    [[p (older st) nil update/clients]]))

(defn kelp-tick
  "Returns the kelp a random tick of the kelp st at p grows above."
  [chunks p st roll _time _world]
  (when (and (< (age st) 25) (< (double (roll :grow)) 0.14)
             (block/water? (chunk/at chunks (dir/up p))))
    [[(dir/up p) (older st)]]))

(defn cocoa-tick
  "Returns the changes of a random tick of the cocoa st at p."
  [_chunks p st roll _time _world]
  (when (and (chance? roll :gate 5) (< (age st) 2))
    [[p (older st) nil update/clients]]))

(defn nether-wart-tick
  "Returns the changes of a random tick of the nether wart st at p."
  [_chunks p st roll _time _world]
  (when (and (< (age st) 3) (chance? roll :gate 10))
    [[p (older st) nil update/clients]]))

(defn- meal-steps ^long [t roll]
  (case t
    :beetroot (quot (+ 2 (random/below (roll :meal) 4)) 3)
    :torchflower-crop 1
    (+ 2 (random/below (roll :meal) 4))))

(defn- ripe-fruit [chunks p st st' roll]
  (when (and (= :stem (block/type-of st))
             (= (age st') (long (max-age :stem)))
             (grows-now? chunks p st roll))
    (fruit-changes chunks p st' roll)))

(defn meal
  "Returns the bone meal result for the crop or stem st at p."
  [chunks p st roll]
  (let [t (block/type-of st) a (age st) top (long (max-age t))]
    (when (< a top)
      (let [st' (aged st (min top (+ a (meal-steps t roll))))]
        {:changes (into [[p st' nil update/clients]]
                        (ripe-fruit chunks p st st' roll))}))))

(defn berry-meal
  "Returns the bone meal result for the berry bush st at p."
  [_chunks p st _roll]
  (when (< (age st) 3)
    {:changes [[p (older st) nil update/clients]]}))

(defn cocoa-meal
  "Returns the bone meal result for the cocoa st at p."
  [_chunks p st _roll]
  (when (< (age st) 2)
    {:changes [[p (older st) nil update/clients]]}))

(defn kelp-meal
  "Returns the kelp bone meal grows above the kelp st at p."
  [chunks p st _roll]
  (when (and (< (age st) 25)
             (block/water? (chunk/at chunks (dir/up p))))
    {:changes [[(dir/up p) (older st)]]}))

(defn tall-flower-meal
  "Returns the drop of a tall flower that bone meal copies."
  [_chunks _p st _roll]
  (when (lower? st)
    {:drops [{:item (block/block-of st) :count 1}]}))

(defn tall-grass-meal
  "Returns the tall plant bone meal grows from the short grass or fern
  st at p."
  [chunks p st _roll]
  (let [[tall upper] (grass/tall-of st)]
    (when (and (air-at? chunks (dir/up p))
               (support/supported? chunks p tall))
      {:changes [[p tall nil update/clients]
                 [(dir/up p) upper nil update/clients]]})))

(defn petals-meal
  "Returns the bone meal result for the flower bed st at p."
  [_chunks p st _roll]
  (let [n (block/prop-long st :flower-amount)]
    (if (< n 4)
      {:changes [[p (block/with st :flower-amount (inc n))
                  nil update/clients]]}
      {:drops [{:item (block/block-of st) :count 1}]})))
