(ns collider.world.blocks.grow.crop
  "Crops and the other plants that grow where they stand."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.grow.common
             :refer [age aged air-at? chance? lit? pick water? with
                     height-below]]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(def max-age
  {:crop 7 :carrot 7 :potato 7 :beetroot 3 :torchflower-crop 1
   :stem 7})

(defn- older [st] (aged st (inc (age st))))

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

(defn tick [chunks p st roll _time _ctx]
  (let [t (block/type-of st)]
    (when (and (< (age st) (long (max-age t)))
               (not (gated? t roll))
               (grows-now? chunks p st roll))
      [[p (older st)]])))

(defn- room-above? [chunks p ^long a]
  (or (< a 3)
      (let [u (chunk/at chunks (dir/up p))]
        (or (zero? u) (= :pitcher-crop (block/type-of u))))))

(defn- pitcher-grown [chunks p st ^long a]
  (when (and (lit? chunks p 8)
             (chunk/in-range? (inc (long (p 1))))
             (room-above? chunks p a))
    (let [st' (aged st a)]
      (cond-> [[p st']]
        (>= a 3) (conj [(dir/up p) (with st' :half :upper)])))))

(defn- lower? [st] (= :lower (:half (block/props-of st))))

(defn pitcher-tick [chunks p st roll _time _ctx]
  (when (and (lower? st)
             (< (age st) 4)
             (growth-roll? chunks p st roll))
    (pitcher-grown chunks p st (inc (age st)))))

(defn pitcher-meal [chunks p st _roll]
  (let [lp (if (lower? st) p (dir/down p))
        lst (if (lower? st) st (chunk/at chunks lp))]
    (when (and (= :pitcher-crop (block/type-of lst)) (< (age lst) 4))
      (when-let [cs (pitcher-grown chunks lp lst (inc (age lst)))]
        {:changes cs}))))

(def ^:private fruits
  {:pumpkin-stem
   [:pumpkin :attached-pumpkin-stem "supports_pumpkin_stem_fruit"]
   :melon-stem
   [:melon :attached-melon-stem "supports_melon_stem_fruit"]})

(defn- fruit-changes [chunks p st roll]
  (let [[fruit attached tag] (fruits (block/block-of st))
        dir (dir/horizontal (pick roll :dir 4))
        beside (mapv + p (dir/horizontal-offset dir))
        soil (chunk/at chunks (dir/down beside))]
    (when (and (air-at? chunks beside) (block/tagged? soil tag))
      [[beside (block/state fruit)]
       [p (block/state attached {:facing dir})]])))

(defn stem-tick [chunks p st roll _time _ctx]
  (when (grows-now? chunks p st roll)
    (if (< (age st) 7)
      [[p (older st)]]
      (fruit-changes chunks p st roll))))

(def ^:private stems
  (into {} (for [[stem [fruit attached _]] fruits]
             [attached [fruit stem]])))

(defn- fruitless? [chunks p ^long st]
  (let [[fruit _] (stems (block/block-of st))
        off (dir/horizontal-offset (block/facing-of st))
        beside (mapv + p off)]
    (not= fruit (block/block-of (chunk/at chunks beside)))))

(defn- detached ^long [^long st]
  (block/state (second (stems (block/block-of st))) {:age :7}))

(defn- attached-due [chunks p _ctx]
  (let [st (chunk/at chunks p)]
    (cond
      (not (support/supported? chunks p st))
      [[p (support/gone-state st)]]
      (fruitless? chunks p st) [[p (detached st)]])))

(def attached-stem-rule
  {:name    :attached-stem
   :match?  (fn [_chunks st _p]
              (= :attached-stem (block/type-of st)))
   :wake    (fn [_chunks _dim _tick _p _old _side] :neighbor)
   :reshape attached-due})

(defn cane-tick [chunks p st _roll _time _ctx]
  (let [h (height-below chunks p (block/block-of st) 3)]
    (when (and (air-at? chunks (dir/up p)) (< (inc h) 3))
      (if (= 15 (age st))
        [[(dir/up p) (block/state (block/block-of st))]
         [p (aged st 0)]]
        [[p (older st)]]))))

(defn- cactus-top [chunks p st roll a h]
  (let [up (dir/up p)
        cactus (block/state :cactus)]
    (cond
      (and (= a 8) (support/supported? chunks up cactus))
      (when (<= (double (roll :flower)) (if (>= h 3) 0.25 0.1))
        [[up (block/state :cactus-flower)]])
      (and (= a 15) (< h 3))
      [[up cactus] [p (aged st 0)]])))

(defn cactus-tick [chunks p st roll _time _ctx]
  (when (air-at? chunks (dir/up p))
    (let [a (age st)
          h (inc (height-below chunks p :cactus 3))]
      (when-not (and (>= h 3) (= a 15))
        (into (vec (cactus-top chunks p st roll a h))
              (when (< a 15) [[p (aged st (inc a))]]))))))

(defn berry-tick [chunks p st roll _time _ctx]
  (when (and (< (age st) 3)
             (chance? roll :gate 5)
             (lit? chunks (dir/up p) 9))
    [[p (older st)]]))

(defn kelp-tick [chunks p st roll _time _ctx]
  (when (and (< (age st) 25) (< (double (roll :grow)) 0.14)
             (block/water? (chunk/at chunks (dir/up p))))
    [[(dir/up p) (older st)]]))

(defn cocoa-tick [_chunks p st roll _time _ctx]
  (when (and (chance? roll :gate 5) (< (age st) 2))
    [[p (older st)]]))

(defn nether-wart-tick [_chunks p st roll _time _ctx]
  (when (and (< (age st) 3) (chance? roll :gate 10))
    [[p (older st)]]))

(defn- hanging? [st] (= :true (:hanging (block/props-of st))))

(defn propagule-tick [_chunks p st _roll _time _ctx]
  (when (and (hanging? st) (< (age st) 4))
    [[p (older st)]]))

(defn- meal-steps ^long [t roll]
  (case t
    :beetroot (quot (+ 2 (pick roll :meal 4)) 3)
    :torchflower-crop 1
    (+ 2 (pick roll :meal 4))))

(defn meal [chunks p st roll]
  (let [t (block/type-of st) a (age st) top (long (max-age t))]
    (when (< a top)
      (let [a' (min top (+ a (meal-steps t roll)))
            st' (aged st a')
            fruit? (and (= :stem t) (= a' 7)
                        (grows-now? chunks p st roll))
            fruit (when fruit? (fruit-changes chunks p st' roll))]
        {:changes (into [[p st']] fruit)}))))

(defn berry-meal [_chunks p st _roll]
  (when (< (age st) 3) {:changes [[p (older st)]]}))

(defn cocoa-meal [_chunks p st _roll]
  (when (< (age st) 2) {:changes [[p (older st)]]}))

(defn kelp-meal [chunks p st _roll]
  (when (and (< (age st) 25)
             (block/water? (chunk/at chunks (dir/up p))))
    {:changes [[(dir/up p) (older st)]]}))

(defn propagule-meal [_chunks p st _roll]
  (when (and (hanging? st) (< (age st) 4))
    {:changes [[p (older st)]]}))

(defn seagrass-meal [chunks p _st _roll]
  (let [half (fn [h] (block/state :tall-seagrass {:half h}))]
    (when (water? (chunk/at chunks (dir/up p)))
      {:changes [[p (half :lower)] [(dir/up p) (half :upper)]]})))

(defn tall-flower-meal [_chunks _p st _roll]
  (when (lower? st)
    {:drops [{:item (block/block-of st) :count 1}]}))

(defn doubled [chunks p st _roll]
  (let [fern? (= :fern (block/block-of st))
        tall (block/state (if fern? :large-fern :tall-grass))
        upper (block/state (block/block-of tall) {:half :upper})]
    (when (and (air-at? chunks (dir/up p))
               (support/supported? chunks p tall))
      {:changes [[p tall] [(dir/up p) upper]]})))

(defn petals-meal [_chunks p st _roll]
  (let [n (block/prop-long st :flower-amount)]
    (if (< n 4)
      {:changes [[p (with st :flower-amount (inc n))]]}
      {:drops [{:item (block/block-of st) :count 1}]})))
