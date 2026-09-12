(ns collider.world.blocks.grow.crop
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]
            [collider.world.blocks.grow.common :refer [age aged air-at? chance? lit? pick water? with height-below]]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(def max-age {:crop 7 :carrot 7 :potato 7 :beetroot 3 :torchflower-crop 1 :stem 7})

(defn- soil-speed ^double [chunks [x y z] ^long dx ^long dz]
  (let [st (gen/at chunks [(+ (long x) dx) (dec (long y)) (+ (long z) dz)])]
    (if (block/tagged? st "grows_crops")
      (if (pos? (block/prop-long st :moisture)) 3.0 1.0)
      0.0)))

(defn- crowded? [chunks [x y z] self]
  (let [same? (fn [^long dx ^long dz] (= self (block/block-of (gen/at chunks [(+ (long x) dx) y (+ (long z) dz)]))))]
    (or (and (or (same? -1 0) (same? 1 0)) (or (same? 0 -1) (same? 0 1)))
        (same? -1 -1) (same? 1 -1) (same? 1 1) (same? -1 1))))

(defn- growth-speed ^double [chunks p self]
  (let [speed (reduce + 1.0 (for [dx [-1 0 1] dz [-1 0 1]]
                              (let [s (soil-speed chunks p dx dz)]
                                (if (and (zero? (long dx)) (zero? (long dz))) s (/ s 4.0)))))]
    (if (crowded? chunks p self) (/ speed 2.0) speed)))

(defn- growth-roll? [chunks p st roll]
  (chance? roll :grow (inc (long (/ 25.0 (growth-speed chunks p (block/block-of st)))))))

(defn- grows-now? [chunks p st roll]
  (and (lit? chunks p 9) (growth-roll? chunks p st roll)))

(defn tick [chunks p st roll _time _ctx]
  (let [t (block/type-of st) a (age st)]
    (when (and (< a (long (max-age t)))
               (or (not (#{:beetroot :torchflower-crop} t)) (not (chance? roll :gate 3)))
               (grows-now? chunks p st roll))
      [[p (aged st (inc a))]])))

(defn- pitcher-grown [chunks p st ^long a]
  (when (and (lit? chunks p 8)
             (chunk/in-range? (inc (long (p 1))))
             (or (< a 3) (let [u (gen/at chunks (dir/up p))] (or (zero? u) (= :pitcher-crop (block/type-of u))))))
    (let [st' (aged st a)]
      (cond-> [[p st']]
              (>= a 3) (conj [(dir/up p) (with st' :half :upper)])))))

(defn pitcher-tick [chunks p st roll _time _ctx]
  (when (and (= :lower (:half (block/props-of st))) (< (age st) 4) (growth-roll? chunks p st roll))
    (pitcher-grown chunks p st (inc (age st)))))

(defn pitcher-meal [chunks p st _roll]
  (let [lower? (= :lower (:half (block/props-of st)))
        lp (if lower? p (dir/down p))
        lst (if lower? st (gen/at chunks lp))]
    (when (and (= :pitcher-crop (block/type-of lst)) (< (age lst) 4))
      (when-let [changes (pitcher-grown chunks lp lst (inc (age lst)))]
        {:changes changes}))))

(def ^:private fruits
  {:pumpkin-stem [:pumpkin :attached-pumpkin-stem "supports_pumpkin_stem_fruit"]
   :melon-stem   [:melon :attached-melon-stem "supports_melon_stem_fruit"]})

(defn- fruit-changes [chunks p st roll]
  (let [[fruit attached tag] (fruits (block/block-of st))
        dir (dir/horizontal (pick roll :dir 4))
        beside (mapv + p (dir/horizontal-offset dir))]
    (when (and (air-at? chunks beside) (block/tagged? (gen/at chunks (dir/down beside)) tag))
      [[beside (block/state fruit)] [p (block/state attached {:facing dir})]])))

(defn stem-tick [chunks p st roll _time _ctx]
  (when (grows-now? chunks p st roll)
    (if (< (age st) 7)
      [[p (aged st (inc (age st)))]]
      (fruit-changes chunks p st roll))))

(defn cane-tick [chunks p st _roll _time _ctx]
  (when (and (air-at? chunks (dir/up p)) (< (inc (height-below chunks p (block/block-of st) 3)) 3))
    (if (= 15 (age st))
      [[(dir/up p) (block/state (block/block-of st))] [p (aged st 0)]]
      [[p (aged st (inc (age st)))]])))

(defn cactus-tick [chunks p st roll _time _ctx]
  (when (air-at? chunks (dir/up p))
    (let [a (age st) h (inc (height-below chunks p :cactus 3))]
      (when-not (and (>= h 3) (= a 15))
        (let [top (cond
                    (and (= a 8) (support/supported? chunks gen/flat-chunk (dir/up p) (block/state :cactus)))
                    (when (<= (double (roll :flower)) (if (>= h 3) 0.25 0.1)) [[(dir/up p) (block/state :cactus-flower)]])
                    (and (= a 15) (< h 3)) [[(dir/up p) (block/state :cactus)] [p (aged st 0)]])]
          (into (vec top) (when (< a 15) [[p (aged st (inc a))]])))))))

(defn berry-tick [chunks p st roll _time _ctx]
  (when (and (< (age st) 3) (chance? roll :gate 5) (lit? chunks (dir/up p) 9))
    [[p (aged st (inc (age st)))]]))

(defn kelp-tick [chunks p st roll _time _ctx]
  (when (and (< (age st) 25) (< (double (roll :grow)) 0.14)
             (= :water (liquid/liquid-class (gen/at chunks (dir/up p)))))
    [[(dir/up p) (aged st (inc (age st)))]]))

(defn cocoa-tick [_chunks p st roll _time _ctx]
  (when (and (chance? roll :gate 5) (< (age st) 2)) [[p (aged st (inc (age st)))]]))

(defn nether-wart-tick [_chunks p st roll _time _ctx]
  (when (and (< (age st) 3) (chance? roll :gate 10))
    [[p (aged st (inc (age st)))]]))

(defn propagule-tick [_chunks p st _roll _time _ctx]
  (when (and (= :true (:hanging (block/props-of st))) (< (age st) 4))
    [[p (aged st (inc (age st)))]]))

(defn meal [chunks p st roll]
  (let [t (block/type-of st) a (age st) top (long (max-age t))]
    (when (< a top)
      (let [n (case t :beetroot (quot (+ 2 (pick roll :meal 4)) 3) :torchflower-crop 1 (+ 2 (pick roll :meal 4)))
            a' (min top (+ a (long n)))]
        {:changes (if (and (= :stem t) (= a' 7) (grows-now? chunks p st roll))
                    (into [[p (aged st a')]] (fruit-changes chunks p (aged st a') roll))
                    [[p (aged st a')]])}))))

(defn berry-meal [_chunks p st _roll] (when (< (age st) 3) {:changes [[p (aged st (inc (age st)))]]}))
(defn cocoa-meal [_chunks p st _roll] (when (< (age st) 2) {:changes [[p (aged st (inc (age st)))]]}))

(defn kelp-meal [chunks p st _roll]
  (when (and (< (age st) 25) (= :water (liquid/liquid-class (gen/at chunks (dir/up p)))))
    {:changes [[(dir/up p) (aged st (inc (age st)))]]}))

(defn propagule-meal [_chunks p st _roll]
  (when (and (= :true (:hanging (block/props-of st))) (< (age st) 4))
    {:changes [[p (aged st (inc (age st)))]]}))

(defn seagrass-meal [chunks p _st _roll]
  (when (water? (gen/at chunks (dir/up p)))
    {:changes [[p (block/state :tall-seagrass {:half :lower})] [(dir/up p) (block/state :tall-seagrass {:half :upper})]]}))

(defn tall-flower-meal [_chunks _p st _roll]
  (when (= :lower (:half (block/props-of st))) {:drops [{:item (block/block-of st) :count 1}]}))

(defn doubled [chunks p st _roll]
  (let [tall (block/state (if (= :fern (block/block-of st)) :large-fern :tall-grass))]
    (when (and (air-at? chunks (dir/up p)) (support/supported? chunks gen/flat-chunk p tall))
      {:changes [[p tall] [(dir/up p) (block/state (block/block-of tall) {:half :upper})]]})))

(defn petals-meal [_chunks p st _roll]
  (let [n (block/prop-long st :flower-amount)]
    (if (< n 4)
      {:changes [[p (with st :flower-amount (inc n))]]}
      {:drops [{:item (block/block-of st) :count 1}]})))
