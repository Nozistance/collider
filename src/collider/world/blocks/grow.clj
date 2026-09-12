(ns collider.world.blocks.grow
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.blocks.chorus :as chorus]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.eyeblossom :as eyeblossom]
            [collider.world.gen :as gen]
            [collider.world.blocks.grass :as grass]
            [collider.world.light :as light]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.multiface :as multiface]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.support :as support]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(defn- air-at? [chunks [_ y _ :as p]] (and (chunk/in-range? y) (zero? (gen/at chunks p))))
(defn- lit? [chunks [x y z] ^long n] (>= (long (light/light-at chunks gen/flat-chunk x y z)) n))
(defn- chance? [roll salt ^long n] (< (double (roll salt)) (/ 1.0 n)))
(defn- pick ^long [roll salt ^long n] (long (Math/floor (* (double (roll salt)) n))))
(defn- water? [st] (and (pos? st) (or (= :water (liquid/liquid-class st)) (block/waterlogged? st))))
(defn- with [st & kvs]
  (block/state (block/block-of st) (apply assoc (block/props-of st) (map-indexed (fn [i v] (if (and (odd? i) (not (keyword? v))) (keyword (str v)) v)) kvs))))
(defn- age ^long [st] (block/prop-long st :age))
(defn- aged ^long [st ^long n] (with st :age n))
(def ^:private max-age {:crop 7 :carrot 7 :potato 7 :beetroot 3 :torchflower-crop 1 :stem 7})
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

(defn- crop-tick [chunks p st roll]
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

(defn- pitcher-tick [chunks p st roll]
  (when (and (= :lower (:half (block/props-of st))) (< (age st) 4) (growth-roll? chunks p st roll))
    (pitcher-grown chunks p st (inc (age st)))))

(defn- pitcher-meal [chunks p st]
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

(defn- stem-tick [chunks p st roll]
  (when (grows-now? chunks p st roll)
    (if (< (age st) 7)
      [[p (aged st (inc (age st)))]]
      (fruit-changes chunks p st roll))))

(defn- height-below ^long [chunks p self ^long cap]
  (loop [h 0]
    (if (and (< h cap) (= self (block/block-of (gen/at chunks (mapv + p [0 (- (inc h)) 0])))))
      (recur (inc h))
      h)))

(defn- cane-tick [chunks p st _rnd]
  (when (and (air-at? chunks (dir/up p)) (< (inc (height-below chunks p (block/block-of st) 3)) 3))
    (if (= 15 (age st))
      [[(dir/up p) (block/state (block/block-of st))] [p (aged st 0)]]
      [[p (aged st (inc (age st)))]])))

(defn- cactus-tick [chunks p st roll]
  (when (air-at? chunks (dir/up p))
    (let [a (age st) h (inc (height-below chunks p :cactus 3))]
      (when-not (and (>= h 3) (= a 15))
        (let [top (cond
                    (and (= a 8) (support/supported? chunks gen/flat-chunk (dir/up p) (block/state :cactus)))
                    (when (<= (double (roll :flower)) (if (>= h 3) 0.25 0.1)) [[(dir/up p) (block/state :cactus-flower)]])
                    (and (= a 15) (< h 3)) [[(dir/up p) (block/state :cactus)] [p (aged st 0)]])]
          (into (vec top) (when (< a 15) [[p (aged st (inc a))]])))))))

(defn- grown-bamboo [chunks p st roll height]
  (let [below (gen/at chunks (dir/down p)) two (gen/at chunks (dir/down (dir/down p)))
        bamboo? (fn [s] (= :bamboo (block/block-of s)))
        leaves (cond
                 (or (not (bamboo? below)) (= :none (:leaves (block/props-of below)))) :small
                 :else :large)
        shift (when (and (= leaves :large) (bamboo? two))
                [[(dir/down p) (with below :leaves :small)] [(dir/down (dir/down p)) (with two :leaves :none)]])
        a (if (and (not= 1 (age st)) (not (bamboo? two))) 0 1)
        height (long height)
        stage (if (or (and (>= height 11) (< (double (roll :stage)) 0.25)) (= height 15)) 1 0)]
    (conj (vec shift) [(dir/up p) (block/state :bamboo {:age (keyword (str a)) :leaves leaves :stage (keyword (str stage))})])))

(defn- bamboo-tick [chunks p st roll]
  (when (and (= 0 (block/prop-long st :stage)) (chance? roll :gate 3) (air-at? chunks (dir/up p)) (lit? chunks (dir/up p) 9))
    (let [height (inc (height-below chunks p :bamboo 16))]
      (when (< height 16)
        (grown-bamboo chunks p st roll height)))))

(defn- bamboo-sapling-tick [chunks p _st roll]
  (when (and (chance? roll :gate 3) (air-at? chunks (dir/up p)) (lit? chunks (dir/up p) 9))
    [[(dir/up p) (block/state :bamboo {:leaves :small})]]))

(defn- berry-tick [chunks p st roll]
  (when (and (< (age st) 3) (chance? roll :gate 5) (lit? chunks (dir/up p) 9))
    [[p (aged st (inc (age st)))]]))

(defn- kelp-tick [chunks p st roll]
  (when (and (< (age st) 25) (< (double (roll :grow)) 0.14)
             (= :water (liquid/liquid-class (gen/at chunks (dir/up p)))))
    [[(dir/up p) (aged st (inc (age st)))]]))

(defn- amethyst-next [^long target dir]
  (let [n (block/block-of target)]
    (cond
      (or (zero? target) (and (= :water (liquid/liquid-class target)) (liquid/source-state? target))) :small-amethyst-bud
      (not= dir (block/facing-of target)) nil
      (= :small-amethyst-bud n) :medium-amethyst-bud
      (= :medium-amethyst-bud n) :large-amethyst-bud
      (= :large-amethyst-bud n) :amethyst-cluster)))

(defn- budding-tick [chunks p _st roll]
  (when (chance? roll :gate 5)
    (let [dir (dir/six (pick roll :dir 6))
          q (mapv + p (dir/offset dir))
          target (gen/at chunks q)]
      (when-let [b (amethyst-next target dir)]
        [[q (block/state b {:facing dir :waterlogged (if (water? target) :true :false)})]]))))

(defn- grow-into ^long [^long st ^long a roll]
  (let [st' (aged st a)]
    (if (= :cave-vines (block/type-of st))
      (with st' :berries (if (< (double (roll :berries)) 0.11) :true :false))
      st')))

(defn- vines-tick [chunks p st roll]
  (let [q (mapv + p (dir/offset (:dir (block/growing-plant (block/type-of st)))))]
    (when (and (< (age st) 25) (< (double (roll :grow)) 0.1) (air-at? chunks q))
      [[q (grow-into st (inc (age st)) roll)]])))

(defn- crowd ^long [chunks [x y z] self]
  (count (for [dx (range -4 5) dy [-1 0 1] dz (range -4 5)
               :when (= self (block/block-of (gen/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])))]
           1)))

(defn- mushroom-tick [chunks p st roll]
  (when (and (chance? roll :gate 25) (< (crowd chunks p (block/block-of st)) 5))
    (let [step (fn [q i] (mapv + q [(dec (pick roll [:x i] 3)) (- (pick roll [:y1 i] 2) (pick roll [:y2 i] 2)) (dec (pick roll [:z i] 3))]))
          ok? (fn [q] (and (air-at? chunks q) (support/supported? chunks gen/flat-chunk q st)))
          target (loop [q p off (step p 0) i 1]
                   (if (> i 4)
                     off
                     (let [q (if (ok? off) off q)]
                       (recur q (step q i) (inc i)))))]
      (when (ok? target) [[target st]]))))

(def ^:private huge-mushroom
  {:red-mushroom   {:cap :red-mushroom-block :radius 2 :tag "huge_red_mushroom_can_place_on"}
   :brown-mushroom {:cap :brown-mushroom-block :radius 3 :tag "huge_brown_mushroom_can_place_on"}})
(def ^:private stem-state (block/state :mushroom-stem {:up :false :down :false}))
(defn- flag [b] (if b :true :false))

(defn- cleared-at ^long [chunks origin q]
  (if (= q origin) 0 (gen/at chunks q)))

(defn- check-radius ^long [kind ^long radius ^long dy]
  (if (= :brown-mushroom kind) (if (<= dy 3) 0 radius) 0))

(defn- mushroom-room? [chunks p kind radius height]
  (every? (fn [[dx dy dz]]
            (let [st (cleared-at chunks p (mapv + p [dx dy dz]))]
              (or (zero? st) (block/tagged? st "leaves"))))
          (for [dy (range (inc (long height)))
                :let [r (check-radius kind (long radius) dy)]
                dx (range (- r) (inc r)) dz (range (- r) (inc r))]
            [dx dy dz])))

(defn- mushroom-fits? [chunks [_ y _ :as p] kind radius tag height]
  (and (>= (long y) 1) (chunk/in-range? (+ (long y) (long height) 1))
       (block/tagged? (gen/at chunks (dir/down p)) tag)
       (mushroom-room? chunks p kind (long radius) (long height))))

(defn- mushroom-height ^long [roll]
  (let [h (+ 4 (pick roll :height 3))]
    (if (zero? (pick roll :double 12)) (* 2 h) h)))

(defn- red-cap [p cap ^long radius ^long height]
  (let [center (- radius 2)]
    (for [dy (range (- height 3) (inc height))
          :let [r (if (< dy height) radius (dec radius))]
          dx (range (- r) (inc r)) dz (range (- r) (inc r))
          :let [xe (or (= dx (- r)) (= dx r)) ze (or (= dz (- r)) (= dz r))]
          :when (or (>= dy height) (not= xe ze))]
      [(mapv + p [dx dy dz])
       (block/state cap {:down  :false :up (flag (>= dy (dec height)))
                         :west  (flag (< dx (- center))) :east (flag (> dx center))
                         :north (flag (< dz (- center))) :south (flag (> dz center))})])))

(defn- brown-cap [p cap ^long radius ^long height]
  (for [dx (range (- radius) (inc radius)) dz (range (- radius) (inc radius))
        :let [nx (= dx (- radius)) px (= dx radius) nz (= dz (- radius)) pz (= dz radius)
              xe (or nx px) ze (or nz pz)]
        :when (not (and xe ze))]
    [(mapv + p [dx height dz])
     (block/state cap {:up    :true :down :false
                       :west  (flag (or nx (and ze (= dx (- 1 radius)))))
                       :east  (flag (or px (and ze (= dx (dec radius)))))
                       :north (flag (or nz (and xe (= dz (- 1 radius)))))
                       :south (flag (or pz (and xe (= dz (dec radius)))))})]))

(defn- mushroom-cells [p kind cap radius height]
  (let [cap-fn (if (= :brown-mushroom kind) brown-cap red-cap)]
    (concat (cap-fn p cap (long radius) (long height))
            (for [dy (range (long height))] [(mapv + p [0 dy 0]) stem-state]))))

(defn- mushroom-changes [chunks origin cells]
  (loop [cells (seq cells) seen {origin 0} acc []]
    (if-let [[q st] (first cells)]
      (let [cur (long (get seen q (gen/at chunks q)))]
        (if (or (zero? cur) (block/tagged? cur "replaceable_by_mushrooms"))
          (recur (next cells) (assoc seen q st) (conj acc [q st]))
          (recur (next cells) seen acc)))
      acc)))

(defn- mushroom-grown [chunks p kind roll]
  (let [{:keys [cap radius tag]} (huge-mushroom kind) radius (long radius)
        height (mushroom-height roll)]
    (if (mushroom-fits? chunks p kind radius tag height)
      (mushroom-changes chunks p (mushroom-cells p kind cap radius height))
      [])))

(defn- mushroom-meal [chunks [_ y _ :as p] st roll]
  (let [kind (block/block-of st) {:keys [radius]} (huge-mushroom kind)]
    (when (and radius (chunk/in-range? (+ (long y) 4 (long radius))))
      {:changes (if (< (double (roll :success)) 0.4) (mushroom-grown chunks p kind roll) [])})))

(defn- roots-meal [chunks [_ y _ :as p]]
  (when (and (chunk/in-range? (dec (long y))) (zero? (gen/at chunks (dir/down p))))
    {:changes [[(dir/down p) (block/state :hanging-roots)]]}))

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

(defn- spread-tick [chunks p st roll time ctx]
  (if-not (grass/can-stay-alive? chunks st p)
    [[p (block/state :dirt)]]
    (when (>= (weather/brightness ctx chunks gen/flat-chunk (p 0) (inc (long (p 1))) (p 2) time) 9)
      (spread-cells chunks p st roll))))

(defn- near-water? [chunks [x y z]]
  (boolean (some (fn [[dx dy dz]] (water? (gen/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])))
                 (for [dx (range -4 5) dy [0 1] dz (range -4 5)] [dx dy dz]))))

(defn- farmland-tick [chunks p st _rnd]
  (let [m (block/prop-long st :moisture)]
    (cond
      (near-water? chunks p) (when (< m 7) [[p (with st :moisture 7)]])
      (pos? m) [[p (with st :moisture (dec m))]]
      (not (block/tagged? (gen/at chunks (dir/up p)) "maintains_farmland")) [[p (block/state :dirt)]])))

(defn- weather-odds [chunks [x y z] st]
  (let [own (block/weather-stage st)
        ages (for [dx (range -4 5) dy (range -4 5) dz (range -4 5)
                   :when (and (<= (+ (Math/abs (long dx)) (Math/abs (long dy)) (Math/abs (long dz))) 4)
                              (not (and (zero? (long dx)) (zero? (long dy)) (zero? (long dz)))))
                   :let [n (gen/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])]
                   :when (block/weathering? n)]
               (block/weather-stage n))]
    (when-not (some #(< (long %) own) ages)
      (let [older (count (filter #(> (long %) own) ages))
            same (- (count ages) older)
            chance (/ (double (inc older)) (double (+ older same 1)))]
        (* chance chance (if (zero? own) 0.75 1.0))))))

(defn- weathers-here? [st]
  (or (not= :weathering-copper-door (block/type-of st))
      (= :lower (:half (block/props-of st)))))

(defn- weather-tick [chunks p st roll]
  (when (and (weathers-here? st) (< (double (roll :day)) 0.05688889))
    (when-let [odds (weather-odds chunks p st)]
      (when (< (double (roll :age)) (double odds))
        (when-let [next (block/weathered-next st)]
          [[p next]])))))

(defn- vine-with [st dir] (with st dir :true))
(defn- vine-has? [st dir] (= :true (get (block/props-of st) dir)))
(defn- attachable? [chunks p dir]
  (let [n (gen/at chunks (mapv + p (dir/offset dir)))]
    (and (pos? n) (block/face-sturdy? n (dir/opposite dir)))))
(defn- vine-face-held? [chunks p st dir]
  (support/supported? chunks gen/flat-chunk p (vine-with (block/state (block/block-of st)) dir)))

(defn- vine-crowded? [chunks [x y z] self]
  (>= (count (for [dx (range -4 5) dy [-1 0 1] dz (range -4 5)
                   :when (= self (block/block-of (gen/at chunks [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)])))]
               1))
      5))

(defn- vine-sideways [chunks p st dir roll]
  (let [self (block/block-of st) fresh (block/state self)
        test (mapv + p (dir/offset dir))
        cw (dir/clockwise dir) ccw (dir/counter-clockwise dir)
        cw? (vine-has? st cw) ccw? (vine-has? st ccw)
        cw-test (mapv + test (dir/offset cw)) ccw-test (mapv + test (dir/offset ccw))
        opp (dir/opposite dir)]
    (cond
      (not (air-at? chunks test)) (when (attachable? chunks p dir) [[p (vine-with st dir)]])
      (and cw? (attachable? chunks test cw)) [[test (vine-with fresh cw)]]
      (and ccw? (attachable? chunks test ccw)) [[test (vine-with fresh ccw)]]
      (and cw? (air-at? chunks cw-test) (attachable? chunks (mapv + p (dir/offset cw)) opp)) [[cw-test (vine-with fresh opp)]]
      (and ccw? (air-at? chunks ccw-test) (attachable? chunks (mapv + p (dir/offset ccw)) opp)) [[ccw-test (vine-with fresh opp)]]
      (and (< (double (roll :up-wall)) 0.05) (attachable? chunks (dir/up test) :up)) [[test (vine-with fresh :up)]])))

(defn- vine-upward [chunks p st roll]
  (let [above (dir/up p)]
    (cond
      (vine-face-held? chunks p st :up) [[p (vine-with st :up)]]
      (air-at? chunks above)
      (when-not (vine-crowded? chunks p (block/block-of st))
        (let [st' (reduce (fn [s dir]
                            (if (or (< (double (roll [:keep dir])) 0.5) (not (attachable? chunks above dir)))
                              (with s dir :false)
                              s))
                          st [:north :south :west :east])]
          (when (some #(vine-has? st' %) [:north :south :west :east]) [[above st']])))
      :else nil)))

(defn- vine-downward [chunks p st roll]
  (let [below (dir/down p) bst (gen/at chunks below)]
    (when (or (zero? bst) (= (block/block-of bst) (block/block-of st)))
      (let [before (if (zero? bst) (block/state (block/block-of st)) bst)
            after (reduce (fn [s dir] (if (and (< (double (roll [:copy dir])) 0.5) (vine-has? st dir)) (vine-with s dir) s))
                          before [:north :south :west :east])]
        (when (and (not= after before) (some #(vine-has? after %) [:north :south :west :east]))
          [[below after]])))))

(defn- vine-tick [chunks p st roll]
  (when (chance? roll :gate 4)
    (let [dir (dir/six (pick roll :dir 6))]
      (cond
        (and (contains? dir/horizontal-offset dir) (not (vine-has? st dir)))
        (when-not (vine-crowded? chunks p (block/block-of st)) (vine-sideways chunks p st dir roll))
        (= :up dir) (or (when (chunk/in-range? (inc (long (p 1)))) (vine-upward chunks p st roll))
                        (vine-downward chunks p st roll))
        :else (vine-downward chunks p st roll)))))

(defn- eyeblossom-tick [p ^long st ^long time]
  (when-let [new (eyeblossom/switched st time)]
    [[p new]]))

(def ^:private potted-eyeblossom {:potted-open-eyeblossom   :potted-closed-eyeblossom
                                  :potted-closed-eyeblossom :potted-open-eyeblossom})

(defn- potted-tick [p ^long st ^long time]
  (let [self (block/block-of st)]
    (when (contains? potted-eyeblossom self)
      (let [open? (= :potted-open-eyeblossom self)
            night? (<= 12600 (mod time 24000) 23400)]
        (when (not= open? night?)
          [[p (block/state (potted-eyeblossom self))]])))))

(defn- nether-wart-tick [p st roll]
  (when (and (< (age st) 3) (chance? roll :gate 10))
    [[p (aged st (inc (age st)))]]))

(defn- propagule-tick [p st]
  (when (and (= :true (:hanging (block/props-of st))) (< (age st) 4))
    [[p (aged st (inc (age st)))]]))

(defn- leaves-tick [p st]
  (when (and (= :false (:persistent (block/props-of st))) (= 7 (block/prop-long st :distance)))
    [[p (block/emptied st)]]))

(defn- chorus-tick [chunks p st roll]
  (chorus/flower-tick chunks p st (fn [salt ^long n] (pick roll salt n))))

(defn random-tick
  ([chunks p st roll time] (random-tick chunks p st roll time nil))
  ([chunks p st roll time ctx]
   (let [st (long st)]
     (case (block/type-of st)
       (:crop :carrot :potato :beetroot :torchflower-crop) (crop-tick chunks p st roll)
       :stem (stem-tick chunks p st roll)
       :pitcher-crop (pitcher-tick chunks p st roll)
       :sugar-cane (cane-tick chunks p st roll)
       :cactus (cactus-tick chunks p st roll)
       :bamboo-stalk (bamboo-tick chunks p st roll)
       :bamboo-sapling (bamboo-sapling-tick chunks p st roll)
       :sweet-berry-bush (berry-tick chunks p st roll)
       :kelp (kelp-tick chunks p st roll)
       :mushroom (mushroom-tick chunks p st roll)
       (:grass :mycelium) (spread-tick chunks p st roll (long time) ctx)
       :farmland (farmland-tick chunks p st roll)
       :cocoa (when (and (chance? roll :gate 5) (< (age st) 2)) [[p (aged st (inc (age st)))]])
       :ice (when (> (long (light/block-light-at chunks gen/flat-chunk (p 0) (p 1) (p 2))) (- 11 (block/dampening st))) [[p (block/state :water)]])
       :snow-layer (when (> (long (light/block-light-at chunks gen/flat-chunk (p 0) (p 1) (p 2))) 11) [[p 0]])
       :vine (vine-tick chunks p st roll)
       :budding-amethyst (budding-tick chunks p st roll)
       :eyeblossom (eyeblossom-tick p st time)
       :flower-pot (potted-tick p st time)
       :nether-wart (nether-wart-tick p st roll)
       :mangrove-propagule (propagule-tick p st)
       :chorus-flower (chorus-tick chunks p st roll)
       (:mangrove-leaves :tinted-particle-leaves :untinted-particle-leaves) (leaves-tick p st)
       (:weeping-vines :twisting-vines :cave-vines) (vines-tick chunks p st roll)
       (when (block/weathering? st) (weather-tick chunks p st roll))))))

(defn random-drops [^long st roll]
  (when (and (block/leaves? st)
             (= :false (:persistent (block/props-of st)))
             (= 7 (block/prop-long st :distance)))
    (block/drops st roll)))

(defn- crop-meal [chunks p st roll]
  (let [t (block/type-of st) a (age st) top (long (max-age t))]
    (when (< a top)
      (let [n (case t :beetroot (quot (+ 2 (pick roll :meal 4)) 3) :torchflower-crop 1 (+ 2 (pick roll :meal 4)))
            a' (min top (+ a (long n)))]
        {:changes (if (and (= :stem t) (= a' 7) (grows-now? chunks p st roll))
                    (into [[p (aged st a')]] (fruit-changes chunks p (aged st a') roll))
                    [[p (aged st a')]])}))))

(defn- doubled [chunks p st]
  (let [tall (block/state (if (= :fern (block/block-of st)) :large-fern :tall-grass))]
    (when (and (air-at? chunks (dir/up p)) (support/supported? chunks gen/flat-chunk p tall))
      {:changes [[p tall] [(dir/up p) (block/state (block/block-of tall) {:half :upper})]]})))

(defn- petals-meal [p st]
  (let [n (block/prop-long st :flower-amount)]
    (if (< n 4)
      {:changes [[p (with st :flower-amount (inc n))]]}
      {:drops [{:item (block/block-of st) :count 1}]})))

(defn- nether-vines-count ^long [roll]
  (loop [p 1.0 n 0]
    (if (and (< n 25) (< (double (roll [:count n])) p))
      (recur (* p 0.826) (inc n))
      n)))

(defn- head-pos [chunks p st]
  (let [{:keys [head body dir]} (block/growing-plant (block/type-of st))
        off (dir/offset dir)]
    (loop [q p n 0]
      (let [nq (mapv + q off) b (block/block-of (gen/at chunks nq))]
        (cond
          (= head b) nq
          (and (= body b) (< n 256)) (recur nq (inc n))
          :else nil)))))

(defn- vines-meal [chunks p st roll]
  (let [off (dir/offset (:dir (block/growing-plant (block/type-of st))))
        n (if (= :cave-vines (block/type-of st)) 1 (nether-vines-count roll))]
    (loop [q (mapv + p off) a (min 25 (inc (age st))) left n acc []]
      (if (and (pos? left) (air-at? chunks q))
        (recur (mapv + q off) (min 25 (inc a)) (dec left) (conj acc [q (aged st a)]))
        (when (seq acc) {:changes acc})))))

(defn- berries-meal [p st]
  (when (= :false (:berries (block/props-of st)))
    {:changes [[p (with st :berries :true)]]}))

(defn- carpet-meal [chunks p st]
  (when (= :true (:bottom (block/props-of st)))
    (when-let [topper (moss/carpet-topper chunks p (constantly true))]
      {:changes [[(dir/up p) topper]]})))

(defn- hanging-moss-meal [chunks p st]
  (let [q (moss/hanging-end chunks p (block/block-of st))]
    (when (air-at? chunks q)
      {:changes [[q (with st :tip :true)]]})))

(defn- lichen-meal [chunks p st roll]
  (when-let [changes (multiface/spread-random chunks p st roll)]
    {:changes changes}))

(defn- shuffled-dirs [roll]
  (loop [pool (vec dir/horizontal) i 0 acc []]
    (if (= i 3)
      (into acc pool)
      (let [j (pick roll [:shuffle i] (- 4 i))]
        (recur (into (subvec pool 0 j) (subvec pool (inc j))) (inc i) (conj acc (pool j)))))))

(defn- spread-meal [chunks p roll target]
  (when-let [q (first (for [d (shuffled-dirs roll)
                            :let [q (mapv + p (dir/horizontal-offset d))]
                            :when (and (air-at? chunks q)
                                       (support/supported? chunks gen/flat-chunk q target))]
                        q))]
    {:changes [[q target]]}))

(defn- height-above ^long [chunks p self ^long cap]
  (loop [h 0]
    (if (and (< h cap) (= self (block/block-of (gen/at chunks (mapv + p [0 (inc h) 0])))))
      (recur (inc h))
      h)))

(defn- bamboo-meal [chunks p roll]
  (let [above (height-above chunks p :bamboo 16)
        below (height-below chunks p :bamboo 16)
        top (mapv + p [0 above 0])
        top-st (gen/at chunks top)
        target (dir/up top)]
    (when (and (< (+ above below 1) 16)
               (not= 1 (block/prop-long top-st :stage))
               (chunk/in-range? (long (target 1)))
               (air-at? chunks target))
      {:changes (grown-bamboo chunks top top-st roll (+ above below 1))})))

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

(defn- pickle-meal [chunks p st roll]
  (when (and (= :true (:waterlogged (block/props-of st)))
             (block/tagged? (gen/at chunks (dir/down p)) "coral_blocks"))
    {:changes (conj (pickle-spots chunks p roll) [p (with st :pickles 4)])}))

(defn bonemeal [chunks p st roll]
  (let [st (long st)]
    (case (block/type-of st)
      (:crop :carrot :potato :beetroot :torchflower-crop :stem) (crop-meal chunks p st roll)
      :pitcher-crop (pitcher-meal chunks p st)
      :tall-grass (doubled chunks p st)
      :tall-flower (when (= :lower (:half (block/props-of st))) {:drops [{:item (block/block-of st) :count 1}]})
      :flower-bed (petals-meal p st)
      :sweet-berry-bush (when (< (age st) 3) {:changes [[p (aged st (inc (age st)))]]})
      :mushroom (mushroom-meal chunks p st roll)
      :rooted-dirt (roots-meal chunks p)
      :cocoa (when (< (age st) 2) {:changes [[p (aged st (inc (age st)))]]})
      :bamboo-sapling (when (air-at? chunks (dir/up p)) {:changes [[(dir/up p) (block/state :bamboo {:leaves :small})]]})
      :kelp (when (and (< (age st) 25) (= :water (liquid/liquid-class (gen/at chunks (dir/up p)))))
              {:changes [[(dir/up p) (aged st (inc (age st)))]]})
      (:weeping-vines :twisting-vines) (vines-meal chunks p st roll)
      (:weeping-vines-plant :twisting-vines-plant)
      (when-let [h (head-pos chunks p st)] (vines-meal chunks h (gen/at chunks h) roll))
      (:cave-vines :cave-vines-plant) (berries-meal p st)
      :glow-lichen (lichen-meal chunks p st roll)
      :hanging-moss (hanging-moss-meal chunks p st)
      :mossy-carpet (carpet-meal chunks p st)
      (:big-dripleaf :big-dripleaf-stem :small-dripleaf) (dripleaf/meal chunks p st roll)
      (:bush :firefly-bush) (spread-meal chunks p roll (block/state (block/block-of st)))
      :short-dry-grass {:changes [[p (block/state :tall-dry-grass)]]}
      :tall-dry-grass (spread-meal chunks p roll (block/state :short-dry-grass))
      :bamboo-stalk (bamboo-meal chunks p roll)
      :sea-pickle (pickle-meal chunks p st roll)
      :mangrove-propagule (when (and (= :true (:hanging (block/props-of st))) (< (age st) 4))
                            {:changes [[p (aged st (inc (age st)))]]})
      :seagrass (when (water? (gen/at chunks (dir/up p)))
                  {:changes [[p (block/state :tall-seagrass {:half :lower})] [(dir/up p) (block/state :tall-seagrass {:half :upper})]]})
      nil)))

(def tilled
  {:grass-block [:farmland] :dirt-path [:farmland] :dirt [:farmland]
   :coarse-dirt [:dirt] :rooted-dirt [:dirt :hanging-roots]})

(def flattened
  #{:grass-block :dirt :podzol :coarse-dirt :mycelium :rooted-dirt})
