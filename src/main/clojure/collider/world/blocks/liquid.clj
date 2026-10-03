(ns collider.world.blocks.liquid
  "Water and lava with their spread, mixing and push on entities."
  (:require [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.attribute :as attribute]
            [collider.world.env.dimension :as dimension])
  (:import (collider.world.blocks Flow Flow$Tables)))

(set! *warn-on-reflection* true)

(def ^:private liquids
  {:water {:block :water :bucket :water-bucket
           :dropoff 1 :slope 4 :delay 5 :infinite? true
           :push 0.014}
   :lava  {:block :lava :bucket :lava-bucket
           :dropoff 2 :slope 2 :delay 30 :infinite? false
           :push 0.0023333333333333335
           :decay-jitter 4
           :mix {:source :obsidian :flowing :cobblestone
                 :smother :stone}}})

(def ^:private fast-lava
  {:dropoff 1 :slope 4 :delay 10 :push 0.007})

(defn- dimension-liquids [dim]
  (cond-> liquids
    (attribute/fast-lava? dim) (update :lava merge fast-lava)))

(def ^:private ^:table by-dim
  (delay (into {}
               (map (fn [dim] [dim (dimension-liquids dim)]))
               (keys (dimension/types)))))

(defn liquids-in
  "Returns the flow of water and lava in dimension dim.
  An unknown dimension flows like the overworld."
  [dim]
  (get @by-dim dim liquids))

(defn- liquid-table [f]
  (delay (into {} (map (fn [[cls m]] (f cls m))) liquids)))

(def ^:private ^:table base
  (liquid-table (fn [cls m] [cls (block/state (:block m))])))

(def ^:private ^:table bucket->class
  (liquid-table (fn [cls m] [(:bucket m) cls])))

(def ^:private flow-faces [:east :west :south :north :up :down])

(def ^:private sides (subvec flow-faces 0 4))

(def ^:private ^:table water-source (delay (block/state :water)))

(defn- liquid-class [st] (block/liquid-class st))

(defn- level ^long [st] (block/liquid-level st))

(defn liquid-state
  "Returns the state of liquid cls at level n."
  ^long [cls ^long n]
  (+ (long (@base cls)) n))

(defn fluid-of
  "Returns the fluid of state st, or nil when it holds none."
  [st]
  (when-let [cls (liquid-class st)]
    (cond
      (zero? (level st)) cls
      (= :lava cls) :flowing-lava
      :else :flowing-water)))

(defn bucket->state
  "Returns the liquid state that bucket item places, or nil."
  [item]
  (when-let [cls (@bucket->class item)] (liquid-state cls 0)))

(def ^:private ^:table void-air (delay (block/state :void-air)))

(defn- raw-at ^long [chunks ^long x ^long y ^long z]
  (if (chunk/in-range? y)
    (long (chunk/chunks-get-block chunks x y z))
    (long @void-air)))

(defn- state-of ^long [^long st]
  (if (and (pos? st) (block/waterlogged? st))
    (long @water-source)
    st))

(defn- state-at ^long [chunks ^long x ^long y ^long z]
  (state-of (raw-at chunks x y z)))

(defn- state-toward ^long [chunks p face]
  (let [[x y z] (dir/toward p face)]
    (state-at chunks x y z)))

(defn- other-class? [cls st]
  (let [c (liquid-class st)]
    (and (some? c) (not= c cls))))

(defn- blocks-movement? [st]
  (and (pos? (long st)) (block/blocks-motion? (long st))))

(defn- amount ^long [st]
  (let [l (level st)] (if (or (zero? l) (>= l 8)) 8 (- 8 l))))

(defn- height ^double [st] (/ (double (amount st)) 9.0))

(defn- own-height ^double [st] (num/f32 (height st)))

(defn- amount-height ^double [^long n] (num/f32 (/ (double n) 9.0)))

(defn- amount-at ^long [chunks cls p]
  (let [st (state-at chunks (p 0) (p 1) (p 2))]
    (if (and (pos? (long st)) (= cls (liquid-class st)))
      (amount st)
      -1)))

(defn- unit [v] (v/normalized v 1.0E-4))

(def ^:private ^:const fall-drop (double (float 0.8888889)))

(defn- below-pull ^double [chunks cls n [nx y nz]]
  (let [j (amount-at chunks cls [nx (dec (long y)) nz])]
    (if (pos? j)
      (num/fsub (amount-height n)
                (num/fsub (amount-height j) fall-drop))
      0.0)))

(defn- neighbor-pull [chunks cls n p face]
  (let [[nx y nz :as q] (dir/toward p face)
        ns (state-at chunks nx y nz)
        j (amount-at chunks cls q)]
    (cond
      (other-class? cls ns) 0.0
      (pos? j) (num/fsub (amount-height n) (amount-height j))
      (not (blocks-movement? ns)) (below-pull chunks cls n q)
      :else 0.0)))

(def ^:private ice-types #{:ice :frosted-ice})

(defn- solid-face? [cls st face]
  (let [st (long st)]
    (and (pos? st)
         (not= cls (liquid-class st))
         (not (contains? ice-types (block/type-of st)))
         (block/face-sturdy? st face))))

(defn- walled-at? [chunks cls p face]
  (let [[x y z] (dir/toward p face)]
    (or (solid-face? cls (raw-at chunks x y z) face)
        (solid-face? cls (raw-at chunks x (inc y) z) face))))

(defn- walled? [chunks cls p]
  (some #(walled-at? chunks cls p %) sides))

(defn- pull-sum [chunks cls n p]
  (reduce (fn [[vx vz] face]
            (let [[dx _ dz] (dir/offset face)
                  k (neighbor-pull chunks cls n p face)]
              [(+ (double vx) (* (long dx) k))
               (+ (double vz) (* (long dz) k))]))
          [0.0 0.0]
          sides))

(defn flow-vector
  "Returns the unit vector the liquid at p flows along, or nil."
  [chunks [x y z :as p]]
  (let [st (state-at chunks x y z)]
    (when-let [cls (when (pos? (long st)) (liquid-class st))]
      (let [[vx vz] (pull-sum chunks cls (amount-at chunks cls p) p)]
        (if (and (>= (level st) 8) (walled? chunks cls p))
          (let [[nx _ nz] (unit [vx 0.0 vz])]
            (unit [nx -6.0 nz]))
          (unit [vx 0.0 vz]))))))

(defn- height-in ^double [chunks cls [x y z]]
  (let [above (state-at chunks x (inc (long y)) z)]
    (if (and (pos? (long above)) (= cls (liquid-class above)))
      1.0
      (own-height (state-at chunks x y z)))))

(defn surface
  "Returns the height of the top of liquid cls in cell c, or nil."
  [chunks cls [x y z :as c]]
  (let [st (state-at chunks x y z)]
    (when (and (pos? st) (= cls (liquid-class st)))
      (let [h (float (height-in chunks cls c))]
        (double (float (+ (double (float y)) (double h))))))))

(def ^:private ^:const fluid-margin 0.001)

(defn- ceil ^long [^double v] (long (Math/ceil v)))

(defn- scaled-add [[ax ay az] [fx fy fz] k]
  (let [k (double k)]
    [(+ (double ax) (* (double fx) k))
     (+ (double ay) (* (double fy) k))
     (+ (double az) (* (double fz) k))]))

(defn- with-cell [acc cls h c chunks]
  (let [h (max (double h) (double (get-in acc [cls :height] 0.0)))
        flow (or (flow-vector chunks c) [0.0 0.0 0.0])
        k (if (< h 0.4) h 1.0)
        sum (get-in acc [cls :flow] [0.0 0.0 0.0])]
    (assoc acc cls {:height h
                    :flow   (scaled-add sum flow k)
                    :n      (inc (long (get-in acc [cls :n] 0)))})))

(defn- liquid-at [chunks ^long x ^long y ^long z]
  (let [st (state-at chunks x y z)]
    (when (pos? st) (liquid-class st))))

(defn- wet-cell [chunks y acc cls [_ cy :as c]]
  (let [h (- (+ (double cy) (height-in chunks cls c)) (double y))]
    (if (< h fluid-margin) acc (with-cell acc cls h c chunks))))

(defn- scan-cells [chunks y x0 x1 y0 y1 z0 z1]
  (let [x1 (long x1) y0 (long y0) y1 (long y1)
        z0 (long z0) z1 (long z1)]
    (loop [cx (long x0) cy y0 cz z0 acc {}]
      (cond
        (>= cx x1) acc
        (>= cy y1) (recur (inc cx) y0 z0 acc)
        (>= cz z1) (recur cx (inc cy) z0 acc)
        :else (recur cx cy (inc cz)
                     (if-let [cls (liquid-at chunks cx cy cz)]
                       (wet-cell chunks y acc cls [cx cy cz])
                       acc))))))

(defn- fluid-around [chunks [x y z] half height]
  (let [x (double x) yd (double y) z (double z)
        a (- (double half) fluid-margin)
        y0 (+ yd fluid-margin)
        y1 (- (+ yd (double height)) fluid-margin)]
    (scan-cells chunks y
                (num/floor (- x a)) (ceil (+ x a))
                (num/floor y0) (ceil y1)
                (num/floor (- z a)) (ceil (+ z a)))))

(defn fluid-height
  "Returns how deep in fluid cls a body of that size at pos stands."
  [chunks pos half height cls]
  (let [around (fluid-around chunks pos half height)]
    (double (get-in around [cls :height] 0.0))))

(def ^:private ^:const min-current 0.0045000000000000005)

(defn- still? [vel]
  (and (< (Math/abs (v/x vel)) 0.003)
       (< (Math/abs (v/z vel)) 0.003)))

(defn- unit-times [[x y z] ^double len ^double k]
  [(* (/ (double x) len) k)
   (* (/ (double y) len) k)
   (* (/ (double z) len) k)])

(defn- current [flow ^double len2 ^double p vel]
  (let [push (unit-times flow (Math/sqrt len2) p)
        ilen (v/length push)]
    (if (and (still? vel) (< ilen min-current))
      (unit-times push ilen min-current)
      push)))

(defn- current-push [table around vel]
  (reduce (fn [acc [cls {:keys [flow n]}]]
            (let [len2 (v/dot flow flow)
                  p (double (get-in table [cls :push] 0.0))]
              (if (or (zero? (long n)) (< len2 1.0E-5) (zero? p))
                acc
                (scaled-add acc (current flow len2 p vel) 1.0))))
          [0.0 0.0 0.0]
          around))

(defn entity-push
  "Returns the push of the flowing liquids on a body.
  The lava of a fast-lava dimension dim pushes harder."
  ([chunks pos half height vel]
   (entity-push chunks pos half height vel nil))
  ([chunks pos half height vel dim]
   (current-push (liquids-in dim)
                 (fluid-around chunks pos half height) vel)))

(defn fluid-info
  "Returns the water and lava over a body and the push of their flow."
  ([chunks pos half height vel]
   (fluid-info chunks pos half height vel nil))
  ([chunks pos half height vel dim]
   (let [around (fluid-around chunks pos half height)]
     {:water (double (get-in around [:water :height] 0.0))
      :lava  (double (get-in around [:lava :height] 0.0))
      :push  (current-push (liquids-in dim) around vel)})))

(def ^:private no-fluid-types
  #{:door :standing-sign :wall-sign :ladder :sugar-cane
    :bubble-column})

(defn- falling? [st] (= 8 (level st)))

(defn- same? [cls st] (= cls (liquid-class st)))

(defn- source-of? [cls st] (and (same? cls st) (zero? (level st))))

(defn- above-at [chunks [x y z]]
  (let [up (inc (long y))]
    (if (chunk/in-range? up)
      (chunk/chunks-get-block chunks [x up z])
      0)))

(defn fluid-height-of
  "Returns the height of the fluid of st at p, or nil.
  Mode :source-only counts sources alone."
  [chunks p st mode]
  (let [cls (liquid-class st)]
    (when (and cls (or (not= mode :source-only) (source-of? cls st)))
      (if (same? cls (above-at chunks p)) 1.0 (height st)))))

(defn- boxes [st]
  (if (pos? (long st)) (block/collision-boxes (long st)) []))

(def ^:private holder-types
  (disj block/water-holder-types :bubble-column))

(def ^:private refusing-types (conj holder-types :barrier))

(def ^:private slab-types #{:slab :weathering-copper-slab})

(def ^:private flowing {:water :flowing-water :lava :flowing-lava})

(defn- container? [st]
  (let [st (long st)]
    (and (pos? st)
         (or (block/has-waterlogged-prop? st)
             (contains? holder-types (block/type-of st))))))

(defn- places? [fluid st]
  (let [st (long st)
        t (block/type-of st)]
    (and (= :water fluid)
         (not (contains? refusing-types t))
         (not (and (contains? slab-types t)
                   (block/double-slab? st))))))

(defn- holds-any-fluid? [st]
  (let [st (long st)]
    (cond
      (zero? st) true
      (container? st) true
      (blocks-movement? st) false
      :else (not (contains? no-fluid-types (block/type-of st))))))

(defn- holds-specific? [fluid st]
  (if (container? st) (places? fluid st) true))

(defn- can-hold? [fluid st]
  (and (holds-any-fluid? st) (holds-specific? fluid st)))

(defn- raw-over ^long [chunks over [x y z :as p]]
  (if-let [st (get over p)]
    (long st)
    (long (raw-at chunks x y z))))

(defn- raw-cell ^long [{:keys [chunks over]} p]
  (raw-over chunks over p))

(defn- state-table ^booleans [f] (block/state-table :boolean f))

(defn- byte-table ^bytes [f] (block/state-table :byte f))

(def ^:private fluid-codes {:water Flow/WATER :lava Flow/LAVA})

(defn- fluid-code ^long [st]
  (let [st (state-of st)]
    (long (if (pos? st) (fluid-codes (liquid-class st) 0) 0))))

(defn- fluid-level ^long [st]
  (let [st (state-of st)]
    (if (and (pos? st) (liquid-class st)) (level st) 0)))

(defn- wall-kind ^long [^long st]
  (cond
    (block/full-cube? st) Flow/FULL
    (empty? (boxes st)) Flow/OPEN
    :else Flow/PARTIAL))

(defn- box-table [^bytes kinds]
  (into-array (map #(double-array (apply concat (boxes %)))
                   (range (alength kinds)))))

(defn- enterable? [cls st]
  (and (not (source-of? cls (state-of st)))
       (holds-any-fluid? st)
       (holds-specific? (flowing cls) st)))

(defn- hole-floor? [cls st]
  (or (same? cls (state-of st)) (can-hold? (flowing cls) st)))

(defn- ground? [cls st]
  (or (block/solid? (long st)) (source-of? cls (state-of st))))

(defn- destroying [mix traw]
  (let [traw (long traw)]
    (when (and (pos? traw)
               (not (block/air-type? traw)))
      (if mix :fizz [:drop traw]))))

(defn- tables-of [cls {:keys [codes levels kinds boxes]}]
  (Flow$Tables. (int (fluid-codes cls)) codes levels kinds boxes
                (state-table #(enterable? cls %))
                (state-table #(hole-floor? cls %))
                (state-table holds-any-fluid?)
                (state-table #(holds-specific? cls %))
                (state-table #(holds-specific? (flowing cls) %))
                (state-table #(ground? cls %))
                (state-table container?)
                (state-table #(some? (destroying nil %)))
                (int (@base cls)) (int @void-air)))

(def ^:private ^:table flow-tables
  (delay (let [kinds (byte-table wall-kind)
               shared {:codes  (byte-table fluid-code)
                       :levels (byte-table fluid-level)
                       :kinds  kinds
                       :boxes  (box-table kinds)}]
           (into {}
                 (map #(vector % (tables-of % shared)))
                 (keys liquids)))))

(defn- hole? [{:keys [cls chunks over]} [x y z]]
  (Flow/hole (@flow-tables cls) chunks over (int x) (int y) (int z)))

(defn- lava-near? [{:keys [chunks over]} [x y z]]
  (Flow/lavaNear (@flow-tables :water) chunks over
                 (int x) (int y) (int z)))

(defn- level->liquid [^long l]
  (case l -1 nil 0 :source 8 :falling (- 8 l)))

(defn- new-liquid
  [{:keys [cls chunks over dropoff infinite?]} [x y z]]
  (level->liquid
    (Flow/newLiquid (@flow-tables cls) chunks over (int x) (int y)
                    (int z) (int dropoff) (boolean infinite?))))

(defn- liquid->state ^long [cls v]
  (case v
    :source (liquid-state cls 0)
    :falling (liquid-state cls 8)
    (liquid-state cls (- 8 (long v)))))

(defn- mix-product [mix m]
  (when mix
    (let [k (if (zero? (long m)) :source :flowing)]
      (block/state (k mix)))))

(def ^:private ^:table basalt-state (delay (block/state :basalt)))

(def ^:private ^:table soul-soil-state
  (delay (block/state :soul-soil)))

(def ^:private ^:table blue-ice-state
  (delay (block/state :blue-ice)))

(def ^:private mix-order [:up :north :south :west :east])

(defn- mixed-by [chunks over p st soul? face]
  (let [n (raw-over chunks over (dir/toward p face))]
    (cond
      (block/water? n)
      (mix-product (get-in liquids [:lava :mix]) (level st))
      (and soul? (== n (long @blue-ice-state))) @basalt-state)))

(defn mixed
  "Returns the block that the lava at p turns into, or nil.
  States in over stand before those of the chunks."
  ([chunks p] (mixed chunks nil p))
  ([chunks over [x y z :as p]]
   (let [st (raw-over chunks over p)]
     (when (block/lava? st)
       (let [below (raw-over chunks over [x (dec (long y)) z])
             soul? (== below (long @soul-soil-state))]
         (some #(mixed-by chunks over p st soul? %) mix-order))))))

(def ^:private update-order [:west :east :down :up :north :south])

(defn- conversion [chunks over p face]
  (let [np (dir/toward p face)]
    (when-let [prod (mixed chunks over np)] [np prod])))

(defn- written [changes] {:changes changes :over changes})

(defn- with-neighbors
  [{:keys [chunks over] :as env} [tp st :as change]]
  (if (lava-near? env tp)
    (let [over (assoc over tp st)]
      {:changes [change]
       :over    (into [change]
                      (keep #(conversion chunks over tp %))
                      update-order)})
    (written [change])))

(def ^:private extinguished
  [:sound :entity.generic.extinguish-fire 1.0 1.0])

(def ^:private soaked-ghast
  [:sound :block.dried-ghast.place-in-water 1.0 1.0])

(defn- doused [tp st]
  (if (= :true (:lit (block/props-of st)))
    [tp (block/with st :lit :false) [extinguished]]
    [tp st]))

(defn- held-liquid [tp traw]
  (let [traw (long traw)
        st (block/with traw :waterlogged :true)
        [p st' fx] (case (block/type-of traw)
                     :campfire (doused tp st)
                     :dried-ghast [tp st [soaked-ghast]]
                     [tp st])]
    [p st' (conj (vec fx) :fluid-tick)]))

(defn- spread-plain [{:keys [chunks over cls mix] :as env} tp v traw]
  (let [st (liquid->state cls v)
        prod (when mix (mixed chunks (assoc over tp st) tp))
        gone (destroying mix traw)
        fx (cond-> []
             gone (conj gone)
             prod (conj :fizz))]
    (with-neighbors env
                    (cond-> [tp (or prod st)] (seq fx) (conj fx)))))

(defn- smothered [tp mix]
  (written [[tp (block/state (:smother mix)) [:fizz]]]))

(defn- spread-to [{:keys [mix] :as env} tp face v]
  (let [traw (raw-cell env tp)]
    (cond
      (not (chunk/in-range? (long (tp 1)))) (written [])
      (and mix (= face :down) (block/water? traw)) (smothered tp mix)
      (container? traw) (with-neighbors env (held-liquid tp traw))
      :else (spread-plain env tp v traw))))

(defn- target [p ^ints found ^long k]
  (let [face (flow-faces (Flow/targetDirection found k))
        v (level->liquid (Flow/targetLevel found k))]
    [(dir/toward p face) face v]))

(defn- targets [p ^ints found]
  (mapv #(target p found %) (range (Flow/count found))))

(defn- lowest-targets
  [{:keys [cls chunks over dropoff slope infinite?]} [x y z :as p]]
  (targets p (Flow/lowestTargets
               (@flow-tables cls) chunks over (int x) (int y) (int z)
               (int dropoff) (int slope) (boolean infinite?))))

(defn- over-with [env changes]
  (update env :over (fnil into {})
          (map (fn [[q st]] [q st])) changes))

(defn- spread-one [[acc env] [tp face v]]
  (let [r (spread-to env tp face v)]
    [(into acc (:changes r)) (over-with env (:over r))]))

(defn- spread-each [env targets]
  (first (reduce spread-one [[] env] targets)))

(defn- spread-sides [{:keys [dropoff] :as env} p st]
  (let [n (if (falling? st) 7 (- (amount st) (long dropoff)))]
    (when (pos? n)
      (spread-each env (lowest-targets env p)))))

(defn- source-neighbours ^long [{:keys [cls] :as env} p]
  (let [source? #(source-of? cls (state-of (raw-cell env %)))]
    (count (filter #(source? (dir/toward p %)) sides))))

(defn- down-level
  [{:keys [cls chunks over dropoff infinite?]} [x y z]]
  (level->liquid
    (Flow/downLevel (@flow-tables cls) chunks over (int x) (int y)
                    (int z) (int dropoff) (boolean infinite?))))

(defn- spread-down [env p st]
  (when-let [v (down-level env p)]
    (let [r (spread-to env (dir/down p) :down v)
          env (over-with env (:over r))]
      (into (:changes r)
            (when (>= (source-neighbours env p) 3)
              (spread-sides env p st))))))

(defn- spread [{:keys [cls] :as env} p st]
  (or (spread-down env p st)
      (when (or (source-of? cls st) (not (hole? env p)))
        (spread-sides env p st))))

(defn- rising? [cls old new]
  (and (same? cls old)
       (not (falling? old)) (not (falling? new))
       (> (height new) (height old))))

(defn update-delay
  "Returns the ticks until a liquid that went from old to new moves.
  Lava that rises waits longer, at random."
  ([old new tick pos] (update-delay nil old new tick pos))
  ([dim old new tick pos]
   (let [cls (liquid-class new)
         {:keys [delay decay-jitter]} ((liquids-in dim) cls)]
     (if (and decay-jitter
              (rising? cls old new)
              (not= 0 (mod (hash [tick pos]) 4)))
       (* (long delay) (long decay-jitter))
       (long delay)))))

(def ^:private column-drag {:soul-sand :false :magma :true})

(defn bubble-column?
  "Returns true when st is a bubble column."
  [st]
  (= :bubble-column (block/type-of (long st))))

(defn- source-water? [st] (= (long st) @water-source))

(defn- makes-column? [below]
  (contains? column-drag (block/type-of (long below))))

(defn column-wake
  "Returns the tick at which a water source st over below forms a
  bubble column, or nil."
  [st below tick]
  (when (and (source-water? st) (makes-column? below))
    (+ (long tick) 20)))

(defn- can-occupy? [st]
  (or (bubble-column? st) (source-water? st)))

(defn- column-state ^long [below occupy]
  (cond
    (bubble-column? below) (long below)
    (makes-column? below)
    (block/state :bubble-column
                 {:drag (column-drag (block/type-of (long below)))})
    (bubble-column? occupy) @water-source
    :else (long occupy)))

(defn- column-up [chunks [x y z] col]
  (loop [y (inc (long y)) acc []]
    (let [st (long (raw-at chunks x y z))]
      (if (and (can-occupy? st) (not= st col))
        (recur (inc y) (conj acc [[x y z] col nil 2]))
        acc))))

(defn column-changes
  "Returns the changes that the block below makes to the bubble
  column from p upward."
  [chunks [x y z :as p]]
  (let [occupy (long (raw-at chunks x y z))
        below (raw-at chunks x (dec (long y)) z)]
    (when (can-occupy? occupy)
      (let [col (column-state below occupy)]
        (into [[p col nil 2]] (column-up chunks p col))))))

(defn- column-due [chunks p _ctx]
  (column-changes chunks p))

(defn- column-survives? [below]
  (or (bubble-column? below) (makes-column? below)))

(defn- column-shaped?
  [chunks [x y z :as p] side]
  (let [below (raw-at chunks x (dec (long y)) z)
        [dx dy dz] (when side (dir/offset side))
        nst (when side
              (raw-at chunks (+ (long x) (long dx))
                      (+ (long y) (long dy)) (+ (long z) (long dz))))]
    (or (not (column-survives? below))
        (= :down side)
        (and (= :up side) (not (bubble-column? nst))
             (can-occupy? nst)))))

(defn- column-rewake [chunks _dim tick p _old side]
  (when (and side (column-shaped? chunks p side))
    (+ (long tick) 5)))

(def column-rule
  "The rule of bubble columns. A column follows the block below it."
  {:name   :bubble-column
   :match? (fn [_chunks st _p] (bubble-column? st))
   :pass   :shape
   :wake   column-rewake
   :due    column-due})

(defn- column-push ^double [drag? open? ^double vy]
  (cond
    (and drag? open?) (max -0.9 (- vy 0.03))
    drag? (max -0.3 (- vy 0.03))
    open? (min 1.8 (+ vy 0.1))
    :else (min 0.7 (+ vy 0.06))))

(defn open-above?
  "Returns true when st above a bubble column leaves it open.
  Such a block has no collision and no fluid."
  [^long st]
  (and (empty? (block/collision-boxes st))
       (nil? (block/liquid-class st))))

(defn bubble-push
  "Returns the vertical speed vy of a body at pos after bubbles push."
  ^double [chunks pos ^double vy]
  (let [x (num/floor (v/x pos))
        y (num/floor (v/y pos))
        z (num/floor (v/z pos))
        st (long (raw-at chunks x y z))]
    (if (bubble-column? st)
      (column-push (= :true (:drag (block/props-of st)))
                   (open-above? (raw-at chunks x (inc y) z))
                   vy)
      vy)))

(def ^:private conversion-rule
  {:water :water-source-conversion :lava :lava-source-conversion})

(defn- flow-env [chunks cls table rules]
  (let [{:keys [dropoff slope infinite? mix]} (table cls)]
    {:chunks    chunks :cls cls
     :dropoff   (long dropoff) :slope (long slope)
     :infinite? (get rules (conversion-rule cls) infinite?)
     :mix       mix}))

(defn- cell-flowed [chunks env cls p st]
  (let [v (if (source-of? cls st) :source (new-liquid env p))
        st' (if v (liquid->state cls v) 0)]
    (cond
      (zero? (long st')) [[p 0]]
      (= (long st') (long st)) (spread env p st')
      :else (into [[p st']]
                  (spread (assoc env :over {p st'}) p st')))))

(defn- reach-of ^long [table]
  (inc (long (transduce (map :slope) max 0 (vals table)))))

(def ^:private ^:table reaches
  (delay (update-vals @by-dim reach-of)))

(defn reach
  "Returns how many columns a fluid tick reads in dimension dim."
  ^long [dim]
  (long (or (get @reaches dim) (reach-of liquids))))

(defn- found-change [^ints found ^long k]
  (let [p [(long (Flow/changeX found k)) (long (Flow/changeY found k))
           (long (Flow/changeZ found k))]
        st (long (Flow/changeState found k))
        g (long (Flow/changeDrop found k))]
    (if (neg? g) [p st] [p st [[:drop g]]])))

(defn- water-cell [chunks {:keys [dropoff slope infinite?]} [x y z]]
  (when-let [found (Flow/changes
                     (@flow-tables :water) chunks
                     (int x) (int y) (int z) (int dropoff)
                     (int slope) (boolean infinite?))]
    (mapv #(found-change found %) (range (Flow/count found)))))

(defn update-cell
  "Returns the changes of the liquid at p on its fluid tick."
  [chunks [x y z :as p] ctx]
  (let [st (state-at chunks x y z)
        cls (liquid-class st)]
    (when cls
      (let [table (liquids-in (:dim ctx))
            env (flow-env chunks cls table (:rules ctx))]
        (or (when (= :water cls) (water-cell chunks env p))
            (cell-flowed chunks env cls p st))))))

(defn- fire-sides [chunks p]
  (into {:age :0}
        (map (fn [face]
               (let [st (state-toward chunks p face)]
                 [face (block/flag (block/burnable? st))])))
        [:north :south :west :east :up]))

(defn- fire-state-at [chunks [x y z :as p]]
  (let [below (long (raw-at chunks x (dec (long y)) z))]
    (cond
      (block/tagged? below "soul_fire_base_blocks")
      (block/state :soul-fire {:age :0})
      (or (block/burnable? below) (block/face-sturdy? below :up))
      (block/state :fire {:age :0})
      :else (block/state :fire (fire-sides chunks p)))))

(defn- flammable-around? [chunks p]
  (some #(block/ignited-by-lava? (state-toward chunks p %))
        flow-faces))

(defn- loaded? [chunks [_ y _ :as p]]
  (and (chunk/in-range? (long y))
       (contains? chunks (chunk/block-chunk p))))

(defn- jittered [p r3 i ^long dy]
  [(+ (long (p 0)) (long (r3 [:x i])))
   (+ (long (p 1)) dy)
   (+ (long (p 2)) (long (r3 [:z i])))])

(defn- lava-fire-walk [chunks p r3 passes]
  (loop [tp p i 0]
    (when (< i (long passes))
      (let [tp' (jittered tp r3 i 1)
            st (long (raw-at chunks (tp' 0) (tp' 1) (tp' 2)))]
        (cond
          (not (loaded? chunks tp')) nil
          (zero? st) (if (flammable-around? chunks tp')
                       [[tp' (fire-state-at chunks tp')]]
                       (recur tp' (inc i)))
          (block/blocks-motion? st) nil
          :else (recur tp' (inc i)))))))

(defn- spot-fire [chunks [_ y _] tp]
  (let [above [(tp 0) (inc (long y)) (tp 2)]
        over (raw-at chunks (above 0) (above 1) (above 2))]
    (when (and (zero? (long over))
               (block/ignited-by-lava?
                 (long (raw-at chunks (tp 0) (tp 1) (tp 2)))))
      [above (fire-state-at chunks above)])))

(defn- spot-fires [chunks p tp acc]
  (if-let [fire (spot-fire chunks p tp)] (conj acc fire) acc))

(defn- lava-spot-fires [chunks p r3]
  (loop [i 0 acc []]
    (let [tp (when (< i 3) (jittered p r3 i 0))]
      (if (and tp (loaded? chunks tp))
        (recur (inc i) (spot-fires chunks p tp acc))
        acc))))

(defn lava-random-tick
  "Returns the fires the lava at p sets on a random tick."
  [chunks p roll]
  (let [r3 (fn [salt]
             (dec (long (Math/floor (* 3.0 (double (roll salt)))))))
        passes (long (Math/floor (* 3.0 (double (roll :passes)))))]
    (if (pos? passes)
      (lava-fire-walk chunks p r3 passes)
      (lava-spot-fires chunks p r3))))

(defn- delay-of ^long [dim st]
  (long (get-in (liquids-in dim) [(liquid-class st) :delay])))

(defn fluid-wake
  "Returns the tick at which the liquid at p moves next."
  [chunks dim tick p old side]
  (let [st (chunk/chunks-get-block chunks p)]
    (+ (long tick)
       (if (nil? side)
         (update-delay dim old st tick p)
         (delay-of dim st)))))

(defn- mix-wake [chunks _dim _tick p _old _side]
  (when (mixed chunks p) :neighbor))

(defn- mix-due [chunks p _ctx]
  (when-let [st (mixed chunks p)] [[p st [:fizz]]]))

(def rule
  "The rule of liquid blocks. Lava mixes with water next to it, and a
  water source over soul sand or magma forms a bubble column."
  {:name    :liquid
   :match?  (fn [_chunks st _p] (block/liquid? (long st)))
   :pass    :neighbor
   :wake    mix-wake
   :reshape mix-due
   :due     column-due})
