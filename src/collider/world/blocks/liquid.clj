(ns collider.world.blocks.liquid
  "Water and lava, their spread and mixing.
  Also their push on entities."
  (:require [collider.data :as data]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.attribute :as attribute])
  (:import (java.util Arrays)))

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
               (keys (data/dimension-types)))))

(defn liquids-in
  "Returns the flow of water and lava in dimension dim.
  An unknown dimension flows like the overworld."
  [dim]
  (get @by-dim dim liquids))

(defn- by-class [f]
  (delay (into {} (map (fn [[cls m]] (f cls m))) liquids)))

(def ^:private ^:table base
  (by-class (fn [cls m] [cls (block/state (:block m))])))

(def ^:private ^:table bucket->class
  (by-class (fn [cls m] [(:bucket m) cls])))

(def ^:private horiz [[1 0] [-1 0] [0 1] [0 -1]])

(def ^:private ^:table water-source (delay (block/state :water)))

(def ^:private liquid-class block/liquid-class)

(def ^:private level block/liquid-level)

(defn liquid-state ^long [cls ^long level]
  (+ (long (@base cls)) level))

(defn fluid-of [st]
  (when-let [cls (liquid-class st)]
    (cond
      (zero? (level st)) cls
      (= :lava cls) :flowing-lava
      :else :flowing-water)))

(defn bucket->state [item]
  (when-let [cls (@bucket->class item)] (liquid-state cls 0)))

(defn- raw-at [chunks x y z]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks x y z)
      -1)))

(defn- state-of ^long [raw]
  (let [st (long raw)]
    (if (and (pos? st) (block/waterlogged? st)) @water-source st)))

(defn- state-at [chunks x y z]
  (state-of (raw-at chunks x y z)))

(defn- shifted [chunks [x y z] [dx dy dz]]
  (state-at chunks
            (+ (long x) (long dx))
            (+ (long y) (long dy))
            (+ (long z) (long dz))))

(defn- other-class? [cls st]
  (let [c (liquid-class st)]
    (and (some? c) (not= c cls))))

(defn- blocks-movement? [st]
  (and (pos? (long st)) (block/blocks-motion? (long st))))

(defn- decay ^long [chunks cls p]
  (let [st (state-at chunks (p 0) (p 1) (p 2))]
    (if (and (pos? (long st)) (= cls (liquid-class st)))
      (let [m (level st)] (if (>= m 8) 0 m))
      -1)))

(defn- normalize [[x y z]]
  (let [x (double x) y (double y) z (double z)
        len (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< len 1.0E-4)
      [0.0 0.0 0.0]
      [(/ x len) (/ y len) (/ z len)])))

(defn- below-pull [chunks cls i [nx y nz]]
  (let [j (decay chunks cls [nx (dec (long y)) nz])]
    (if (>= j 0) (- j (- (long i) 8)) 0)))

(defn- neighbor-pull [chunks cls i [x y z] [dx dz]]
  (let [nx (+ (long x) (long dx))
        nz (+ (long z) (long dz))
        ns (state-at chunks nx y nz)
        j (decay chunks cls [nx y nz])]
    (cond
      (other-class? cls ns) 0
      (>= j 0) (- j (long i))
      (not (blocks-movement? ns))
      (below-pull chunks cls (long i) [nx y nz])
      :else 0)))

(def ^:private side-face
  {[1 0] :east [-1 0] :west [0 1] :south [0 -1] :north})

(defn- solid-face? [cls st d]
  (let [st (long st)]
    (and (pos? st)
         (not= cls (liquid-class st))
         (not (block/tagged? st "ice"))
         (block/face-sturdy? st (side-face d)))))

(defn- walled? [chunks cls [x y z]]
  (some (fn [[dx dz :as d]]
          (let [nx (+ (long x) (long dx))
                nz (+ (long z) (long dz))
                up (inc (long y))]
            (or (solid-face? cls (raw-at chunks nx y nz) d)
                (solid-face? cls (raw-at chunks nx up nz) d))))
        horiz))

(defn- pull-sum [chunks cls i p]
  (reduce (fn [[vx vz] [dx dz :as d]]
            (let [k (neighbor-pull chunks cls i p d)]
              [(+ (double vx) (* (long dx) k))
               (+ (double vz) (* (long dz) k))]))
          [0.0 0.0]
          horiz))

(defn flow-vector [chunks [x y z :as p]]
  (let [st (state-at chunks x y z)]
    (when-let [cls (when (pos? (long st)) (liquid-class st))]
      (let [[vx vz] (pull-sum chunks cls (decay chunks cls p) p)]
        (if (and (>= (level st) 8) (walled? chunks cls p))
          (let [[nx _ nz] (normalize [vx 0.0 vz])]
            (normalize [nx -6.0 nz]))
          (normalize [vx 0.0 vz]))))))

(defn- own-height ^double [st]
  (let [l (level st)
        n (if (or (zero? l) (>= l 8)) 8 (- 8 l))]
    (/ (double n) 9.0)))

(defn- height-in ^double [chunks cls [x y z]]
  (let [above (state-at chunks x (inc (long y)) z)]
    (if (and (pos? (long above)) (= cls (liquid-class above)))
      1.0
      (own-height (state-at chunks x y z)))))

(defn surface [chunks cls [x y z :as c]]
  (let [st (state-at chunks x y z)]
    (when (and (pos? st) (= cls (liquid-class st)))
      (let [h (float (height-in chunks cls c))]
        (double (float (+ (double (float y)) (double h))))))))

(def ^:private ^:const fluid-margin
  "How far the fluid box of a body is shrunk on every side."
  0.001)

(defn- span [^double lo ^double hi]
  (range (long (Math/floor lo)) (long (Math/ceil hi))))

(defn- cells-of [x y z half height]
  (let [x (double x) y (double y) z (double z)
        a (- (double half) fluid-margin)
        y0 (+ y fluid-margin)
        y1 (- (+ y (double height)) fluid-margin)]
    (for [cx (span (- x a) (+ x a))
          cy (span y0 y1)
          cz (span (- z a) (+ z a))]
      [cx cy cz])))

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

(defn- add-fluid [chunks y acc [cx cy cz :as c]]
  (let [st (state-at chunks cx cy cz)
        cls (when (pos? (long st)) (liquid-class st))
        h (when cls
            (- (+ (double cy) (height-in chunks cls c)) (double y)))]
    (if (or (nil? cls) (< (double h) fluid-margin))
      acc
      (with-cell acc cls h c chunks))))

(defn- fluid-around [chunks [x y z] half height]
  (reduce (partial add-fluid chunks y) {}
          (cells-of x y z half height)))

(defn fluid-height [chunks pos half height cls]
  (let [around (fluid-around chunks pos half height)]
    (double (get-in around [cls :height] 0.0))))

(def ^:private ^:const min-current 0.0045000000000000005)

(defn- length ^double [[x y z]]
  (let [x (double x) y (double y) z (double z)]
    (Math/sqrt (+ (* x x) (* y y) (* z z)))))

(defn- still? [vel]
  (and (< (Math/abs (v/x vel)) 0.003)
       (< (Math/abs (v/z vel)) 0.003)))

(defn- unit-times [[x y z] ^double len ^double k]
  [(* (/ (double x) len) k)
   (* (/ (double y) len) k)
   (* (/ (double z) len) k)])

(defn- current [flow ^double len2 ^double p vel]
  (let [push (unit-times flow (Math/sqrt len2) p)
        ilen (length push)]
    (if (and (still? vel) (< ilen min-current))
      (unit-times push ilen min-current)
      push)))

(defn- square ^double [[x y z]]
  (let [x (double x) y (double y) z (double z)]
    (+ (* x x) (* y y) (* z z))))

(defn- current-push [table around vel]
  (reduce (fn [acc [cls {:keys [flow n]}]]
            (let [len2 (square flow)
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
  "The water and lava standing over a body and the push of their
  flow, all of one walk over the cells its box meets."
  ([chunks pos half height vel]
   (fluid-info chunks pos half height vel nil))
  ([chunks pos half height vel dim]
   (let [around (fluid-around chunks pos half height)]
     {:water (double (get-in around [:water :height] 0.0))
      :lava  (double (get-in around [:lava :height] 0.0))
      :push  (current-push (liquids-in dim) around vel)})))

(def ^:private horiz3 [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1]])

(def ^:private horiz3+
  [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0] [0 -1 0]])

(def ^:private opposite
  {[1 0 0] [-1 0 0] [-1 0 0] [1 0 0]
   [0 0 1] [0 0 -1] [0 0 -1] [0 0 1]})

(def ^:private no-fluid-types
  #{:door :standing-sign :wall-sign :ladder :sugar-cane
    :bubble-column})

(defn- amount ^long [st]
  (let [l (level st)] (if (or (zero? l) (>= l 8)) 8 (- 8 l))))

(defn- falling? [st] (= 8 (level st)))

(defn- same? [cls st] (= cls (liquid-class st)))

(defn- source-of? [cls st] (and (same? cls st) (zero? (level st))))

(defn- height ^double [st] (/ (double (amount st)) 9.0))

(defn- above-at [chunks [x y z]]
  (let [up (inc (long y))]
    (if (chunk/in-range? up)
      (chunk/chunks-get-block chunks [x up z])
      0)))

(defn fluid-height-of [chunks p st mode]
  (let [cls (liquid-class st)]
    (when (and cls (or (not= mode :source-only) (source-of? cls st)))
      (if (same? cls (above-at chunks p)) 1.0 (height st)))))

(defn- boxes [st]
  (if (pos? (long st)) (block/collision-boxes (long st)) []))

(def ^:private ^ThreadLocal cover-rows
  (proxy [ThreadLocal] [] (initialValue [] (int-array 16))))

(defn- lo-edge ^long [box ^long i]
  (max 0 (long (Math/ceil (double (nth box i))))))

(defn- hi-edge ^long [box ^long i]
  (min 16 (long (Math/floor (double (nth box i))))))

(defn- mark-face [^ints rows box ^long u ^long v]
  (let [u0 (lo-edge box u) u1 (hi-edge box (+ u 3))
        v0 (lo-edge box v) v1 (hi-edge box (+ v 3))]
    (when (and (< u0 u1) (< v0 v1))
      (let [bits (dec (bit-shift-left 1 (- v1 v0)))
            m (int (bit-shift-left bits v0))]
        (loop [a u0]
          (when (< a u1)
            (aset rows a (int (bit-or (aget rows a) m)))
            (recur (inc a))))))))

(defn- all-rows? [^ints rows]
  (loop [i 0]
    (cond
      (= i 16) true
      (not= 0xFFFF (aget rows i)) false
      :else (recur (inc i)))))

(defn- face-covered? [first second ^long axis]
  (let [^ints rows (ThreadLocal/.get cover-rows)
        u (if (= axis 0) 1 0)
        v (if (= axis 2) 1 2)]
    (Arrays/fill rows (int 0))
    (doseq [box first :when (== 16.0 (double (nth box (+ axis 3))))]
      (mark-face rows box u v))
    (doseq [box second :when (== 0.0 (double (nth box axis)))]
      (mark-face rows box u v))
    (all-rows? rows)))

(defn- axis-of ^long [d]
  (cond (not= 0 (long (d 0))) 0 (not= 0 (long (d 1))) 1 :else 2))

(defn- faces-open? [src tgt d]
  (let [axis (axis-of d)
        s (boxes src) t (boxes tgt)]
    (if (pos? (long (d axis)))
      (not (face-covered? s t axis))
      (not (face-covered? t s axis)))))

(defn- pass-wall? [src tgt d]
  (let [src (long src) tgt (long tgt)]
    (cond
      (or (neg? src) (neg? tgt)) false
      (or (block/full-cube? tgt) (block/full-cube? src)) false
      (and (empty? (boxes src)) (empty? (boxes tgt))) true
      :else (faces-open? src tgt d))))

(defn- container? [st]
  (let [st (long st)]
    (and (pos? st)
         (or (contains? (block/props-of st) :waterlogged)
             (contains? block/water-holder-types
                        (block/type-of st))))))

(defn- holds-any-fluid? [st]
  (let [st (long st)]
    (cond
      (zero? st) true
      (container? st) true
      (blocks-movement? st) false
      :else (not (contains? no-fluid-types (block/type-of st))))))

(defn- holds-specific? [cls st]
  (if (container? st)
    (and (= :water cls) (not (block/waterlogged? (long st))))
    true))

(defn- can-hold? [cls st]
  (and (holds-any-fluid? st) (holds-specific? cls st)))

(defn- replaceable-with? [tgt cls d]
  (case (liquid-class tgt)
    nil true
    :water (and (= d [0 -1 0]) (not= cls :water))
    :lava (and (= cls :water) (>= (height tgt) 0.44444445))))

(defn- can-maybe-pass? [cls src-raw tgt-raw tgt d]
  (and (not (source-of? cls tgt))
       (holds-any-fluid? tgt-raw)
       (pass-wall? src-raw tgt-raw d)))

(defn- raw-of ^long [{:keys [chunks]} x y z]
  (long (raw-at chunks x y z)))

(defn- raw-by [env [x y z] [dx dy dz]]
  (raw-of env (+ (long x) (long dx)) (+ (long y) (long dy))
          (+ (long z) (long dz))))

(defn- hole? [{:keys [cls] :as env} p]
  (let [raw (raw-by env p [0 0 0])
        braw (raw-by env p [0 -1 0])]
    (and (pass-wall? raw braw [0 -1 0])
         (or (same? cls (state-of braw)) (can-hold? cls braw)))))

(defn- horizontal-source-scan [{:keys [cls] :as env} raw p]
  (reduce (fn [[h s] d]
            (let [nraw (raw-by env p d)
                  n (state-of nraw)]
              (if (and (same? cls n) (pass-wall? raw nraw d))
                [(max (long h) (amount n))
                 (if (source-of? cls n) (inc (long s)) s)]
                [h s])))
          [0 0] horiz3))

(defn- source-ground? [cls braw]
  (or (block/solid? (long (max 0 (long braw))))
      (source-of? cls (state-of braw))))

(defn- new-liquid [{:keys [cls dropoff infinite?] :as env} p]
  (let [raw (raw-by env p [0 0 0])
        [highest sources] (horizontal-source-scan env raw p)
        braw (raw-by env p [0 -1 0])
        araw (raw-by env p [0 1 0])]
    (cond
      (and infinite? (>= (long sources) 2) (source-ground? cls braw))
      :source
      (and (same? cls (state-of araw)) (pass-wall? raw araw [0 1 0]))
      :falling
      :else (let [n (- (long highest) (long dropoff))]
              (when (pos? n) n)))))

(defn- liquid->state ^long [cls v]
  (case v
    :source (liquid-state cls 0)
    :falling (liquid-state cls 8)
    (liquid-state cls (- 8 (long v)))))

(defn- step [[x y z] [dx _ dz]]
  [(+ (long x) (long dx)) y (+ (long z) (long dz))])

(defn- passable? [cls raw traw d]
  (and (can-maybe-pass? cls raw traw (state-of traw) d)
       (holds-specific? cls traw)))

(defn- blocked? [{:keys [cls] :as env} raw tp d]
  (not (passable? cls raw (raw-by env tp [0 0 0]) d)))

(defn- slope-distance
  "Returns the steps of falling ground ahead of a liquid leaving p."
  ^long [{:keys [slope] :as env} p ^long pass from]
  (let [raw (raw-by env p [0 0 0])
        n (inc pass)]
    (reduce (fn [lowest d]
              (let [tp (step p d)]
                (cond
                  (or (= d from) (blocked? env raw tp d)) lowest
                  (hole? env tp) (reduced pass)
                  (< pass (long slope))
                  (->> (slope-distance env tp n (opposite d))
                       (min (long lowest)))
                  :else lowest)))
            1000 horiz3)))

(defn- mix-product [mix m]
  (when mix
    (let [k (if (zero? (long m)) :source :flowing)]
      (block/state (k mix)))))

(def ^:private ^:table basalt-state (delay (block/state :basalt)))

(def ^:private ^:table soul-soil-state
  (delay (block/state :soul-soil)))

(def ^:private ^:table blue-ice-state
  (delay (block/state :blue-ice)))

(def ^:private mix-dirs
  [[0 1 0] [0 0 -1] [0 0 1] [-1 0 0] [1 0 0]])

(defn- raw-over ^long [chunks over [x y z :as p]]
  (if (= p (first over))
    (long (second over))
    (long (raw-at chunks x y z))))

(defn- mixed-by [chunks over p st soul? d]
  (let [n (raw-over chunks over (mapv + p d))]
    (cond
      (block/water? n)
      (mix-product (get-in liquids [:lava :mix]) (level st))
      (and soul? (== n (long @blue-ice-state))) @basalt-state)))

(defn mixed
  "Returns the block the lava at p turns into, or nil when it
  stays. This is shouldSpreadLiquid of vanilla. over is a change
  [q st] that is read in place of the block at q, or nil."
  ([chunks p] (mixed chunks nil p))
  ([chunks over [x y z :as p]]
   (let [st (raw-over chunks over p)
         below (raw-over chunks over [x (dec (long y)) z])
         soul? (== below (long @soul-soil-state))]
     (when (and (block/liquid? st) (block/lava? st))
       (some #(mixed-by chunks over p st soul? %) mix-dirs)))))

(def ^:private update-order
  (mapv dir/offset [:west :east :down :up :north :south]))

(defn- converted [chunks over p d]
  (let [np (mapv + p d)]
    (when-let [prod (mixed chunks over np)]
      [np prod [:fizz]])))

(defn- with-neighbors [chunks [tp st :as change]]
  (into [change]
        (keep #(converted chunks [tp st] tp %))
        update-order))

(defn- logged [traw]
  (let [traw (long traw)
        props (assoc (block/props-of traw) :waterlogged :true)]
    (block/state (block/block-of traw) props)))

(def ^:private air-blocks #{:air :cave-air :void-air})

(defn- destroys? [mix traw]
  (and mix (pos? (long traw))
       (not (contains? air-blocks (block/block-of (long traw))))))

(defn- spread-plain [{:keys [chunks cls mix]} tp v traw]
  (let [st (liquid->state cls v)
        prod (mixed chunks [tp st] tp)
        fx (cond-> []
             (destroys? mix traw) (conj :fizz)
             prod (conj :fizz))]
    (with-neighbors chunks
                    (cond-> [tp (or prod st)] (seq fx) (conj fx)))))

(defn- spread-to [{:keys [mix] :as env} tp d v]
  (let [traw (raw-by env tp [0 0 0])]
    (cond
      (and mix (= d [0 -1 0]) (block/water? traw))
      [[tp (block/state (:smother mix)) [:fizz]]]
      (container? traw)
      (with-neighbors (:chunks env) [tp (logged traw)])
      :else (spread-plain env tp v traw))))

(defn- target-of [{:keys [cls] :as env} raw p d]
  (let [tp (step p d)
        traw (raw-by env tp [0 0 0])
        t (state-of traw)
        v (and (can-maybe-pass? cls raw traw t d)
               (new-liquid env tp))]
    (when (and v (holds-specific? cls traw))
      {:tp tp :t t :v v
       :dist (if (hole? env tp)
               0
               (slope-distance env tp 1 (opposite d)))})))

(defn- lowest-targets [{:keys [cls] :as env} raw p]
  (second
    (reduce (fn [[lowest acc :as best] d]
              (if-let [{:keys [tp t v dist]} (target-of env raw p d)]
                (let [dist (long dist)
                      acc (if (< dist (long lowest)) [] acc)]
                  (cond
                    (> dist (long lowest)) best
                    (replaceable-with? t cls d)
                    [dist (conj acc [tp d v])]
                    :else [dist acc]))
                best))
            [1000 []] horiz3)))

(defn- spread-sides [{:keys [dropoff] :as env} p st]
  (let [n (if (falling? st) 7 (- (amount st) (long dropoff)))]
    (when (pos? n)
      (into []
            (mapcat (fn [[tp d v]] (spread-to env tp d v)))
            (lowest-targets env (raw-by env p [0 0 0]) p)))))

(defn- source-neighbours ^long [{:keys [cls] :as env} p]
  (count (filter #(source-of? cls (state-of (raw-by env p %)))
                 horiz3)))

(defn- spread-down [{:keys [cls] :as env} p st]
  (let [bp [(p 0) (dec (long (p 1))) (p 2)]
        braw (raw-by env bp [0 0 0])
        b (state-of braw)
        raw (raw-by env p [0 0 0])]
    (when (can-maybe-pass? cls raw braw b [0 -1 0])
      (when-let [v (new-liquid env bp)]
        (when (and (replaceable-with? b cls [0 -1 0])
                   (holds-specific? cls braw))
          (into (vec (spread-to env bp [0 -1 0] v))
                (when (>= (source-neighbours env p) 3)
                  (spread-sides env p st))))))))

(defn- spread [{:keys [cls] :as env} p st]
  (or (spread-down env p st)
      (when (or (source-of? cls st) (not (hole? env p)))
        (spread-sides env p st))))

(defn- rising? [cls old new]
  (and (same? cls old)
       (not (falling? old)) (not (falling? new))
       (> (height new) (height old))))

(defn update-delay
  "Returns the ticks until a liquid that went from old to new
  moves again. Lava that rises waits longer, at random."
  ([old new tick pos] (update-delay nil old new tick pos))
  ([dim old new tick pos]
   (let [cls (liquid-class new)
         {:keys [delay decay-jitter]} ((liquids-in dim) cls)]
     (if (and decay-jitter
              (rising? cls old new)
              (not= 0 (mod (hash [tick pos]) 4)))
       (* (long delay) (long decay-jitter))
       (long delay)))))

(defn- with-sides [p]
  (cons p (map #(mapv + p %) horiz3+)))

(defn mix-changes [chunks positions]
  (into []
        (comp (mapcat with-sides)
              (distinct)
              (keep (fn [p]
                      (when-let [st (mixed chunks p)]
                        [p st [:fizz]]))))
        positions))

(def ^:private column-drag {:soul-sand :false :magma :true})

(defn bubble-column? [st]
  (= :bubble-column (block/type-of (long st))))

(defn- column-state [below]
  (if (bubble-column? below)
    below
    (when-let [drag (get column-drag (block/type-of (long below)))]
      (block/state :bubble-column {:drag drag}))))

(defn- water-source? [st] (= (long st) @water-source))

(defn- column-changes [chunks [x y z] col]
  (loop [y (long y) acc []]
    (let [st (long (raw-at chunks x y z))]
      (if (or (water-source? st)
              (and (bubble-column? st) (not= st (long col))))
        (recur (inc y) (conj acc [[x y z] col]))
        acc))))

(defn- bubble-changes [chunks [x y z :as p]]
  (let [raw (long (raw-at chunks x y z))
        col (column-state (long (raw-at chunks x (dec (long y)) z)))]
    (cond
      (and (bubble-column? raw) (nil? col)) [[p @water-source]]
      (and col (or (water-source? raw) (not= raw (long col))))
      (column-changes chunks p col))))

(defn- column-push ^double [drag? open? ^double vy]
  (cond
    (and drag? open?) (max -0.9 (- vy 0.03))
    drag? (max -0.3 (- vy 0.03))
    open? (min 1.8 (+ vy 0.1))
    :else (min 0.7 (+ vy 0.06))))

(defn bubble-push ^double [chunks pos ^double vy]
  (let [x (long (Math/floor (v/x pos)))
        y (long (Math/floor (v/y pos)))
        z (long (Math/floor (v/z pos)))
        st (long (raw-at chunks x y z))]
    (if (bubble-column? st)
      (column-push (= :true (:drag (block/props-of st)))
                   (zero? (long (raw-at chunks x (inc y) z)))
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
      (and (= :source v) (seq (bubble-changes chunks p)))
      (bubble-changes chunks p)
      :else (into (if (not= (long st') (long st)) [[p st']] [])
                  (spread env p st')))))

(defn update-cell
  "Returns the changes of the liquid at p on its fluid tick.
  ctx gives the dimension and the game rules."
  [chunks [x y z :as p] ctx]
  (let [st (state-at chunks x y z)
        cls (liquid-class st)]
    (when cls
      (let [table (liquids-in (:dim ctx))
            env (flow-env chunks cls table (:rules ctx))]
        (cell-flowed chunks env cls p st)))))

(defn- ground ^long [raw] (long (max 0 (long raw))))

(defn- fire-sides [chunks p]
  (into {:age :0}
        (map (fn [[k d]]
               (let [st (ground (shifted chunks p d))]
                 [k (if (block/burnable? st) :true :false)])))
        {:north [0 0 -1] :south [0 0 1] :west [-1 0 0]
         :east [1 0 0] :up [0 1 0]}))

(defn- fire-state-at [chunks [x y z :as p]]
  (let [below (ground (raw-at chunks x (dec (long y)) z))]
    (cond
      (block/tagged? below "soul_fire_base_blocks")
      (block/state :soul-fire {:age :0})
      (or (block/burnable? below) (block/face-sturdy? below :up))
      (block/state :fire {:age :0})
      :else (block/state :fire (fire-sides chunks p)))))

(defn- flammable-around? [chunks p]
  (some #(block/ignited-by-lava? (ground (shifted chunks p %)))
        horiz3+))

(defn- walk-step [tp r3 i]
  [(+ (long (tp 0)) (long (r3 [:x i])))
   (inc (long (tp 1)))
   (+ (long (tp 2)) (long (r3 [:z i])))])

(defn- lava-fire-walk [chunks p r3 passes]
  (loop [tp p i 0]
    (when (< i (long passes))
      (let [tp' (walk-step tp r3 i)
            st (raw-at chunks (tp' 0) (tp' 1) (tp' 2))]
        (cond
          (neg? st) nil
          (zero? st) (if (flammable-around? chunks tp')
                       [[tp' (fire-state-at chunks tp')]]
                       (recur tp' (inc i)))
          (block/blocks-motion? st) nil
          :else (recur tp' (inc i)))))))

(defn- spot-fire [chunks [x y z] r3 i]
  (let [tp [(+ (long x) (long (r3 [:x i]))) y
            (+ (long z) (long (r3 [:z i])))]
        above [(tp 0) (inc (long y)) (tp 2)]
        over (raw-at chunks (above 0) (above 1) (above 2))]
    (when (and (zero? (long over))
               (block/ignited-by-lava?
                 (ground (raw-at chunks (tp 0) (tp 1) (tp 2)))))
      [above (fire-state-at chunks above)])))

(defn- lava-spot-fires [chunks p r3]
  (into [] (keep #(spot-fire chunks p r3 %)) (range 3)))

(defn lava-random-tick [chunks p roll]
  (let [r3 (fn [salt]
             (dec (long (Math/floor (* 3.0 (double (roll salt)))))))
        passes (long (Math/floor (* 3.0 (double (roll :passes)))))]
    (if (pos? passes)
      (lava-fire-walk chunks p r3 passes)
      (lava-spot-fires chunks p r3))))

(defn- delay-of ^long [dim st]
  (long (get-in (liquids-in dim) [(liquid-class st) :delay])))

(defn fluid-wake [chunks dim tick p old side]
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
  {:name    :liquid
   :match?  (fn [_chunks st _p] (block/liquid? (long st)))
   :wake    mix-wake
   :reshape mix-due})
