(ns collider.world.blocks.liquid
  "Water and lava, their spread and mixing, and their push
  on entities."
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(def liquids
  {:water {:block :water :dropoff 1 :slope 4 :delay 5 :bucket :water-bucket :infinite? true
           :push  0.014}
   :lava  {:block        :lava :dropoff 2 :slope 2 :delay 30 :bucket :lava-bucket :infinite? false
           :push         0.0023333333333333335
           :decay-jitter 4
           :mix          {:source :obsidian :flowing :cobblestone :smother :stone}}})

(def ^:private ^:table base
  (delay (into {} (map (fn [[cls {:keys [block]}]] [cls (block/state block)])) liquids)))

(def ^:private ^:table class-of-block
  (delay (into {} (map (fn [[cls {:keys [block]}]] [block cls])) liquids)))

(def ^:private ^:table bucket->class
  (delay (into {} (map (fn [[cls {:keys [bucket]}]] [bucket cls])) liquids)))

(def ^:private horiz [[1 0] [-1 0] [0 1] [0 -1]])

(def ^:private ^:table water-source (delay (block/state :water)))

(def ^:private liquid-state? block/liquid?)

(def ^:private liquid-class block/liquid-class)

(def ^:private level block/liquid-level)

(defn liquid-state ^long [cls ^long level]
  (+ (long (@base cls)) level))

(defn bucket->state [item] (when-let [cls (@bucket->class item)] (liquid-state cls 0)))

(defn delay-of [st] (long (get-in liquids [(liquid-class st) :delay])))

(def ^:private source-state? block/source-state?)

(defn mix-class? [st]
  (some? (get-in liquids [(liquid-class st) :mix])))

(defn push-of [st]
  (get-in liquids [(liquid-class st) :push]))

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

(defn- effective ^long [st] (let [m (level st)] (if (>= m 8) 0 m)))

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
      (not (blocks-movement? ns)) (below-pull chunks cls (long i) [nx y nz])
      :else 0)))

(def ^:private side-face {[1 0] :east [-1 0] :west [0 1] :south [0 -1] :north})

(defn- solid-face? [cls st d]
  (let [st (long st)]
    (and (pos? st)
         (not= cls (liquid-class st))
         (not (block/tagged? st "ice"))
         (block/face-sturdy? st (side-face d)))))

(defn- walled? [chunks cls [x y z]]
  (some (fn [[dx dz :as d]]
          (let [nx (+ (long x) (long dx))
                nz (+ (long z) (long dz))]
            (or (solid-face? cls (raw-at chunks nx y nz) d)
                (solid-face? cls (raw-at chunks nx (inc (long y)) nz) d))))
        horiz))

(defn flow-vector [chunks [x y z :as p]]
  (let [st (state-at chunks x y z)]
    (when-let [cls (when (pos? (long st)) (liquid-class st))]
      (let [i (decay chunks cls p)
            [vx vz] (reduce (fn [[vx vz] [dx dz :as d]]
                              (let [k (neighbor-pull chunks cls i p d)]
                                [(+ (double vx) (* (long dx) k))
                                 (+ (double vz) (* (long dz) k))]))
                            [0.0 0.0]
                            horiz)]
        (if (and (>= (level st) 8) (walled? chunks cls p))
          (let [[nx _ nz] (normalize [vx 0.0 vz])]
            (normalize [nx -6.0 nz]))
          (normalize [vx 0.0 vz]))))))

(defn- own-height ^double [st]
  (let [l (level st)] (/ (double (if (or (zero? l) (>= l 8)) 8 (- 8 l))) 9.0)))

(defn- height-in ^double [chunks cls [x y z]]
  (let [above (state-at chunks x (inc (long y)) z)]
    (if (and (pos? (long above)) (= cls (liquid-class above)))
      1.0
      (own-height (state-at chunks x y z)))))

(defn- cells-of [x y z half height]
  (let [x (double x) y (double y) z (double z) half (double half) height (double height)]
    (for [cx (range (long (Math/floor (- x half))) (long (Math/ceil (+ x half))))
          cy (range (long (Math/floor y)) (long (Math/ceil (+ y height))))
          cz (range (long (Math/floor (- z half))) (long (Math/ceil (+ z half))))]
      [cx cy cz])))

(defn- add-fluid [chunks y acc [cx cy cz :as c]]
  (let [st (state-at chunks cx cy cz)
        cls (when (pos? (long st)) (liquid-class st))
        h (when cls (- (+ (double cy) (height-in chunks cls c)) (double y)))]
    (if (or (nil? cls) (neg? (double h)))
      acc
      (let [h (max (double h) (double (get-in acc [cls :height] 0.0)))
            [fx fy fz] (or (flow-vector chunks c) [0.0 0.0 0.0])
            k (if (< h 0.4) h 1.0)
            [ax ay az] (get-in acc [cls :flow] [0.0 0.0 0.0])]
        (assoc acc cls {:height h
                        :flow   [(+ (double ax) (* (double fx) k))
                                 (+ (double ay) (* (double fy) k))
                                 (+ (double az) (* (double fz) k))]
                        :n      (inc (long (get-in acc [cls :n] 0)))})))))

(defn- fluid-around [chunks [x y z] half height]
  (reduce (partial add-fluid chunks y) {} (cells-of x y z half height)))

(defn fluid-height [chunks pos half height cls]
  (double (get-in (fluid-around chunks pos half height) [cls :height] 0.0)))

(defn entity-push [chunks pos half height vel]
  (reduce (fn [[ax ay az] [cls {[fx fy fz] :flow n :n}]]
            (let [len2 (+ (* (double fx) (double fx)) (* (double fy) (double fy)) (* (double fz) (double fz)))
                  p (double (get-in liquids [cls :push] 0.0))]
              (if (or (zero? (long n)) (< len2 1.0E-5) (zero? p))
                [ax ay az]
                (let [len (Math/sqrt len2)
                      [ix iy iz] [(* (/ (double fx) len) p) (* (/ (double fy) len) p) (* (/ (double fz) len) p)]
                      ilen (Math/sqrt (+ (* ix ix) (* iy iy) (* iz iz)))
                      [ix iy iz] (if (and (< (Math/abs (v/x vel)) 0.003) (< (Math/abs (v/z vel)) 0.003) (< ilen 0.0045))
                                   [(* (/ ix ilen) 0.0045) (* (/ iy ilen) 0.0045) (* (/ iz ilen) 0.0045)]
                                   [ix iy iz])]
                  [(+ (double ax) ix) (+ (double ay) iy) (+ (double az) iz)]))))
          [0.0 0.0 0.0]
          (fluid-around chunks pos half height)))

(def ^:private horiz3 [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1]])

(def ^:private horiz3+ [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0] [0 -1 0]])

(def ^:private opposite {[1 0 0] [-1 0 0] [-1 0 0] [1 0 0] [0 0 1] [0 0 -1] [0 0 -1] [0 0 1]})

(def ^:private no-fluid-types #{:door :standing-sign :wall-sign :ladder :sugar-cane :bubble-column})

(defn- amount ^long [st] (let [l (level st)] (if (or (zero? l) (>= l 8)) 8 (- 8 l))))

(defn- falling? [st] (= 8 (level st)))

(defn- same? [cls st] (= cls (liquid-class st)))

(defn- source-of? [cls st] (and (same? cls st) (zero? (level st))))

(defn- height ^double [st] (/ (double (amount st)) 9.0))

(defn fluid-height-of [chunks [x y z] st mode]
  (let [cls (liquid-class st)]
    (when (and cls (or (not= mode :source-only) (source-of? cls st)))
      (let [above (if (chunk/in-range? (inc (long y))) (chunk/chunks-get-block chunks [x (inc (long y)) z]) 0)]
        (if (same? cls above) 1.0 (height st))))))

(defn- boxes [st] (if (pos? (long st)) (block/collision-boxes (long st)) []))

(def ^:private ^ThreadLocal cover-rows
  (proxy [ThreadLocal] [] (initialValue [] (int-array 16))))

(defn- mark-face [^ints rows box ^long u ^long v]
  (let [u0 (max 0 (long (Math/ceil (double (nth box u)))))
        u1 (min 16 (long (Math/floor (double (nth box (+ u 3))))))
        v0 (max 0 (long (Math/ceil (double (nth box v)))))
        v1 (min 16 (long (Math/floor (double (nth box (+ v 3))))))]
    (when (and (< u0 u1) (< v0 v1))
      (let [m (int (bit-shift-left (dec (bit-shift-left 1 (- v1 v0))) v0))]
        (loop [a u0]
          (when (< a u1)
            (aset rows a (int (bit-or (aget rows a) m)))
            (recur (inc a))))))))

(defn- face-covered? [first second ^long axis]
  (let [^ints rows (.get cover-rows)
        u (if (= axis 0) 1 0)
        v (if (= axis 2) 1 2)]
    (Arrays/fill rows (int 0))
    (doseq [box first :when (== 16.0 (double (nth box (+ axis 3))))] (mark-face rows box u v))
    (doseq [box second :when (== 0.0 (double (nth box axis)))] (mark-face rows box u v))
    (loop [i 0]
      (cond
        (= i 16) true
        (not= 0xFFFF (aget rows i)) false
        :else (recur (inc i))))))

(defn- pass-wall? [src tgt d]
  (let [src (long src) tgt (long tgt)]
    (cond
      (or (neg? src) (neg? tgt)) false
      (or (block/full-cube? tgt) (block/full-cube? src)) false
      (and (empty? (boxes src)) (empty? (boxes tgt))) true
      :else (let [axis (cond (not= 0 (long (d 0))) 0 (not= 0 (long (d 1))) 1 :else 2)
                  positive? (pos? (long (d axis)))
                  s (boxes src) t (boxes tgt)]
              (not (face-covered? (if positive? s t) (if positive? t s) axis))))))

(defn- container? [st]
  (and (pos? (long st))
       (or (contains? (block/props-of (long st)) :waterlogged)
           (contains? block/water-holder-types (block/type-of (long st))))))

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

(defn- can-hold? [cls st] (and (holds-any-fluid? st) (holds-specific? cls st)))

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

(defn- hole? [{:keys [cls] :as env} [x y z :as p]]
  (let [raw (raw-of env x y z)
        braw (raw-of env x (dec (long y)) z)]
    (and (pass-wall? raw braw [0 -1 0])
         (or (same? cls (state-of braw)) (can-hold? cls braw)))))

(defn- horizontal-source-scan [{:keys [cls] :as env} raw [x y z]]
  (reduce (fn [[h s] [dx _ dz :as d]]
            (let [nraw (raw-of env (+ (long x) dx) y (+ (long z) dz))
                  n (state-of nraw)]
              (if (and (same? cls n) (pass-wall? raw nraw d))
                [(max (long h) (amount n)) (if (source-of? cls n) (inc (long s)) s)]
                [h s])))
          [0 0] horiz3))

(defn- new-liquid [{:keys [cls dropoff infinite?] :as env} [x y z :as p]]
  (let [raw (raw-of env x y z)
        [highest sources] (horizontal-source-scan env raw p)
        braw (raw-of env x (dec (long y)) z)
        b (state-of braw)
        araw (raw-of env x (inc (long y)) z)
        a (state-of araw)]
    (cond
      (and infinite? (>= (long sources) 2)
           (or (block/solid? (long (max 0 (long braw)))) (source-of? cls b)))
      :source
      (and (same? cls a) (pass-wall? raw araw [0 1 0]))
      :falling
      :else (let [n (- (long highest) (long dropoff))] (when (pos? n) n)))))

(defn- liquid->state ^long [cls v]
  (case v
    :source (liquid-state cls 0)
    :falling (liquid-state cls 8)
    (liquid-state cls (- 8 (long v)))))

(defn- slope-distance
  "Returns how many steps of falling ground lie ahead of a liquid
  leaving p."
  ^long [{:keys [cls slope] :as env} [x y z :as p] ^long pass from]
  (let [raw (raw-of env x y z)]
    (reduce (fn [lowest [dx _ dz :as d]]
              (if (= d from)
                lowest
                (let [tp [(+ (long x) dx) y (+ (long z) dz)]
                      traw (raw-of env (tp 0) (tp 1) (tp 2))
                      t (state-of traw)]
                  (if (and (can-maybe-pass? cls raw traw t d) (holds-specific? cls traw))
                    (cond
                      (hole? env tp) (reduced pass)
                      (< pass (long slope)) (min (long lowest) (slope-distance env tp (inc pass) (opposite d)))
                      :else lowest)
                    lowest))))
            1000 horiz3)))

(defn- mix-product [mix m]
  (when mix
    (block/state (if (zero? (long m)) (:source mix) (:flowing mix)))))

(def ^:private contact-dirs [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])

(def ^:private convert-dirs [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 -1 0]])

(defn- touches-other? [chunks cls pos]
  (some (fn [d] (other-class? cls (shifted chunks pos d))) contact-dirs))

(defn- convert-neighbors [chunks cls [x y z]]
  (into []
        (keep (fn [[dx dy dz]]
                (let [np [(+ (long x) (long dx))
                          (+ (long y) (long dy))
                          (+ (long z) (long dz))]
                      ns (state-at chunks (np 0) (np 1) (np 2))
                      nc (liquid-class ns)]
                  (when (and (some? nc) (not= nc cls))
                    (when-let [prod (mix-product (get-in liquids [nc :mix]) (level ns))]
                      [np prod])))))
        convert-dirs))

(defn mix-wake? [chunks pos]
  (let [st (state-at chunks (pos 0) (pos 1) (pos 2))
        cls (liquid-class st)]
    (boolean
      (and cls
           (get-in liquids [cls :mix])
           (touches-other? chunks cls pos)))))

(defn- spread-to [{:keys [chunks cls mix] :as env} tp d v]
  (let [traw (raw-of env (tp 0) (tp 1) (tp 2))
        t (state-of traw)]
    (cond
      (and mix (= d [0 -1 0]) (= :water (liquid-class t)))
      [[tp (block/state (:smother mix))]]
      (container? traw)
      [[tp (block/state (block/block-of (long traw)) (assoc (block/props-of (long traw)) :waterlogged :true))]]
      :else
      (let [plain (liquid->state cls v)
            st (if (and mix (touches-other? chunks cls tp))
                 (or (mix-product mix (level plain)) plain)
                 plain)]
        (cons [tp st] (convert-neighbors chunks cls tp))))))

(defn- lowest-targets [{:keys [cls] :as env} raw [x y z]]
  (second
    (reduce (fn [[lowest acc] [dx _ dz :as d]]
              (let [tp [(+ (long x) dx) y (+ (long z) dz)]
                    traw (raw-of env (tp 0) (tp 1) (tp 2))
                    t (state-of traw)
                    v (and (can-maybe-pass? cls raw traw t d) (new-liquid env tp))]
                (if-not (and v (holds-specific? cls traw))
                  [lowest acc]
                  (let [dist (if (hole? env tp) 0 (slope-distance env tp 1 (opposite d)))
                        acc (if (< dist (long lowest)) [] acc)]
                    (if (<= dist (long lowest))
                      [dist (if (replaceable-with? t cls d) (conj acc [tp d v]) acc)]
                      [lowest acc])))))
            [1000 []] horiz3)))

(defn- spread-sides [{:keys [dropoff] :as env} p st]
  (let [n (if (falling? st) 7 (- (amount st) (long dropoff)))]
    (when (pos? n)
      (let [raw (raw-of env (p 0) (p 1) (p 2))]
        (into [] (mapcat (fn [[tp d v]] (spread-to env tp d v))) (lowest-targets env raw p))))))

(defn- source-neighbours ^long [{:keys [cls] :as env} [x y z]]
  (count (filter (fn [[dx _ dz]] (source-of? cls (state-of (raw-of env (+ (long x) dx) y (+ (long z) dz))))) horiz3)))

(defn- spread [{:keys [cls] :as env} [x y z :as p] st]
  (let [bp [x (dec (long y)) z]
        raw (raw-of env x y z)
        braw (raw-of env x (dec (long y)) z)
        b (state-of braw)]
    (or (when (can-maybe-pass? cls raw braw b [0 -1 0])
          (when-let [v (new-liquid env bp)]
            (when (and (replaceable-with? b cls [0 -1 0]) (holds-specific? cls braw))
              (into (vec (spread-to env bp [0 -1 0] v))
                    (when (>= (source-neighbours env p) 3) (spread-sides env p st))))))
        (when (or (source-of? cls st) (not (hole? env p)))
          (spread-sides env p st)))))

(defn update-delay ^long [old new tick pos]
  (let [cls (liquid-class new)
        {:keys [delay decay-jitter]} (liquids cls)]
    (if (and decay-jitter
             (same? cls old)
             (not (falling? old)) (not (falling? new))
             (> (height new) (height old))
             (not= 0 (mod (hash [tick pos]) 4)))
      (* (long delay) (long decay-jitter))
      (long delay))))

(defn- side-states [chunks [x y z]]
  (mapv (fn [[dx dz]]
          (state-at chunks (+ (long x) (long dx)) y (+ (long z) (long dz))))
        horiz))

(def ^:private ^:table basalt-state (delay (block/state :basalt)))

(def ^:private ^:table soul-soil-state (delay (block/state :soul-soil)))

(def ^:private ^:table blue-ice-state (delay (block/state :blue-ice)))

(defn- mixed-state [cls mix st above sides below-raw]
  (when mix
    (cond
      (some (fn [s] (other-class? cls s)) (cons above sides)) (mix-product mix (level st))
      (and (= (long below-raw) (long @soul-soil-state))
           (some #(= (long %) (long @blue-ice-state)) (cons above sides))) @basalt-state
      :else nil)))

(defn- solidified [chunks [x y z :as p]]
  (let [st (state-at chunks x y z)
        cls (liquid-class st)]
    (when-let [mix (and cls (get-in liquids [cls :mix]))]
      (let [above (shifted chunks p [0 1 0])
            sides (side-states chunks p)
            below-raw (raw-at chunks x (dec (long y)) z)]
        (mixed-state cls mix st above sides below-raw)))))

(defn mix-changes [chunks positions]
  (into []
        (comp (mapcat (fn [[x y z]] (cons [x y z] (map (fn [[dx dy dz]] [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)]) horiz3+))))
              (distinct)
              (keep (fn [p] (when-let [st (solidified chunks p)] [p st]))))
        positions))

(def ^:private column-drag {:soul-sand :false :magma :true})

(defn bubble-column? [st] (= :bubble-column (block/type-of (long st))))

(defn- column-state [below]
  (cond
    (bubble-column? below) below
    :else (when-let [drag (get column-drag (block/type-of (long below)))]
            (block/state :bubble-column {:drag drag}))))

(defn- water-source? [st] (= (long st) @water-source))

(defn- column-changes [chunks [x y z] col]
  (loop [y (long y) acc []]
    (let [st (long (raw-at chunks x y z))]
      (if (or (water-source? st) (and (bubble-column? st) (not= st (long col))))
        (recur (inc y) (conj acc [[x y z] col]))
        acc))))

(defn- bubble-changes [chunks [x y z :as p]]
  (let [raw (long (raw-at chunks x y z))
        col (column-state (long (raw-at chunks x (dec (long y)) z)))]
    (cond
      (and (bubble-column? raw) (nil? col)) [[p @water-source]]
      (and col (or (water-source? raw) (not= raw (long col)))) (column-changes chunks p col))))

(defn bubble-push ^double [chunks pos ^double vy]
  (let [x (long (Math/floor (v/x pos))) y (long (Math/floor (v/y pos))) z (long (Math/floor (v/z pos)))
        st (long (raw-at chunks x y z))]
    (if (bubble-column? st)
      (let [drag? (= :true (:drag (block/props-of st)))
            open? (zero? (long (raw-at chunks x (inc y) z)))]
        (cond
          (and drag? open?) (max -0.9 (- vy 0.03))
          drag? (max -0.3 (- vy 0.03))
          open? (min 1.8 (+ vy 0.1))
          :else (min 0.7 (+ vy 0.06))))
      vy)))

(def ^:private conversion-rule {:water :water-source-conversion :lava :lava-source-conversion})

(defn- flow-env [chunks cls rules]
  (let [{:keys [dropoff slope infinite? mix]} (liquids cls)]
    {:chunks    chunks :cls cls
     :dropoff   (long dropoff) :slope (long slope)
     :infinite? (get rules (conversion-rule cls) infinite?) :mix mix}))

(defn- cell-mixed [chunks mix cls st [x y z :as p]]
  (let [above (shifted chunks p [0 1 0])
        sides (side-states chunks p)
        below-raw (raw-at chunks x (dec (long y)) z)]
    (mixed-state cls mix st above sides below-raw)))

(defn- cell-flowed [chunks env cls p st]
  (let [v (if (source-of? cls st) :source (new-liquid env p))
        st' (if v (liquid->state cls v) 0)]
    (cond
      (zero? (long st')) [[p 0]]
      (and (= :source v) (seq (bubble-changes chunks p))) (bubble-changes chunks p)
      :else (into (if (not= (long st') (long st)) [[p st']] [])
                  (spread env p st')))))

(defn update-cell [chunks [x y z :as p] rules]
  (let [st (state-at chunks x y z)
        cls (liquid-class st)]
    (when cls
      (let [env (flow-env chunks cls rules)]
        (if-let [mixed (cell-mixed chunks (:mix env) cls st p)]
          [[p mixed]]
          (cell-flowed chunks env cls p st))))))

(defn- fire-state-at [chunks [x y z :as p]]
  (let [below (raw-at chunks x (dec (long y)) z)]
    (cond
      (block/tagged? (long (max 0 (long below))) "soul_fire_base_blocks")
      (block/state :soul-fire {:age :0})
      (or (block/burnable? (long (max 0 (long below)))) (block/face-sturdy? (long (max 0 (long below))) :up))
      (block/state :fire {:age :0})
      :else
      (block/state :fire (into {:age :0}
                               (map (fn [[k d]] [k (if (block/burnable? (long (max 0 (long (shifted chunks p d))))) :true :false)]))
                               {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0] :up [0 1 0]})))))

(defn- flammable-around? [chunks p]
  (some (fn [d] (block/ignited-by-lava? (long (max 0 (long (shifted chunks p d)))))) horiz3+))

(defn- lava-fire-walk [chunks p r3 passes]
  (loop [tp p i 0]
    (when (< i (long passes))
      (let [tp' [(+ (long (tp 0)) (long (r3 [:x i]))) (inc (long (tp 1))) (+ (long (tp 2)) (long (r3 [:z i])))]
            st (raw-at chunks (tp' 0) (tp' 1) (tp' 2))]
        (cond
          (neg? st) nil
          (zero? st) (if (flammable-around? chunks tp')
                       [[tp' (fire-state-at chunks tp')]]
                       (recur tp' (inc i)))
          (block/blocks-motion? st) nil
          :else (recur tp' (inc i)))))))

(defn- lava-spot-fires [chunks [x y z] r3]
  (into []
        (keep (fn [i]
                (let [tp [(+ (long x) (long (r3 [:x i]))) y (+ (long z) (long (r3 [:z i])))]
                      above [(tp 0) (inc (long y)) (tp 2)]]
                  (when (and (zero? (long (raw-at chunks (above 0) (above 1) (above 2))))
                             (block/ignited-by-lava?
                               (long (max 0 (long (raw-at chunks (tp 0) (tp 1) (tp 2)))))))
                    [above (fire-state-at chunks above)]))))
        (range 3)))

(defn lava-random-tick [chunks p roll]
  (let [r3 (fn [salt] (dec (long (Math/floor (* 3.0 (double (roll salt)))))))
        passes (long (Math/floor (* 3.0 (double (roll :passes)))))]
    (if (pos? passes)
      (lava-fire-walk chunks p r3 passes)
      (lava-spot-fires chunks p r3))))

(def rule
  {:name   :liquid
   :match? (fn [_chunks st _p] (some? (liquid-class st)))
   :wake   (fn [chunks tick p old self?]
             (if (mix-wake? chunks p)
               (inc (long tick))
               (+ (long tick)
                  (if self?
                    (update-delay old (chunk/chunks-get-block chunks p) tick p)
                    (delay-of (chunk/chunks-get-block chunks p))))))
   :due    (fn [chunks p ctx] (update-cell chunks p (:rules ctx)))})
