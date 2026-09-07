(ns collider.world.liquid
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def liquids
  {:water {:block :water :dropoff 1 :slope 4 :delay 5  :bucket :water-bucket :infinite? true
           :push 0.014}
   :lava  {:block :lava :dropoff 2 :slope 2 :delay 30 :bucket :lava-bucket :infinite? false
           :push 0.0023333333333333335
           :decay-jitter 4
           :mix {:source :obsidian :flowing :cobblestone :smother :stone}}})

(def ^:private base
  (into {} (map (fn [[cls {:keys [block]}]] [cls (block/state block)])) liquids))

(def ^:private class-of-block
  (into {} (map (fn [[cls {:keys [block]}]] [block cls])) liquids))

(def ^:private bucket->class
  (into {} (map (fn [[cls {:keys [bucket]}]] [bucket cls])) liquids))

(def ^:private horiz [[1 0] [-1 0] [0 1] [0 -1]])

(def ^:private water-source (block/state :water))

(defn liquid-state? [st] (block/liquid? (long st)))
(defn liquid-class
  "Class of the liquid at the state: :water or :lava. A waterlogged state counts as :water."
  [st]
  (cond
    (liquid-state? st) (class-of-block (block/block-of (long st)))
    (block/waterlogged? (long st)) :water))
(defn level
  "Level of the liquid: 0 for a source, 1..7 flowing, 8 falling. Waterlogged states are sources."
  ^long [st]
  (if (liquid-state? st) (- (long st) (long (base (liquid-class st)))) 0))
(defn liquid-state ^long [cls ^long level] (+ (long (base cls)) level))
(defn bucket->state [item] (when-let [cls (bucket->class item)] (liquid-state cls 0)))
(defn delay-of [st] (long (get-in liquids [(liquid-class st) :delay])))

(defn source-state? [st]
  (and (liquid-state? st) (zero? (level st))))

(defn mix-class? [st]
  (some? (get-in liquids [(liquid-class st) :mix])))

(defn push-of [st]
  (get-in liquids [(liquid-class st) :push]))

(defn update-delay ^long [old new tick pos]
  (let [cls (liquid-class new)
        {:keys [delay decay-jitter]} (liquids cls)
        om  (if (liquid-state? old) (level old) 0)
        nm  (level new)]
    (if (and decay-jitter
             (= cls (liquid-class old))
             (< om 8) (< nm 8) (> nm om)
             (not= 0 (mod (hash [tick pos]) 4)))
      (* (long delay) (long decay-jitter))
      (long delay))))

(defn- raw-at [chunks template x y z]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks template x y z)
      -1)))

(defn- state-at [chunks template x y z]
  (let [st (long (raw-at chunks template x y z))]
    (if (and (pos? st) (block/waterlogged? st)) water-source st)))

(defn- shifted [chunks template [x y z] [dx dy dz]]
  (state-at chunks template
            (+ (long x) (long dx))
            (+ (long y) (long dy))
            (+ (long z) (long dz))))

(defn- air? [st] (zero? (long st)))
(defn- effective ^long [st] (let [m (level st)] (if (>= m 8) 0 m)))
(defn- other-class? [cls st]
  (let [c (liquid-class st)]
    (and (some? c) (not= c cls))))

(defn- blocks-movement? [st]
  (and (pos? (long st)) (block/blocks-motion? (long st))))

(defn- decay ^long [chunks template cls p]
  (let [st (state-at chunks template (p 0) (p 1) (p 2))]
    (if (and (pos? (long st)) (= cls (liquid-class st)))
      (let [m (level st)] (if (>= m 8) 0 m))
      -1)))

(defn- normalize [[x y z]]
  (let [x (double x) y (double y) z (double z)
        len (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< len 1.0E-4)
      [0.0 0.0 0.0]
      [(/ x len) (/ y len) (/ z len)])))

(defn- neighbor-pull [chunks template cls i [x y z] [dx dz]]
  (let [nx (+ (long x) (long dx))
        nz (+ (long z) (long dz))
        ns (state-at chunks template nx y nz)
        j  (decay chunks template cls [nx y nz])]
    (cond
      (other-class? cls ns)
      0
      (>= j 0)
      (- j (long i))
      (not (blocks-movement? ns))
      (let [j2 (decay chunks template cls [nx (dec (long y)) nz])]
        (if (>= j2 0)
          (- j2 (- (long i) 8))
          0))
      :else 0)))

(def ^:private side-face {[1 0] :east [-1 0] :west [0 1] :south [0 -1] :north})

(defn- solid-face?
  "Vanilla isSolidFace: not the same liquid, not ice, and the face of the
   neighbour in direction d is sturdy."
  [cls st d]
  (let [st (long st)]
    (and (pos? st)
         (not= cls (liquid-class st))
         (not (contains? #{:ice :packed-ice :blue-ice :frosted-ice} (block/block-of st)))
         (block/face-sturdy? st (side-face d)))))

(defn- walled? [chunks template cls [x y z]]
  (some (fn [[dx dz :as d]]
          (let [nx (+ (long x) (long dx))
                nz (+ (long z) (long dz))]
            (or (solid-face? cls (raw-at chunks template nx y nz) d)
                (solid-face? cls (raw-at chunks template nx (inc (long y)) nz) d))))
        horiz))

(defn flow-vector [chunks template [x y z :as p]]
  (let [st (state-at chunks template x y z)]
    (when-let [cls (when (pos? (long st)) (liquid-class st))]
      (let [i (decay chunks template cls p)
            [vx vz] (reduce (fn [[vx vz] [dx dz :as d]]
                              (let [k (neighbor-pull chunks template cls i p d)]
                                [(+ (double vx) (* (long dx) k))
                                 (+ (double vz) (* (long dz) k))]))
                            [0.0 0.0]
                            horiz)]
        (if (and (>= (level st) 8) (walled? chunks template cls p))
          (let [[nx _ nz] (normalize [vx 0.0 vz])]
            (normalize [nx -6.0 nz]))
          (normalize [vx 0.0 vz]))))))

(defn- own-height
  "Height of the liquid in its cell, vanilla getOwnHeight: amount / 9."
  ^double [st]
  (let [l (level st)] (/ (double (if (or (zero? l) (>= l 8)) 8 (- 8 l))) 9.0)))

(defn- height-in
  "Height of the liquid in the cell, 1.0 when the same liquid stands above."
  ^double [chunks template cls [x y z]]
  (let [above (state-at chunks template x (inc (long y)) z)]
    (if (and (pos? (long above)) (= cls (liquid-class above)))
      1.0
      (own-height (state-at chunks template x y z)))))

(defn- cells-of [x y z half height]
  (let [x (double x) y (double y) z (double z) half (double half) height (double height)]
    (for [cx (range (long (Math/floor (- x half))) (long (Math/ceil (+ x half))))
          cy (range (long (Math/floor y)) (long (Math/ceil (+ y height))))
          cz (range (long (Math/floor (- z half))) (long (Math/ceil (+ z half))))]
      [cx cy cz])))

(defn- fluid-around
  "{cls {:height h :flow [x y z] :n count}} for the liquids the box at pos
   touches, vanilla EntityFluidInteraction.update: the height is the top of
   the liquid over the bottom of the box, the flow is summed, scaled by the
   height while it is under 0.4."
  [chunks template [x y z] half height]
  (reduce (fn [acc [cx cy cz :as c]]
            (let [st (state-at chunks template cx cy cz)
                  cls (when (pos? (long st)) (liquid-class st))]
              (if (nil? cls)
                acc
                (let [top (+ (double cy) (height-in chunks template cls c))
                      h (- top (double y))]
                  (if (neg? h)
                    acc
                    (let [h (max h (double (get-in acc [cls :height] 0.0)))
                          [fx fy fz] (or (flow-vector chunks template c) [0.0 0.0 0.0])
                          k (if (< h 0.4) h 1.0)
                          [ax ay az] (get-in acc [cls :flow] [0.0 0.0 0.0])]
                      (assoc acc cls {:height h
                                      :flow [(+ (double ax) (* (double fx) k)) (+ (double ay) (* (double fy) k)) (+ (double az) (* (double fz) k))]
                                      :n (inc (long (get-in acc [cls :n] 0)))})))))))
          {}
          (cells-of x y z half height)))

(defn fluid-height
  "How deep the box at pos stands in the liquid class (0.0 when not in it)."
  [chunks template pos half height cls]
  (double (get-in (fluid-around chunks template pos half height) [cls :height] 0.0)))

(defn entity-push
  "Push of the currents on an entity with velocity vel, to add to it
   (vanilla applyCurrentTo for a non-player): the summed flow, normalized,
   times the push of the liquid; at least 0.0045 when the entity stands still."
  [chunks template pos half height vel]
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
          (fluid-around chunks template pos half height)))

;; --- spread: port of FlowingFluid / LavaFluid / WaterFluid 26.2 ------------
;;
;; Our level is the legacy one: 0 source, 1..7 flow, 8 falling. Vanilla counts
;; in amount (8 for source and falling, 7..1 for flow). We convert at the boundary.

(def ^:private horiz3 [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1]])
(def ^:private horiz3+ [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0] [0 -1 0]])
(def ^:private faces
  {[1 0 0] [:east :west] [-1 0 0] [:west :east] [0 0 1] [:south :north]
   [0 0 -1] [:north :south] [0 -1 0] [:down :up] [0 1 0] [:up :down]})
(def ^:private opposite {[1 0 0] [-1 0 0] [-1 0 0] [1 0 0] [0 0 1] [0 0 -1] [0 0 -1] [0 0 1]})
(def ^:private no-fluid-types #{:door :standing-sign :wall-sign :ladder :sugar-cane :bubble-column})

(defn- amount ^long [st] (let [l (level st)] (if (or (zero? l) (>= l 8)) 8 (- 8 l))))
(defn- falling? [st] (= 8 (level st)))
(defn- same? [cls st] (= cls (liquid-class st)))
(defn- source-of? [cls st] (and (same? cls st) (zero? (level st))))
(defn- height ^double [st] (/ (double (amount st)) 9.0))

(defn- boxes [st] (if (pos? (long st)) (block/collision-boxes (long st)) []))

(defn- face-covered?
  "Shapes.mergedFaceOccludes: the far layer of first (max = 16 on the axis)
   and the near layer of second (min = 0) cover the face. We count a 16x16
   grid in cells of 1/16."
  [first second ^long axis]
  (let [grid (boolean-array 256)
        [u v] (case axis 0 [1 2] 1 [0 2] [0 1])
        mark (fn [box]
               (let [u0 (long (Math/ceil (double (nth box u)))) u1 (long (Math/floor (double (nth box (+ u 3)))))
                     v0 (long (Math/ceil (double (nth box v)))) v1 (long (Math/floor (double (nth box (+ v 3)))))]
                 (doseq [a (range (max 0 u0) (min 16 u1)) b (range (max 0 v0) (min 16 v1))]
                   (aset grid (+ (* a 16) b) true))))]
    (doseq [box first :when (== 16.0 (double (nth box (+ axis 3))))] (mark box))
    (doseq [box second :when (== 0.0 (double (nth box axis)))] (mark box))
    (every? true? grid)))

(defn- pass-wall?
  "canPassThroughWall: fluid can flow from src to tgt in direction d. A full
   cube blocks. Two empty shapes pass. Otherwise the face must not be covered."
  [src tgt d]
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

(defn- holds-any-fluid?
  "canHoldAnyFluid: a container (waterlogged) or a block that does not block
   motion, except doors, signs, ladders, sugar cane and bubble columns."
  [st]
  (let [st (long st)]
    (cond
      (zero? st) true
      (container? st) true
      (blocks-movement? st) false
      :else (not (contains? no-fluid-types (block/type-of st))))))

(defn- holds-specific?
  "canHoldSpecificFluid: a dry container accepts only water."
  [cls st]
  (if (container? st)
    (and (= :water cls) (not (block/waterlogged? (long st))))
    true))

(defn- can-hold? [cls st] (and (holds-any-fluid? st) (holds-specific? cls st)))

(defn- replaceable-with?
  "canBeReplacedWith. Empty: any fluid. Water: only from above, not by water.
   Lava: only by water with height 4/9 or more."
  [tgt cls d]
  (case (liquid-class tgt)
    nil true
    :water (and (= d [0 -1 0]) (not= cls :water))
    :lava (and (= cls :water) (>= (height tgt) 0.44444445))))

(defn- can-maybe-pass? [cls src-raw tgt-raw tgt d]
  (and (not (source-of? cls tgt))
       (holds-any-fluid? tgt-raw)
       (pass-wall? src-raw tgt-raw d)))

(defn- cell [{:keys [chunks template]} [x y z]]
  [(raw-at chunks template x y z) (state-at chunks template x y z)])

(defn- hole?
  "isWaterHole: the cell below holds the same fluid or can take it."
  [{:keys [cls] :as env} [x y z :as p]]
  (let [[raw _] (cell env p)
        [braw b] (cell env [x (dec (long y)) z])]
    (and (pass-wall? raw braw [0 -1 0])
         (or (same? cls b) (can-hold? cls braw)))))

(defn- new-liquid
  "getNewLiquid for the cell p: :source, :falling, amount 1..7 or nil (empty)."
  [{:keys [cls dropoff infinite?] :as env} [x y z :as p]]
  (let [[raw _] (cell env p)
        [highest sources]
        (reduce (fn [[h s] [dx _ dz :as d]]
                  (let [[nraw n] (cell env [(+ (long x) dx) y (+ (long z) dz)])]
                    (if (and (same? cls n) (pass-wall? raw nraw d))
                      [(max (long h) (amount n)) (if (source-of? cls n) (inc (long s)) s)]
                      [h s])))
                [0 0] horiz3)
        [braw b] (cell env [x (dec (long y)) z])
        [araw a] (cell env [x (inc (long y)) z])]
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
  "getSlopeDistance: steps from p to a hole, not through from. 1000 if none
   within slope."
  ^long [{:keys [cls slope] :as env} [x y z :as p] ^long pass from]
  (reduce (fn [lowest [dx _ dz :as d]]
            (if (= d from)
              lowest
              (let [tp [(+ (long x) dx) y (+ (long z) dz)]
                    [raw _] (cell env p)
                    [traw t] (cell env tp)]
                (if (and (can-maybe-pass? cls raw traw t d) (holds-specific? cls traw))
                  (cond
                    (hole? env tp) (reduced pass)
                    (< pass (long slope)) (min (long lowest) (slope-distance env tp (inc pass) (opposite d)))
                    :else lowest)
                  lowest))))
          1000 horiz3))

(defn- mix-product [mix m]
  (when mix
    (block/state (if (zero? (long m)) (:source mix) (:flowing mix)))))

(def ^:private contact-dirs [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])
(def ^:private convert-dirs [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 -1 0]])
(defn- touches-other? [chunks template cls pos]
  (some (fn [d] (other-class? cls (shifted chunks template pos d))) contact-dirs))

(defn- convert-neighbors
  "Adjacent lava hardened by incoming water (shouldSpreadLiquid runs at once
   in neighborChanged)."
  [chunks template cls [x y z]]
  (into []
        (keep (fn [[dx dy dz]]
                (let [np [(+ (long x) (long dx))
                          (+ (long y) (long dy))
                          (+ (long z) (long dz))]
                      ns (state-at chunks template (np 0) (np 1) (np 2))
                      nc (liquid-class ns)]
                  (when (and (some? nc) (not= nc cls))
                    (when-let [prod (mix-product (get-in liquids [nc :mix]) (level ns))]
                      [np prod])))))
        convert-dirs))

(defn mix-wake? [chunks template pos]
  (let [st  (state-at chunks template (pos 0) (pos 1) (pos 2))
        cls (liquid-class st)]
    (boolean
     (and cls
          (get-in liquids [cls :mix])
          (touches-other? chunks template cls pos)))))

(defn- spread-to
  "spreadTo: what to write when fluid comes to tp. Lava on water gives stone.
   A container gets filled. Otherwise the new fluid state. Lava next to water
   hardens at once, water hardens adjacent lava."
  [{:keys [chunks template cls mix] :as env} tp d v]
  (let [[traw t] (cell env tp)]
    (cond
      (and mix (= d [0 -1 0]) (= :water (liquid-class t)))
      [[tp (block/state (:smother mix))]]
      (container? traw)
      [[tp (block/state (block/block-of (long traw)) (assoc (block/props-of (long traw)) :waterlogged :true))]]
      :else
      (let [plain (liquid->state cls v)
            st (if (and mix (touches-other? chunks template cls tp))
                 (or (mix-product mix (level plain)) plain)
                 plain)]
        (cons [tp st] (convert-neighbors chunks template cls tp))))))

(defn- spread-sides
  "spreadToSides + getSpread: replaceable neighbors with the nearest hole."
  [{:keys [cls dropoff] :as env} [x y z :as p] st]
  (let [n (if (falling? st) 7 (- (amount st) (long dropoff)))]
    (when (pos? n)
      (let [[raw _] (cell env p)
            cands (reduce (fn [[lowest acc] [dx _ dz :as d]]
                            (let [tp [(+ (long x) dx) y (+ (long z) dz)]
                                  [traw t] (cell env tp)]
                              (if-let [v (and (can-maybe-pass? cls raw traw t d)
                                              (new-liquid env tp))]
                                (if (holds-specific? cls traw)
                                  (let [dist (if (hole? env tp) 0 (slope-distance env tp 1 (opposite d)))
                                        acc (if (< dist (long lowest)) [] acc)]
                                    (if (<= dist (long lowest))
                                      [dist (if (replaceable-with? t cls d) (conj acc [tp d v]) acc)]
                                      [lowest acc]))
                                  [lowest acc])
                                [lowest acc])))
                          [1000 []] horiz3)]
        (into [] (mapcat (fn [[tp d v]] (spread-to env tp d v))) (second cands))))))

(defn- source-neighbours ^long [{:keys [cls] :as env} [x y z]]
  (count (filter (fn [[dx _ dz]] (source-of? cls (second (cell env [(+ (long x) dx) y (+ (long z) dz)])))) horiz3)))

(defn- spread
  "FlowingFluid.spread: down first (also sideways with three sources around).
   Otherwise sideways if the cell is a source or the cell below is not a hole."
  [{:keys [cls] :as env} [x y z :as p] st]
  (let [bp [x (dec (long y)) z]
        [raw _] (cell env p)
        [braw b] (cell env bp)]
    (or (when (can-maybe-pass? cls raw braw b [0 -1 0])
          (when-let [v (new-liquid env bp)]
            (when (and (replaceable-with? b cls [0 -1 0]) (holds-specific? cls braw))
              (into (vec (spread-to env bp [0 -1 0] v))
                    (when (>= (source-neighbours env p) 3) (spread-sides env p st))))))
        (when (or (source-of? cls st) (not (hole? env p)))
          (spread-sides env p st)))))

(defn update-delay
  "getSpreadDelay: tick delay after a change from old to new. A lava level
   increase is four times slower in 3 of 4 cases (hash of tick and pos)."
  ^long [old new tick pos]
  (let [cls (liquid-class new)
        {:keys [delay decay-jitter]} (liquids cls)]
    (if (and decay-jitter
             (same? cls old)
             (not (falling? old)) (not (falling? new))
             (> (height new) (height old))
             (not= 0 (mod (hash [tick pos]) 4)))
      (* (long delay) (long decay-jitter))
      (long delay))))

(defn- side-states [chunks template [x y z]]
  (mapv (fn [[dx dz]]
          (state-at chunks template (+ (long x) (long dx)) y (+ (long z) (long dz))))
        horiz))

(def ^:private basalt-state (delay (block/state :basalt)))
(def ^:private soul-soil-state (delay (block/state :soul-soil)))
(def ^:private blue-ice-state (delay (block/state :blue-ice)))

(defn- mixed-state
  "LiquidBlock.shouldSpreadLiquid: lava next to water gives obsidian (source)
   or cobblestone (flow). Lava on soul soil next to blue ice gives basalt."
  [cls mix st above sides below-raw]
  (when mix
    (cond
      (some (fn [s] (other-class? cls s)) (cons above sides)) (mix-product mix (level st))
      (and (= (long below-raw) (long @soul-soil-state))
           (some #(= (long %) (long @blue-ice-state)) (cons above sides))) @basalt-state
      :else nil)))

(defn- solidified [chunks template [x y z :as p]]
  (let [st (state-at chunks template x y z)
        cls (liquid-class st)]
    (when-let [mix (and cls (get-in liquids [cls :mix]))]
      (let [above (shifted chunks template p [0 1 0])
            sides (side-states chunks template p)
            below-raw (raw-at chunks template x (dec (long y)) z)]
        (mixed-state cls mix st above sides below-raw)))))

(defn mix-changes
  "Lava at or next to positions that touches water or blue ice hardens at
   once (neighborChanged). chunks already hold the written blocks."
  [chunks template positions]
  (into []
        (comp (mapcat (fn [[x y z]] (cons [x y z] (map (fn [[dx dy dz]] [(+ (long x) dx) (+ (long y) dy) (+ (long z) dz)]) horiz3+))))
              (distinct)
              (keep (fn [p] (when-let [st (solidified chunks template p)] [p st]))))
        positions))


(def ^:private column-drag {:soul-sand :false :magma :true})

(defn bubble-column? [st] (= :bubble-column (block/type-of (long st))))

(defn- column-state
  "Bubble column state that belongs above the raw state below, or nil."
  [below]
  (cond
    (bubble-column? below) below
    :else (when-let [drag (get column-drag (block/type-of (long below)))]
            (block/state :bubble-column {:drag drag}))))

(defn- water-source? [st] (= (long st) water-source))

(defn- column-changes [chunks template [x y z] col]
  (loop [y (long y) acc []]
    (let [st (long (raw-at chunks template x y z))]
      (if (or (water-source? st) (and (bubble-column? st) (not= st (long col))))
        (recur (inc y) (conj acc [[x y z] col]))
        acc))))

(defn- bubble-changes [chunks template [x y z :as p]]
  (let [raw (long (raw-at chunks template x y z))
        col (column-state (long (raw-at chunks template x (dec (long y)) z)))]
    (cond
      (and (bubble-column? raw) (nil? col)) [[p water-source]]
      (and col (or (water-source? raw) (not= raw (long col)))) (column-changes chunks template p col))))

(defn bubble-push
  "Vertical speed of an entity with its feet at pos after the bubble column there, as vanilla."
  ^double [chunks template pos ^double vy]
  (let [x (long (Math/floor (v/x pos))) y (long (Math/floor (v/y pos))) z (long (Math/floor (v/z pos)))
        st (long (raw-at chunks template x y z))]
    (if (bubble-column? st)
      (let [drag? (= :true (:drag (block/props-of st)))
            open? (zero? (long (raw-at chunks template x (inc y) z)))]
        (cond
          (and drag? open?) (max -0.9 (- vy 0.03))
          drag? (max -0.3 (- vy 0.03))
          open? (min 1.8 (+ vy 0.1))
          :else (min 0.7 (+ vy 0.06))))
      vy)))

(def ^:private conversion-rule {:water :water-source-conversion :lava :lava-source-conversion})

(defn update-cell
  "FlowingFluid.tick: harden next to the other fluid, update the level of a
   non-source, then spread. rules are the game rules (source conversion)."
  [chunks template [x y z :as p] rules]
  (let [st  (state-at chunks template x y z)
        cls (liquid-class st)]
    (when cls
      (let [{:keys [dropoff slope infinite? mix]} (liquids cls)
            env   {:chunks chunks :template template :cls cls
                   :dropoff (long dropoff) :slope (long slope)
                   :infinite? (get rules (conversion-rule cls) infinite?) :mix mix}
            above (shifted chunks template p [0 1 0])
            sides (side-states chunks template p)
            below-raw (raw-at chunks template x (dec (long y)) z)]
        (if-let [mixed (mixed-state cls mix st above sides below-raw)]
          [[p mixed]]
          (let [v   (if (source-of? cls st) :source (new-liquid env p))
                st' (if v (liquid->state cls v) 0)]
            (cond
              (zero? st') [[p 0]]
              (and (= :source v) (seq (bubble-changes chunks template p))) (bubble-changes chunks template p)
              :else (into (if (not= st' (long st)) [[p st']] [])
                          (spread env p st')))))))))

(defn- fire-state-at
  "BaseFireBlock.getState: soul fire above soul sand/soil, else fire of age 0.
   Without support below, set side flags towards burnable blocks."
  [chunks template [x y z :as p]]
  (let [below (raw-at chunks template x (dec (long y)) z)]
    (cond
      (contains? #{:soul-sand :soul-soil} (block/block-of (long (max 0 (long below)))))
      (block/state :soul-fire {:age :0})
      (or (block/burnable? (long (max 0 (long below)))) (block/face-sturdy? (long (max 0 (long below))) :up))
      (block/state :fire {:age :0})
      :else
      (block/state :fire (into {:age :0}
                               (map (fn [[k d]] [k (if (block/burnable? (long (max 0 (long (shifted chunks template p d))))) :true :false)]))
                               {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0] :up [0 1 0]})))))

(defn- flammable-around? [chunks template p]
  (some (fn [d] (block/ignited-by-lava? (long (max 0 (long (shifted chunks template p d)))))) horiz3+))

(defn lava-random-tick
  "LavaFluid.randomTick. 2 of 3 cases: up to two steps up and sideways through
   air, fire next to a burnable block. Else three tries at the same level:
   fire above a burnable block. rnd is (fn [salt] 0..1)."
  [chunks template [x y z :as p] rnd]
  (let [r3 (fn [salt] (dec (long (Math/floor (* 3.0 (double (rnd salt)))))))
        passes (long (Math/floor (* 3.0 (double (rnd :passes)))))]
    (if (pos? passes)
      (loop [tp p i 0]
        (when (< i passes)
          (let [tp' [(+ (long (tp 0)) (r3 [:x i])) (inc (long (tp 1))) (+ (long (tp 2)) (r3 [:z i]))]
                st (raw-at chunks template (tp' 0) (tp' 1) (tp' 2))]
            (cond
              (neg? st) nil
              (zero? st) (if (flammable-around? chunks template tp')
                           [[tp' (fire-state-at chunks template tp')]]
                           (recur tp' (inc i)))
              (block/blocks-motion? st) nil
              :else (recur tp' (inc i))))))
      (into []
            (keep (fn [i]
                    (let [tp [(+ (long x) (r3 [:x i])) y (+ (long z) (r3 [:z i]))]
                          above [(tp 0) (inc (long y)) (tp 2)]]
                      (when (and (zero? (long (raw-at chunks template (above 0) (above 1) (above 2))))
                                 (block/ignited-by-lava? (long (max 0 (long (raw-at chunks template (tp 0) (tp 1) (tp 2)))))))
                        [above (fire-state-at chunks template above)]))))
            (range 3)))))

(def rule
  {:name   :liquid
   :match? (fn [_chunks st _p] (some? (liquid-class st)))
   :wake   (fn [chunks tick p old self?]
             (if (mix-wake? chunks gen/flat-chunk p)
               (inc (long tick))
               (+ (long tick)
                  (if self?
                    (update-delay old (chunk/chunks-get-block chunks gen/flat-chunk p) tick p)
                    (delay-of (chunk/chunks-get-block chunks gen/flat-chunk p))))))
   :due    (fn [chunks p ctx] (update-cell chunks gen/flat-chunk p (:rules ctx)))})
