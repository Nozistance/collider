(ns collider.world.blocks.fall
  "Falling blocks, and what a block does with a fall onto it."
  (:require [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.dragonegg :as dragonegg]
            [collider.world.blocks.dripstone :as dripstone]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const legacy-offset (double (float 0.2)))

(defn- keeps-support? [^long st]
  (or (block/tagged? st "fences") (block/tagged? st "walls")
      (= :fence-gate (block/type-of st))))

(defn on-pos
  "Returns the cell of the block a body at pos lands on. sup is the
  block the body rests on, or nil."
  [chunks pos sup]
  (let [y (num/floor (- (v/y pos) legacy-offset))]
    (cond
      (nil? sup) [(num/floor (v/x pos)) y (num/floor (v/z pos))]
      (keeps-support? (chunk/at chunks sup)) sup
      :else [(nth sup 0) y (nth sup 2)])))

(defn- spike-landing [^long st ^double fall]
  (if (dripstone/stalagmite-tip? st)
    [(+ fall 2.5) 2.0 :stalagmite]
    [fall 1.0 :fall]))

(defn landing
  "Returns the fall distance, the damage factor and the damage kind
  of a body that falls fall onto st. Returns nil when the block
  takes the fall."
  [^long st ^double fall]
  (case (block/type-of st)
    (:hay :honey) [fall 0.2 :fall]
    :bed [(* fall 0.5) 1.0 :fall]
    :slime [fall 0.0 :fall]
    :powder-snow nil
    :pointed-dripstone (spike-landing st fall)
    [fall 1.0 :fall]))

(defn- span [^double f ^double d ^double lo ^double hi]
  (cond (pos? d) [(/ (- lo f) d) (/ (- hi f) d)]
        (neg? d) [(/ (- hi f) d) (/ (- lo f) d)]
        (and (<= lo f) (<= f hi)) [##-Inf ##Inf]
        :else [##Inf ##-Inf]))

(defn- inside? [f d b]
  (every? (fn [i]
            (let [p (+ (double (f i)) (* 0.001 (double (d i))))]
              (and (<= (double (b i)) p) (< p (double (b (+ i 3)))))))
          [0 1 2]))

(defn- meets?
  "Returns true when the ray from f along d starts in box b or
  enters it."
  [f d b]
  (let [ts (mapv #(span (f %) (d %) (b %) (b (+ % 3))) [0 1 2])
        t-in (double (reduce max (map first ts)))
        t-out (double (reduce min (map second ts)))]
    (or (inside? f d b)
        (and (<= t-in t-out) (<= 0.0 t-in) (< t-in 1.0)))))

(defn- cell-box [[x y z] ^double top]
  [(double x) (double y) (double z)
   (inc (double x)) top (inc (double z))])

(defn- resets-at?
  "Returns true when the ray from f along d meets a block of cell c
  that resets a fall, or the water in c."
  [chunks f d c]
  (let [st (chunk/at chunks c)
        top (when (block/water? st) (liquid/surface chunks :water c))]
    (or (and (block/tagged? st "fall_damage_resetting")
             (meets? f d (cell-box c (inc (double (c 1))))))
        (and top (meets? f d (cell-box c (double top)))))))

(defn- frac ^double [^double x] (- x (Math/floor x)))

(defn- axis-start [^double f ^double d]
  (let [s (long (Math/signum d))
        dt (if (zero? s) Double/MAX_VALUE (/ (double s) d))]
    [s dt (* dt (if (pos? s) (- 1.0 (frac f)) (frac f)))]))

(defn- next-axis ^long [[tx ty tz]]
  (let [tx (double tx) ty (double ty) tz (double tz)]
    (if (< tx ty) (if (< tx tz) 0 2) (if (< ty tz) 1 2))))

(defn- nudged [a b]
  (mapv (fn [x y]
          (+ (double x) (* -1.0E-7 (- (double y) (double x)))))
        a b))

(defn- walk-step [axes [c ts]]
  (let [i (next-axis ts) [s dt] (axes i)]
    [(update c i + s) (update ts i + dt)]))

(defn- going? [[_ ts]] (some #(<= (double %) 1.0) ts))

(defn- cells
  "Returns the cells a ray from f to t passes, in the order it
  passes them."
  [f t]
  (let [f' (nudged f t)
        t' (nudged t f)
        axes (mapv #(axis-start %1 (- (double %2) (double %1))) f' t')
        step #(walk-step axes %)
        start [(v/cell f') (mapv #(nth % 2) axes)]]
    (cons (start 0)
          (map #((step %) 0)
               (take-while going? (iterate step start))))))

(def ^:private ^:const reach 8.0)

(defn resets?
  "Returns true when a body that moves from pos to pos' passes a
  block or water that resets its fall. Only the first eight blocks
  of the move count."
  [chunks pos pos']
  (let [f [(v/x pos) (v/y pos) (v/z pos)]
        m (mapv - [(v/x pos') (v/y pos') (v/z pos')] f)
        len (Math/sqrt (reduce + (map * m m)))
        c (min len reach)
        d (mapv #(* (/ (double %) len) c) m)
        t (mapv + f d)]
    (boolean (some #(resets-at? chunks f d %) (cells f t)))))

(defn free-below?
  "Returns true when a falling block at the cell x y z would fall."
  [chunks [x y z]]
  (let [y' (dec (long y))]
    (and (chunk/in-range? y')
         (block/free? (chunk/chunks-get-block chunks [x y' z])))))

(defn- place-delay ^long [chunks p]
  (let [st (chunk/chunks-get-block chunks p)]
    (if (= :dragon-egg (block/type-of st))
      dragonegg/place-delay
      2)))

(def falling-rule
  "The block rule of blocks that fall when nothing is below them."
  {:name   :falling
   :match? (fn [_chunks st _p]
             (and (block/falls? st)
                  (not= :scaffolding (block/type-of st))))
   :wake   (fn [chunks _dim tick p _old _side]
             (+ (long tick) (place-delay chunks p)))
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks p)]
               (when (free-below? chunks p)
                 [[p (block/emptied st) [[:fall st]]]])))})
