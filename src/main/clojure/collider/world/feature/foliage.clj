(ns collider.world.feature.foliage
  "The foliage placers of trees, with the leaves around each place
  where foliage grows."
  (:require [collider.num :as num]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.feature.level :as lv :refer [off]]))

(set! *warn-on-reflection* true)

(defn- leaf-spot? [l p]
  (and (not= :true (:persistent (block/props-of (lv/at l p))))
       (lv/valid? l p)))

(defn- leaf [l cfg p]
  (if (leaf-spot? l p)
    (let [st (lv/provide l (:foliage-provider cfg) p)
          wet? (contains? (block/props-of st) :waterlogged)]
      (lv/placed l :foliage p
                 (cond-> st
                   wet? (block/with :waterlogged (lv/source? l p)))))
    l))

(defn- blob-skip? [l _fp dx y dz r _dbl?]
  (and (= dx r) (= dz r)
       (or (zero? (lv/next-int l 2)) (zero? y))))

(defn- square ^double [x]
  (let [f (float (+ (float x) 0.5))] (float (* f f))))

(defn- fancy-skip? [_l _fp dx _y dz r _dbl?]
  (> (float (+ (float (square dx)) (float (square dz))))
     (float (* r r))))

(defn- spruce-skip? [_l _fp dx _y dz r _dbl?]
  (and (= dx r) (= dz r) (pos? r)))

(defn- pine-skip? [_l _fp dx _y dz r _dbl?]
  (or (>= (+ dx dz) 7) (> (+ (* dx dx) (* dz dz)) (* r r))))

(defn- acacia-skip? [_l _fp dx y dz r _dbl?]
  (if (zero? y)
    (and (or (> dx 1) (> dz 1)) (not= dx 0) (not= dz 0))
    (and (= dx r) (= dz r) (pos? r))))

(defn- dark-oak-skip? [_l _fp dx y dz r dbl?]
  (cond (and (= y -1) (not dbl?)) (and (= dx r) (= dz r))
        (= y 1) (> (+ dx dz) (- (* r 2) 2))
        :else false))

(defn- cherry-corner? [l fp dx dz r]
  (let [corner? (and (= dx r) (= dz r))]
    (if (> r 2)
      (or corner? (and (> (+ dx dz) (- (* r 2) 2))
                       (lv/chance? l (:corner-hole-chance fp))))
      (and corner? (lv/chance? l (:corner-hole-chance fp))))))

(defn- cherry-skip? [l fp dx y dz r _dbl?]
  (or (and (= y -1) (or (= dx r) (= dz r))
           (lv/chance? l (:wide-bottom-layer-hole-chance fp)))
      (cherry-corner? l fp dx dz r)))

(def ^:private skips
  {:blob-foliage-placer blob-skip?
   :fancy-foliage-placer fancy-skip?
   :spruce-foliage-placer spruce-skip?
   :mega-pine-foliage-placer pine-skip?
   :jungle-foliage-placer pine-skip?
   :acacia-foliage-placer acacia-skip?
   :dark-oak-foliage-placer dark-oak-skip?
   :cherry-foliage-placer cherry-skip?})

(defn- skip? [l fp dx y dz r dbl?]
  (if-let [f (skips (:type fp))]
    (f l fp dx y dz r dbl?)
    false))

(defn- min-abs ^long [^long d dbl?]
  (if dbl? (min (Math/abs d) (Math/abs (dec d))) (Math/abs d)))

(defn- skip-signed? [l fp dx y dz r dbl?]
  (if (and (= :dark-oak-foliage-placer (:type fp)) (zero? y) dbl?
           (or (= dx (- r)) (>= dx r)) (or (= dz (- r)) (>= dz r)))
    true
    (skip? l fp (min-abs dx dbl?) y (min-abs dz dbl?) r dbl?)))

(defn- row [l cfg o r y dbl?]
  (let [fp (:foliage-placer cfg)
        e (if dbl? 1 0)]
    (reduce (fn [l [dx dz]]
              (if (skip-signed? l fp dx y dz r dbl?)
                l
                (leaf l cfg (off o dx y dz))))
            l (for [dx (range (- r) (+ r e 1))
                    dz (range (- r) (+ r e 1))]
                [dx dz]))))

(defn- extension [l cfg chance log p]
  (if (or (>= (lv/manhattan p log) 7)
          (> (lv/next-float l) (double (float chance))))
    [l false]
    [(leaf l cfg p) (leaf-spot? l p)]))

(defn- hang [l cfg fp log p]
  (if-not ((:leafy l) (lv/at-dir p :up))
    l
    (let [[l ok] (extension l cfg (:hanging-leaves-chance fp) log p)
          more (:hanging-leaves-extension-chance fp)]
      (if ok
        (first (extension l cfg more log (lv/at-dir p :down)))
        l))))

(defn- positive? [d] (#{:east :south :up} d))

(defn- edge [l cfg fp o r y e log along]
  (let [to (dir/clockwise along)
        n (if (positive? to) (+ r e) r)
        start (-> (off o 0 (dec y) 0) (lv/toward to n)
                  (lv/toward along (- r)))]
    (reduce #(hang %1 cfg fp log (lv/toward start along %2))
            l (range (+ r r e)))))

(defn- hanging-row [l cfg o r y dbl?]
  (let [fp (:foliage-placer cfg)
        e (if dbl? 1 0)
        l (row l cfg o r y dbl?)
        log (lv/at-dir o :down)]
    (reduce #(edge %1 cfg fp o r y e log %2) l lv/horizontal)))

(defn- rows [l cfg o spec dbl?]
  (reduce (fn [l [r y]] (row l cfg o r y dbl?)) l spec))

(defn- blob [l cfg a h radius offset]
  (let [ro (long (:radius-offset a))]
    (rows l cfg (:pos a)
          (for [yo (range offset (dec (- offset h)) -1)]
            [(max (- (+ radius ro) 1 (quot yo 2)) 0) yo])
          (:double? a))))

(defn- fancy-radius [radius offset h yo]
  (+ radius (if (and (not= yo offset) (not= yo (- offset h))) 1 0)))

(defn- fancy [l cfg a h radius offset]
  (rows l cfg (:pos a)
        (for [yo (range offset (dec (- offset h)) -1)]
          [(fancy-radius radius offset h yo) yo])
        (:double? a)))

(defn- spruce [l cfg a h radius offset]
  (let [cap (+ radius (long (:radius-offset a)))]
    (loop [l l yo offset r (lv/next-int l 2) hi 1 lo 0]
      (if (< yo (- h))
        l
        (let [l (row l cfg (:pos a) r yo (:double? a))]
          (if (>= r hi)
            (recur l (dec yo) lo (long (min (inc hi) cap)) 1)
            (recur l (dec yo) (inc r) hi lo)))))))

(defn- pine-row [cfg a h base [l prev] yy]
  (let [[x y0 z] (:pos a)
        yo (- (long y0) (long yy))
        part (float (/ (float yo) (float h)))
        smooth (+ base (num/floor (float (* part (float 3.5)))))
        r (if (and (pos? yo) (= smooth prev) (even? yy))
            (inc smooth)
            smooth)]
    [(row l cfg [x yy z] r 0 (:double? a)) smooth]))

(defn- mega-pine [l cfg a h radius offset]
  (let [y0 (long ((:pos a) 1))
        base (+ radius (long (:radius-offset a)))
        ys (range (+ (- y0 h) offset) (inc (+ y0 offset)))]
    (first (reduce #(pine-row cfg a h base %1 %2) [l 0] ys))))

(defn- acacia [l cfg a h radius offset]
  (let [o (off (:pos a) 0 offset 0)
        ro (long (:radius-offset a))]
    (rows l cfg o [[(+ radius ro) (- -1 h)] [(dec radius) (- h)]
                   [(dec (+ radius ro)) 0]]
          (:double? a))))

(defn- dark-oak [l cfg a _h radius offset]
  (let [o (off (:pos a) 0 offset 0)
        dbl? (:double? a)]
    (if dbl?
      (let [spec [[(+ radius 2) -1] [(+ radius 3) 0] [(+ radius 2) 1]]
            l (rows l cfg o spec true)]
        (if (lv/next-bool l) (row l cfg o radius 2 true) l))
      (rows l cfg o [[(+ radius 2) -1] [(inc radius) 0]] false))))

(defn- jungle [l cfg a h radius offset]
  (let [n (if (:double? a) h (inc (lv/next-int l 2)))
        ro (long (:radius-offset a))]
    (rows l cfg (:pos a)
          (for [yo (range offset (dec (- offset n)) -1)]
            [(- (+ radius ro 1) yo) yo])
          (:double? a))))

(defn- spread [l cfg a h radius _offset]
  (let [o (:pos a)
        d #(- (lv/next-int l %) (lv/next-int l %))
        n (:leaf-placement-attempts (:foliage-placer cfg))]
    (reduce (fn [l _]
              (let [dx (d radius) dy (d h) dz (d radius)]
                (leaf l cfg (off o dx dy dz))))
            l (range n))))

(defn- cherry [l cfg a h radius offset]
  (let [o (off (:pos a) 0 offset 0)
        dbl? (:double? a)
        r (dec (+ radius (long (:radius-offset a))))
        spec (into [[(- r 2) (- h 3)] [(dec r) (- h 4)]]
                   (for [y (range (- h 5) -1 -1)] [r y]))
        l (rows l cfg o spec dbl?)
        l (hanging-row l cfg o r -1 dbl?)]
    (hanging-row l cfg o (dec r) -2 dbl?)))

(def ^:private placers
  {:blob-foliage-placer blob
   :fancy-foliage-placer fancy
   :spruce-foliage-placer spruce
   :mega-pine-foliage-placer mega-pine
   :acacia-foliage-placer acacia
   :dark-oak-foliage-placer dark-oak
   :jungle-foliage-placer jungle
   :random-spread-foliage-placer spread
   :cherry-foliage-placer cherry})

(defn place
  "Returns l with the foliage that the foliage placer of cfg grows at
  the place a, with height h and radius."
  [l cfg a h radius]
  (let [fp (:foliage-placer cfg)
        offset (lv/sample l (:offset fp))]
    ((placers (:type fp)) l cfg a h radius offset)))

(defn- spruce-height ^long [l fp ^long tree-height]
  (max 4 (- tree-height (lv/sample l (:trunk-height fp)))))

(defn height
  "Returns the foliage height that the foliage placer of cfg draws
  for a tree of tree-height."
  ^long [l cfg ^long tree-height]
  (let [fp (:foliage-placer cfg)
        drawn #(lv/sample l (% fp))]
    (case (:type fp)
      (:blob-foliage-placer :fancy-foliage-placer
       :jungle-foliage-placer) (long (:height fp))
      :spruce-foliage-placer (spruce-height l fp tree-height)
      :mega-pine-foliage-placer (drawn :crown-height)
      :acacia-foliage-placer 0
      :dark-oak-foliage-placer 4
      :random-spread-foliage-placer (drawn :foliage-height)
      :cherry-foliage-placer (drawn :height))))

(defn radius
  "Returns the foliage radius that the foliage placer of cfg draws."
  ^long [l cfg]
  (lv/sample l (:radius (:foliage-placer cfg))))
