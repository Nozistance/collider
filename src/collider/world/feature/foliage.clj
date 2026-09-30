(ns collider.world.feature.foliage
  "The foliage placers of trees: the leaves around each attachment."
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.feature.level :as lv]
            [collider.world.feature.trunk :refer [off]]))

(set! *warn-on-reflection* true)

(defn- leaf-spot? [l p]
  (and (not= :true (:persistent (block/props-of (lv/at l p))))
       (lv/valid? l p)))

(defn- leaf
  "FoliagePlacer.tryPlaceLeaf."
  [l cfg p]
  (if (leaf-spot? l p)
    (let [st (lv/provide l (:foliage-provider cfg) p)
          wet? (contains? (block/props-of st) :waterlogged)]
      (lv/placed l :foliage p
                 (cond-> st wet? (lv/with-prop :waterlogged
                                   (lv/source? l p)))))
    l))

(defn- skip? [l fp dx y dz r dbl?]
  (let [corner? (and (= dx r) (= dz r))]
    (case (:type fp)
      (:blob-foliage-placer)
      (and corner? (or (zero? (lv/next-int l 2)) (zero? y)))
      :fancy-foliage-placer
      (let [sq #(let [f (float (+ (float %) 0.5))] (float (* f f)))]
        (> (float (+ (float (sq dx)) (float (sq dz)))) (float (* r r))))
      :spruce-foliage-placer (and corner? (pos? r))
      (:mega-pine-foliage-placer :jungle-foliage-placer)
      (or (>= (+ dx dz) 7) (> (+ (* dx dx) (* dz dz)) (* r r)))
      :acacia-foliage-placer
      (if (zero? y)
        (and (or (> dx 1) (> dz 1)) (not= dx 0) (not= dz 0))
        (and corner? (pos? r)))
      :dark-oak-foliage-placer
      (cond (and (= y -1) (not dbl?)) corner?
            (= y 1) (> (+ dx dz) (- (* r 2) 2))
            :else false)
      :cherry-foliage-placer
      (or (and (= y -1) (or (= dx r) (= dz r))
               (lv/chance? l (:wide-bottom-layer-hole-chance fp)))
          (if (> r 2)
            (or corner? (and (> (+ dx dz) (- (* r 2) 2))
                             (lv/chance? l (:corner-hole-chance fp))))
            (and corner? (lv/chance? l (:corner-hole-chance fp)))))
      false)))

(defn- min-abs ^long [^long d dbl?]
  (if dbl? (min (Math/abs d) (Math/abs (dec d))) (Math/abs d)))

(defn- skip-signed? [l fp dx y dz r dbl?]
  (if (and (= :dark-oak-foliage-placer (:type fp)) (zero? y) dbl?
           (or (= dx (- r)) (>= dx r)) (or (= dz (- r)) (>= dz r)))
    true
    (skip? l fp (min-abs dx dbl?) y (min-abs dz dbl?) r dbl?)))

(defn- row
  "FoliagePlacer.placeLeavesRow."
  [l cfg o r y dbl?]
  (let [fp (:foliage-placer cfg)
        e (if dbl? 1 0)]
    (reduce (fn [l [dx dz]]
              (if (skip-signed? l fp dx y dz r dbl?)
                l
                (leaf l cfg (off o dx y dz))))
            l (for [dx (range (- r) (+ r e 1))
                    dz (range (- r) (+ r e 1))]
                [dx dz]))))

(defn- manhattan ^long [a b]
  (reduce + (map #(Math/abs (- (long %1) (long %2))) a b)))

(defn- extension [l cfg chance log p]
  (if (or (>= (manhattan p log) 7)
          (> (lv/next-float l) (double (float chance))))
    [l false]
    [(leaf l cfg p) (leaf-spot? l p)]))

(defn- hang [l cfg fp log p]
  (if-not ((:leafy l) (lv/at-dir p :up))
    l
    (let [[l ok] (extension l cfg (:hanging-leaves-chance fp) log p)]
      (if ok
        (first (extension l cfg (:hanging-leaves-extension-chance fp)
                          log (lv/at-dir p :down)))
        l))))

(defn- positive? [d] (#{:east :south :up} d))

(defn- hanging-row [l cfg o r y dbl?]
  (let [fp (:foliage-placer cfg)
        e (if dbl? 1 0)
        l (row l cfg o r y dbl?)
        log (lv/at-dir o :down)
        edge (fn [l along]
               (let [to (dir/clockwise along)
                     n (if (positive? to) (+ r e) r)
                     start (-> (off o 0 (dec y) 0) (lv/toward to n)
                               (lv/toward along (- r)))]
                 (reduce #(hang %1 cfg fp log (lv/toward start along %2))
                         l (range (+ r r e)))))]
    (reduce edge l lv/horizontal)))

(defn- rows [l cfg o spec dbl?]
  (reduce (fn [l [r y]] (row l cfg o r y dbl?)) l spec))

(defn- blob [l cfg a h radius offset]
  (let [ro (long (:radius-offset a))]
    (rows l cfg (:pos a)
          (for [yo (range offset (dec (- offset h)) -1)]
            [(max (- (+ radius ro) 1 (quot yo 2)) 0) yo])
          (:double? a))))

(defn- fancy [l cfg a h radius offset]
  (rows l cfg (:pos a)
        (for [yo (range offset (dec (- offset h)) -1)]
          [(+ radius (if (and (not= yo offset) (not= yo (- offset h))) 1 0))
           yo])
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

(defn- mega-pine [l cfg a h radius offset]
  (let [[x y0 z] (:pos a)
        base (+ radius (long (:radius-offset a)))]
    (first
      (reduce (fn [[l prev] yy]
                (let [yo (- (long y0) (long yy))
                      part (float (/ (float yo) (float h)))
                      smooth (+ base (long (Math/floor
                                            (float (* part (float 3.5))))))
                      r (if (and (pos? yo) (= smooth prev) (even? yy))
                          (inc smooth) smooth)]
                  [(row l cfg [x yy z] r 0 (:double? a)) smooth]))
              [l 0]
              (range (+ (- (long y0) h) offset)
                     (inc (+ (long y0) offset)))))))

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
      (let [l (rows l cfg o [[(+ radius 2) -1] [(+ radius 3) 0]
                             [(+ radius 2) 1]] true)]
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
        d #(- (lv/next-int l %) (lv/next-int l %))]
    (reduce (fn [l _]
              (let [dx (d radius) dy (d h) dz (d radius)]
                (leaf l cfg (off o dx dy dz))))
            l (range (:leaf-placement-attempts (:foliage-placer cfg))))))

(defn- cherry [l cfg a h radius offset]
  (let [o (off (:pos a) 0 offset 0)
        dbl? (:double? a)
        r (dec (+ radius (long (:radius-offset a))))
        l (rows l cfg o (into [[(- r 2) (- h 3)] [(dec r) (- h 4)]]
                              (for [y (range (- h 5) -1 -1)] [r y]))
                dbl?)
        l (hanging-row l cfg o r -1 dbl?)]
    (hanging-row l cfg o (dec r) -2 dbl?)))

(defn place
  "FoliagePlacer.createFoliage at attachment a."
  [l cfg a h radius]
  (let [fp (:foliage-placer cfg)
        offset (lv/sample l (:offset fp))]
    ((case (:type fp)
       :blob-foliage-placer blob
       :fancy-foliage-placer fancy
       :spruce-foliage-placer spruce
       :mega-pine-foliage-placer mega-pine
       :acacia-foliage-placer acacia
       :dark-oak-foliage-placer dark-oak
       :jungle-foliage-placer jungle
       :random-spread-foliage-placer spread
       :cherry-foliage-placer cherry)
     l cfg a h radius offset)))

(defn height
  "FoliagePlacer.foliageHeight."
  ^long [l cfg ^long tree-height]
  (let [fp (:foliage-placer cfg)]
    (case (:type fp)
      (:blob-foliage-placer :fancy-foliage-placer :jungle-foliage-placer)
      (long (:height fp))
      :spruce-foliage-placer
      (max 4 (- tree-height (lv/sample l (:trunk-height fp))))
      :mega-pine-foliage-placer (lv/sample l (:crown-height fp))
      :acacia-foliage-placer 0
      :dark-oak-foliage-placer 4
      :random-spread-foliage-placer (lv/sample l (:foliage-height fp))
      :cherry-foliage-placer (lv/sample l (:height fp)))))

(defn radius
  "FoliagePlacer.foliageRadius."
  ^long [l cfg]
  (lv/sample l (:radius (:foliage-placer cfg))))
