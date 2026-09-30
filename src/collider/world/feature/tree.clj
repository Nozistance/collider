(ns collider.world.feature.tree
  "TreeFeature: a tree of roots, trunk, foliage and decorations, the
  distances of its leaves and the shapes around it."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.feature.decorate :as decorate]
            [collider.world.feature.foliage :as foliage]
            [collider.world.feature.level :as lv]
            [collider.world.feature.trunk :as trunk])
  (:import (collider.world.feature Cells)))

(set! *warn-on-reflection* true)

(defn- cells ^Cells [ps] (Cells/of ps))

(defn- order [^Cells c] (Cells/.order c))

(defn- add! [^Cells c p] (Cells/.add c p))

(defn- poll! [^Cells c] (Cells/.poll c))

(defn- none? [^Cells c] (Cells/.isEmpty c))

(defn- size-at ^long [ms ^long h ^long y]
  (case (:type ms)
    :two-layers-feature-size
    (if (< y (long (:limit ms 1))) (:lower-size ms 0) (:upper-size ms 1))
    :three-layers-feature-size
    (cond (< y (long (:limit ms 1))) (:lower-size ms 0)
          (>= y (- h (long (:upper-limit ms 1)))) (:upper-size ms 1)
          :else (:middle-size ms 1))))

(defn- blocked? [l cfg p]
  (or (not (trunk/free? l (:trunk-placer cfg) p))
      (and (not (:ignore-vines cfg))
           (= :vine (block/block-of (lv/at l p))))))

(defn- free-height
  "TreeFeature.getMaxFreeTreeHeight."
  ^long [l cfg ^long h o]
  (or (first
        (for [y (range (+ h 2))
              :let [r (size-at (:minimum-size cfg) h y)]
              :when (some #(blocked? l cfg %)
                          (for [x (range (- r) (inc r))
                                z (range (- r) (inc r))]
                            (trunk/off o x y z)))]
          (- y 2)))
      h))

(defn- fits? [cfg ^long clipped ^long h]
  (let [m (:min-clipped-height (:minimum-size cfg))]
    (or (>= clipped h) (and m (>= clipped (long m))))))

(defn- grown [cfg fh radius [l atts]]
  (reduce #(foliage/place %1 cfg %2 fh radius) l atts))

(defn- body
  "TreeFeature.doPlace: lv with the tree, or nil when it does not fit."
  [l cfg o]
  (let [h (trunk/height l (:trunk-placer cfg))
        fh (foliage/height l cfg h)
        radius (foliage/radius l cfg)
        rp (:root-placer cfg)
        t (if rp (decorate/trunk-origin l rp o) o)
        lo (min (long (o 1)) (long (t 1)))
        hi (+ (max (long (o 1)) (long (t 1))) h 1)]
    (when (and (>= lo (inc chunk/min-y)) (<= hi (inc chunk/max-y)))
      (let [clipped (free-height l cfg h t)]
        (when (fits? cfg clipped h)
          (when-let [l (if rp (decorate/roots l rp o t) l)]
            (grown cfg fh radius (trunk/place l cfg clipped t))))))))

(defn- by-y [ps] (vec (sort-by #(% 1) (order (cells ps)))))

(defn- context [l]
  {:logs (by-y (:trunks l)) :leaves (by-y (:foliage l))
   :roots (by-y (:roots l))})

(defn- bounds [ps]
  (let [lo (reduce #(mapv min %1 %2) ps) hi (reduce #(mapv max %1 %2) ps)]
    [lo hi]))

(defn- inside? [[lo hi] p]
  (every? true? (map #(<= (long %1) (long %2) (long %3)) lo p hi)))

(defn- distance-of [st]
  (cond (block/tagged? st "prevents_nearby_leaf_decay") 0
        (contains? (block/props-of st) :distance)
        (block/prop-long st :distance)))

(defn- spread [l box ^objects checks shape p d]
  (reduce (fn [[l shape ^long d] side]
            (let [q (lv/at-dir p side)]
              (if-let [n (and (inside? box q) (not (shape q))
                              (distance-of (lv/at l q)))]
                (let [n (min (long n) (inc d))]
                  (if (< n 7)
                    (do (add! (aget checks n) q) [l shape (min d n)])
                    [l shape d]))
                [l shape d])))
          [l shape d] dir/six))

(defn- visit [l box checks shape p d]
  (if-not (inside? box p)
    [l shape d]
    (let [l (if (zero? d) l
                (lv/put l p (lv/with-prop (lv/at l p) :distance d)))]
      (spread l box checks (conj shape p) p d))))

(defn- leaves
  "TreeFeature.updateLeaves: lv with the leaf distances set, and the
  cells it filled."
  [l box]
  (let [checks (object-array (repeatedly 7 #(cells [])))
        shape (into #{} (filter #(inside? box %))
                    (concat (:decorations l) (:roots l)))]
    (doseq [p (order (cells (:trunks l)))] (add! (aget checks 0) p))
    (loop [l l shape shape d 0]
      (cond
        (>= d 7) [l shape]
        (none? (aget checks d)) (recur l shape (inc d))
        :else (let [p (poll! (aget checks d))
                    [l shape d] (visit l box checks shape p d)]
                (recur l shape (long d)))))))

(defn- line-faces [shape ps lo-side hi-side]
  (let [fs (mapv #(contains? shape %) ps)
        n (count ps)]
    (for [i (range (inc n))
          :let [full? (and (< i n) (fs i))
                last? (and (pos? i) (fs (dec i)))]
          :when (not= full? last?)]
      (if full?
        [:edge (ps i) lo-side]
        [:edge (ps (dec i)) hi-side]))))

(defn- faces
  "The faces of the cells of shape inside the box, in the order of
  DiscreteVoxelShape.forAllFaces, as edge updates."
  [[[x0 y0 z0] [x1 y1 z1]] shape]
  (let [xs (range x0 (inc (long x1))) ys (range y0 (inc (long y1)))
        zs (range z0 (inc (long z1)))
        line #(line-faces shape (mapv %1 %2) %3 %4)]
    (concat
      (for [x xs y ys f (line #(vector x y %) zs :north :south)] f)
      (for [z zs x xs f (line #(vector x % z) ys :down :up)] f)
      (for [y ys z zs f (line #(vector % y z) xs :west :east)] f))))

(defn- finished [l cfg]
  (let [l (decorate/decorate l (:decorators cfg) (context l))
        box (bounds (concat (:roots l) (:trunks l) (:foliage l)
                            (:decorations l)))
        [l shape] (leaves l box)]
    (update l :changes into (faces box shape))))

(defn place
  "TreeFeature.place of config cfg at origin o. Returns lv with the
  tree and :placed? true, or lv with :placed? false when it does
  not grow."
  [l cfg o]
  (let [l' (body l cfg o)]
    (if (and l' (or (seq (:trunks l')) (seq (:foliage l'))))
      (assoc (finished l' cfg) :placed? true)
      (assoc (or l' l) :placed? false))))
